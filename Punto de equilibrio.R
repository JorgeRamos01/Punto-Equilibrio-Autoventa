rm(list=ls()) #Limpiamos la memoria
setwd("~/Documentos/Qualtia")
library(readr)
library(dplyr)
library(tidyverse)

margen_venta<-0.2   #Establecemos el margen de venta que se desea
#Cargamos el archivo de rutas por vehiculo
rutas_vehicle <- read_delim("rutas_vehicle.csv", "\t", escape_double = FALSE, trim_ws = TRUE)
names(rutas_vehicle)<-c("Rutas", "Vehicle")

#Cargamos los datos de actividad por vehiculo
activ_vehic <- read_csv("Informe de actividad por vehículo (3).csv")
activ_vehic$ActivityDate <- as.Date(activ_vehic$ActivityDate, "%d/%m/%Y") #Convertimos en formato de fecha

#####Limpiamos el campo de vehiculo de la tabla de actividad vehicular para tener solo las rutas y no las placas

temp<-strsplit(activ_vehic$Vehicle, " ")

temp2<-matrix(0L,ncol=1,nrow=length(temp))
for (i in 1:length(temp)){
  temp2[i]<-temp[[i]][1]
}

activ_vehic$Vehicle<-temp2

rm(temp,temp2)   #Removemos variables temporales de memoria

#Unimos las dos tablas con respecto al ID Vehicle con un left join correspondiendo a activ_vehic
activ_vehic<-merge(x = rutas_vehicle, y = activ_vehic, by = "Vehicle", all.x = TRUE) 

################################ Distancia de las rutas
temp2<-activ_vehic

temp2<-temp2 %>% drop_na(TripDistance)  #Nos quedamos con los valores que tienen una distancia en km

dist_ruta<-temp2 %>% select(Vehicle, Rutas, TripDistance) %>% filter(TripDistance<50) %>%   #Generamos la columna de distancia de la ruta
  group_by(Vehicle, Rutas) %>% summarise(distancia_ruta=sum(TripDistance))

dist_ruta<-dist_ruta %>% filter(distancia_ruta>10)

rm(temp2) #Limpiamos variables temporales de la memoria

########################### Calculando costos ##################################
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
temp<-read_csv("Movimientos de Facturacion SF-07 19.csv", 
               col_types = cols(`Fecha Transacción` = col_datetime(format = "%d/%m/%Y"), 
                                Rendimiento = col_number()))
temp2<-read_csv("Movimientos de Facturacion SF-08 19.csv", 
                col_types = cols(`Fecha Transacción` = col_datetime(format = "%d/%m/%Y"), 
                                 Rendimiento = col_number()))

names(temp2)[2]<-"Tipo" 

mov_fact<-rbind(temp,temp2)

rm(temp,temp2)   #Removemos variables temporales de memoria
mov_fact$Rendimiento<-mov_fact$Rendimiento/100

mov_fact$`Precio Unitario`<-gsub("\\$", "", mov_fact$`Precio Unitario`) #Damos formato al precio
mov_fact$`Precio Unitario`<-as.numeric(gsub(",", ".", mov_fact$`Precio Unitario`))

#Calculamos el precio de combustible promedio, por tipo de combustible por litro
costo_combustible <- mov_fact %>% select(`Identificador Vehículo`, Tipo, Region, `Fecha Transacción`, Mercancía, `Precio Unitario`, Rendimiento) %>% 
  filter(`Fecha Transacción`>= "2019-02-11" & `Fecha Transacción`<="2019-02-16") %>% 
  group_by(Mercancía) %>% summarize(precio_prom_comb=mean(`Precio Unitario`, na.rm = TRUE), rend_prom=getmode(Rendimiento))

temp<-mov_fact %>% select("Identificador Vehículo", Mercancía)
temp2<-merge(x=temp, y=costo_combustible, by="Mercancía", all.x=TRUE)

combustible<-temp2 %>% distinct()    #Tabla de costos de combustible y rendimientos
combustible<- combustible[!duplicated(combustible[,2]),]

rm(temp,temp2, costo_combustible, mov_fact)  #Removemos variables temporales de memoria

#####Informacion de venta y ruta de ventas (Variables de control)

gasto_rutas<-read_delim("Información venta y gasto de rutas.csv", 
                        "\t", escape_double = FALSE, trim_ws = TRUE)


prom_rutas<-gasto_rutas[,1:13]
prom_rutas$Sueldos <-apply(gasto_rutas[,14:19],1,mean)
prom_rutas$Mantenimiento <-apply(gasto_rutas[,26:31],1,mean)
prom_rutas$Depreciacion <-apply(gasto_rutas[,32:37],1,mean)
prom_rutas$Otros <-apply(gasto_rutas[,38:43],1,mean)

prom_rutas <- prom_rutas %>% filter(Sueldos>5000 & Mantenimiento>100)

rm(gasto_rutas) #Removemos variables temporales de memoria

names(combustible)[2]<-"Vehicle"

temp3<-merge(x = rutas_vehicle, y = combustible, by = "Vehicle", all.x = TRUE)
temp3<- na.omit(temp3)

temp<-strsplit(prom_rutas$Ruta, ":")

temp2<-matrix(0L,ncol=1,nrow=length(temp))
for (i in 1:length(temp)){
  temp2[i]<-temp[[i]][1]
}

names(prom_rutas)[1]<-"Rutas"
prom_rutas$Rutas<-temp2

rm(temp, temp2) #borramos las variables temporales de la memoria

temp<-merge(x = temp3, y = prom_rutas, by = "Rutas", all.x = TRUE)

tabla_final<- na.omit(temp)  #Quitamos valores faltantes

rm(temp, temp3, i) #borramos las variables temporales de la memoria

tabla_final<-merge(x = tabla_final, y = dist_ruta, by = "Vehicle", all.x = TRUE)

tabla_final<-na.omit(tabla_final) #Quitamos valores faltantes

#Generamos el indice de uso de combustible
tabla_final$Comb_Ruta<-round((tabla_final$distancia_ruta/tabla_final$rend_prom)*tabla_final$precio_prom_comb, digits=1)

tabla_final$Punto_Equil<-(((tabla_final$Sueldos+tabla_final$Mantenimiento+tabla_final$Depreciacion+tabla_final$Otros)/4.33)+tabla_final$Comb_Ruta)/margen_venta

#######Deteccion de Outliers

#Cargamos el catalogo de tipo de vendedor
Descripciones_tipo_vendedor <- read_delim("Descripciones tipo vendedor.csv", 
                                          "\t", escape_double = FALSE, trim_ws = TRUE)

tabla_final$Rutas.y<-NULL
names(Descripciones_tipo_vendedor)<-c("Rutas.x", "Esquema")

tabla_final<-merge(x = tabla_final, y = Descripciones_tipo_vendedor, by = "Rutas.x", all.x = TRUE)

#con respecto a uso de combustible
hist(tabla_final$Comb_Ruta,  col=rgb(0.2,0.8,0.5,0.5) , border=F, main="Histograma: Combustible-Ruta", ylab="Frecuencia", xlab="Combustible Ruta")
a<-boxplot(tabla_final$Comb_Ruta ~ tabla_final$Esquema, main="BoxPlot: Combustible-Ruta",col=rgb(0.2,0.8,0.5,0.5), ylab="Combustible-Ruta")

Outliers_combustible<- tabla_final %>% select(Vehicle, Comb_Ruta) %>% filter(Comb_Ruta %in% a$out)
