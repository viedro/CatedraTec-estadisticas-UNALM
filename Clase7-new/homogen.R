### Establecemos nuestro directorio de trabajo
path <- "C:/Users/ASUS/Desktop/catedra_2023_2/Clase4"
setwd(path)

### Comprobamos nuestro directorio
getwd()
list.files()
############################
########################## Homogeneizaci?n (climatol)

#install.packages("climatol")
library(climatol)

### 1) generar archivos de entrada
#install.packages("readxl")
library(readxl)

#Leemos los metadatos
metad <- read_excel("Estaciones_Meteorologicas_Peru.xlsx",col_names = F)
metad
View(metad)
class(metad)

#Nombres de columnas
colnames(metad) <- c("code","lat_grad","lat_min","lat_seg","lon_grad","lon_min","lon_seg","alt","nombre")
head(metad)

#Vector con nombres de archivos de estaciones
station <-  list.files(pattern = "ho.*\\.txt")

#Creamos otro vector con los nombres de los archivos pero en formato csv
station2 <- sub(".txt$", ".csv", station)

#Reescribiendo el archivo txt a csv
for (i in 1:length(station)){
  # Lee el archivo de texto
  datos <- read.table(station[i], header = F)  
  
  # Escribe los datos en un archivo CSV
  write.csv(datos, station2[i], row.names = F)
  
}

#Filtrando metadatos para nuestras estaciones
est <- NULL
for (i in 1:length(station)){
  est <- rbind(est,metad[metad$code == substr(station[i],1,10),])
}
est


#Funcion para convertir coordenadas sexagesimales a decimales
sexagesimal <- function(x, y, z, digits = 4){
  value <- as.numeric(x+(y+z/60)/60)
  return(as.numeric(format(round(value, digits), nsmall = digits)))
}


#Convertimos a grados decimales
lat <- sexagesimal(est$lat_grad,est$lat_min,est$lat_seg, digits = 4)
lat
lon <- sexagesimal(est$lon_grad,est$lon_min,est$lon_seg, digits = 4)
lon

# Nombre de archivos
Archivo <- station2

#Ordenar fichero al formato de climatol
Est <- data.frame(Archivo,lon,lat,alt=est$alt,code=est$code,nombre=est$nombre)

#Exportando el fichero
write.csv(Est, 'Est.csv', row.names=FALSE)


#Modificando los datos de estaciones al formato de climatol


#En datcol solo modificar el 4 por 5 o 6 si es que la variable es tmax o tmin
#Recordar que la columna 4 es precipitacion, la 5 tmin y la 6 tmax
#varcli="pre" modificar para tmin y tmax como varcli="tmin" o varcli="tmax"

daily2climatol(stfile = "Est.csv",stcol=1:6,datcol = c(1:3,4),varcli= "pre",anyi = 1980,anyf=1995,dec=".",header=T,na.strings = -99.9)

### AED
homogen("pre",anyi= 1980, anyf=1995,expl= T)

#3) Convertir datos diarios a datos mensuales
# Variabilidad no muy bien observada en series diarias
#Nota: valm 
#1: suma
#2: media
#Solo para precipitación usar valm =1, para temperatura valm =2

dd2m("pre",anyi= 1980, anyf=1995,valm= 1)

#AED series mensuales
homogen("pre-m",anyi= 1980, anyf=1995,expl=T)

#Homogenizando serie de tiempo mensual

###nota std=2 para precipitación, para temperatura escomún usar 3 ,std=3
### vmin= 0 porque la precipitación no puede ser negativa, para temperatura quitar vmin
homogen("pre-m",1980,1995,std=2,vmin = 0)

#Homogenizando serie de tiempo diaria
#metad: indica el uso de las series homogeneizadas mensuales en los datos diarios para hacer correcciones
homogen("pre",1980,1995,vmin = 0,metad=T)

#Cargando resultados
load("pre_1980-1995.rda")

#Generando serie de tiempo homogenizada
dahstat("pre",1980,1995,stat="series")

#Importando serie
homogen <- read.table("pre_1980-1995_series.csv",sep=",",header = T)
homogen

#pod: porcentaje de datos originales
#ios: numero de estacion
#ope: 0 : finaliza con un dato calculado
#     1 : 
#snht: prueba de homogeneidad
#rmse: error cuadratico medio

###Para evaluar:
# +%datos originales,menor RMSE, menor SNHT
est.c
View(est.c)

####Exportamos los datos de %datos originales, RMSE y SNHT
write.table(est.c,"Pre_sel.txt")



############Para manipular los datos homogeneizados
homogen

str(homogen)
homogen$Date <- as.Date(homogen$Date)

plot(homogen$Date,homogen$ho00000503)
colnames(homogen)

#install.packages("ggplot2")
library(ggplot2)
ggplot(homogen,aes(x=Date,y=ho00000541))+
  geom_point(col="blue")+
  ggtitle("Estación Jauja Serie completada y homogeneizada")



library(tidyverse)


smy <- homogen[,2:7] %>%
  summarise(across(where(is.numeric), .fns = 
                     list(min = ~min(., na.rm = TRUE),
                          median = ~median(., na.rm = TRUE),
                          mean = ~mean(., na.rm = TRUE),
                          stdev = ~sd(., na.rm = TRUE),
                          var = ~var(., na.rm = TRUE),
                          q25 = ~quantile(., 0.25, na.rm = TRUE),
                          q75 = ~quantile(., 0.75, na.rm = TRUE),
                          max = ~max(., na.rm = TRUE),
                          IQR = ~IQR(.,na.rm=T)))) %>% pivot_longer(everything(), names_sep='_', names_to=c('Variable', '.value'))
smy
class(smy)

write.table(smy,"res_homogen_sc.csv",row.names = F)


#######################Medias mensuales

sum_na <- function(x) {if (all(is.na(x)) == TRUE | sum(is.na(x)) >5) {NA} else {sum(x, na.rm = TRUE)}}

mean_na <- function(x) {if (all(is.na(x)) == TRUE | sum(is.na(x)) >5) {NA} else {mean(x, na.rm = TRUE)}}

library(tidyverse)
colnames(homogen)
homogen$Año <- format(homogen$Date,"%Y")
homogen$Mes <- format(homogen$Date,"%m")

head(homogen)

prom_m <- homogen %>% group_by(Año,Mes) %>%
  summarise(PP_jauja = sum_na(ho00000503))

Fecha <- seq.Date(as.Date("1980-01-01"),as.Date("1995-12-01"),by="month")
prom_m <- data.frame(prom_m,Fecha)
head(prom_m,15) 


ggplot(prom_m,aes(Fecha,PP_jauja))+
  geom_point(col="green")
