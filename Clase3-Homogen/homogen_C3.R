### Establecemos nuestro directorio de trabajo
path <- "C:/Users/ASUS/Desktop/Catedra 2024_1/Clase3-Homogen"
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
  value <- as.numeric(x-(y+(z/60))/60)
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

varcli <- "pre"
#varcli <- "tmax"
#varcli <- "tmin"

if (varcli == "pre"){
  ncol <- 4 
} else if (varcli == "tmax"){
  ncol <- 5
} else {
  ncol <- 6
}

inic <- 1980
fin <- 1995
  
daily2climatol(stfile = "Est.csv",stcol=1:6,datcol = c(1:3,ncol),varcli= varcli,anyi= ini, anyf=fin,dec=".",header=T,na.strings = -99.9)

#homogen("pre",anyi= 1980, anyf=1995,onlyQC = T)

### AED 
homogen(varcli,anyi= inic, anyf=fin,expl= T)
varcli
#3) Convertir datos diarios a datos mensuales
# Variabilidad no muy bien observada en series diarias
#Nota: valm 
#1: suma
#2: media
#Solo para precipitación usar valm =1, para temperatura valm =2

dd2m(varcli,anyi= inic, anyf=fin,valm= 1)
varcli
#AED series mensuales
homogen(paste0(varcli,"-m"),anyi= inic, anyf=fin,expl=T)
paste0(varcli,"-m")
#Homogenizando serie de tiempo mensual
###nota std=2 para precipitación, para temperatura escomún usar 3 ,std=3
### vmin= 0 porque la precipitación no puede ser negativa, para temperatura quitar vmin
homogen(paste0(varcli,"-m"),anyi= inic, anyf=fin,std=2,vmin = 0)
paste0(varcli,"-m")

#Homogenizando serie de tiempo diaria
#metad: indica el uso de las series homogeneizadas mensuales en los datos diarios para hacer correcciones
homogen(varcli,anyi= inic, anyf=fin,vmin = 0,metad=T)
varcli
#Cargando resultados
load(paste0(varcli,"_",ini,"-",fin,".rda"))   
paste0(varcli,"_",ini,"-",fin,".rda")

#pod: porcentaje de datos originales
#ios: numero de estacion
#ope: 0 : finaliza con un dato calculado
#     1 : 
#snht: prueba de homogeneidad
#rmse: error cuadratico medio

###Para evaluar:
# +%datos originales,menor RMSE, menor SNHT
est.c
#View(est.c)

####Exportamos los datos de %datos originales, RMSE y SNHT
write.csv(est.c,paste0(varcli,"_sel.csv"))  
paste0(varcli,"_sel.csv")

#Generando serie de tiempo homogenizada
dahstat(varcli,anyi= ini, anyf= fin,stat="series")
paste0(varcli,"_",ini,"-",fin,".series")

#Importando serie
homogen <- read.table(paste0(varcli,"_",ini,"-",fin,"_series.csv"),sep=",",header = T)
paste0(varcli,"_",ini,"-",fin,"_series.csv")
homogen

############Para manipular los datos homogeneizados
homogen

############Seleccionando la serie a utilizar
names(homogen)
Est$code

homogen_sel <- data.frame(Fecha=homogen$Date,ho00000503=homogen$ho00000503,ho00000541=homogen$ho00000541,
           ho00000542=homogen$ho00000542,ho00000548=homogen$ho00000548,
           ho00000554=NA,ho00000635=homogen$ho00000635,ho00000648=NA)

write.table(homogen_sel,file ="precip_diario_complete.csv",sep = ",",row.names=F)

###### Nuevas estadisticas
library(hydroTSM)
smy <- smry(homogen_sel)
smy

write.table(smy,"precip_complete_estadisticas.csv",row.names = T)


library(reshape2)
homogen_sel <- melt(homogen_sel, id.vars = c("Fecha"))
head(homogen_sel,15)
unique(homogen_sel$variable)
str(homogen_sel)
homogen_sel$variable<- factor(homogen_sel$variable,labels = Est$nombre)

homogen_sel$Fecha <- as.Date(homogen_sel$Fecha)

library(ggplot2)

# Crear multiples graficos en 1 solo
graph <- ggplot(data = homogen_sel, aes(x = Fecha, y = value, color = variable)) +
    geom_line() +
    labs(x = "Fecha", y = "") +
    scale_x_date(date_labels = "%Y", date_breaks = "3 year")+
    facet_wrap(~ variable, scales = "free_y", nrow = length(Est$nombre)) +  # Separar uno debajo del otro
    theme_bw() +
    theme(legend.position = "none")+
    theme(plot.title = element_text(hjust = 0.5,face = "bold"),  strip.text = element_text(color = "black", face = "bold"),strip.background = element_rect(fill="white",color=NA))+
    theme(axis.title = element_text(face = "bold"))+ggtitle("Serie temporal diaria completada")
graph  
  
ggsave(file="precip_complete.png", width = 15, height = 9)


unique(homogen_sel$variable)
## Solo algunas estaciones 

ggplot(subset(homogen_sel, variable == c("JAUJA","PICOY","MATUCANA")), aes(x = Fecha, y = value,color=variable)) +
  geom_line() +
  labs(x = "Fecha", y = "") +
  scale_x_date(date_labels = "%Y", date_breaks = "3 year")+
  facet_wrap(~ variable, scales = "free_y", nrow = 3) +  # Separar uno debajo del otro
  theme_bw() +
  theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 0.5,face = "bold"),  strip.text = element_text(color = "black", face = "bold"),strip.background = element_rect(fill="white",color=NA))+
  theme(axis.title = element_text(face = "bold"))+ggtitle("Serie temporal diaria completada")
+
  geom_line(data = subset(datos_melted, variable == c("Tmax", "Tmin")), linewidth = 0.8)

ggsave(file="precip_complete_jauja_picoy_matucana.png", width = 15, height = 9)


library(plotly)
ggplotly(graph)








