setwd("C:/Users/ASUS/Desktop/Catedra_2024_1/Clase5-Validacion")

##Paquete para manejo facil de datos grillados
#install.packages("metR")
library(metR)
#Otroos paquetes que tal vez te parezcan interesantes para manejar datos grillados: "raster" y "ncdf4" 
##Paquete para graficado general
#install.packages("ggplot2")
library(ggplot2)
#Paquete para pruebas estadisticas
#install.packages("nortest")
library(nortest)
#Paquete para graficado estadistico
#install.packages("ggstatsplot")
library(ggstatsplot)
#install.packages("ggside")
library(ggside)


#¿Que datos tengo?
#Leyendo metadatos del modelo HadGEM2: datos históricos
GlanceNetCDF("SENAMHI_pp_d12k_HadGEM2-ES_hist_scal.nc")




#Accediendo al punto mas cercano de la estacion

#Jauja_tmin_m <- ReadNetCDF("SENAMHI_tmin_d12k_HadGEM2-ES_hist_scal.nc",subset=list(lat=-11.783,lon=-75.479))

Jauja_pp_m <- ReadNetCDF("SENAMHI_pp_d12k_HadGEM2-ES_hist_scal.nc",subset=list(lat=-11.783,lon=-75.479))
#Picoy_m <- ReadNetCDF("SENAMHI_tmin_d12k_HadGEM2-ES_hist_scal.nc",subset=list(lat=-11.783,lon=-75.479))
#Matucana_m <- ReadNetCDF("SENAMHI_tmin_d12k_HadGEM2-ES_hist_scal.nc",subset=list(lat=-11.783,lon=-75.479))
#Tarma_m <- ReadNetCDF("SENAMHI_tmin_d12k_HadGEM2-ES_hist_scal.nc",subset=list(lat=-11.783,lon=-75.479))
#Huayao_m <- ReadNetCDF("SENAMHI_tmin_d12k_HadGEM2-ES_hist_scal.nc",subset=list(lat=-11.783,lon=-75.479))
#Pilchaca_m <- ReadNetCDF("SENAMHI_tmin_d12k_HadGEM2-ES_hist_scal.nc",subset=list(lat=-11.783,lon=-75.479))

class(Jauja_pp_m)

##Convirtiendo a dataframe los datos del modelo
Jauja_pp_m <- data.frame(Jauja_pp_m)
head(Jauja_pp_m)

str(Jauja_pp_m$time)

summary(Jauja_pp_m)
##Convirtiendo a columna tipo fecha
Jauja_pp_m$time <- as.Date(Jauja_pp_m$time)

str(Jauja_pp_m$time)

#############Lectura de los datos homogeneizados
jauja <- read.csv("ho00000503.csv")
jauja
head(jauja)
#############Creando columna de fechas
jauja$Fecha <- paste0(jauja$year,"-",jauja$month,"-",jauja$day)

class(jauja$Fecha)

jauja$Fecha <- as.Date(jauja$Fecha)

class(jauja$Fecha)

#############Filtrando con las fechas del modelo
jauja <- jauja[jauja$Fecha >= min(Jauja_pp_m$time) & jauja$Fecha <= max(Jauja_pp_m$time),]

summary(jauja)


#######################Dataframe con los datos del modelo y observado
M_O_JAUJA <- data.frame(Fecha = jauja$Fecha,pp_m = Jauja_pp_m$pr, pp_o =jauja$Pp)


#######################Correlacion
#####1)Prueba de normalidad: Prueba de lilliefors
####



###Correlacion de spearman 



###Correlacion de pearon





####Aplicando la prueba a los datos del modelo
p1 <- lillie.test(M_O_JAUJA$pp_m)
p1$p.value

####Aplicando la prueba a los datos observados
p2 <- lillie.test(M_O_JAUJA$pp_o)
p2$p.value

##Hipótesis nula (Ho): Los datos provienen de una distribución normal .
##Hipótesis alternativa (H1): Los datos no provienen de una distribución normal

##Cuando el pvalor es menor que 0.05 
##Podemos rechazar la hipótesis nula.
##Conluimos que los datos no provienen de una distribución normal
###No aplicar la correlacion de pearson

##Cuando el pvalor es mayor que 0.05
##Podemos aceptar la hipótesis nula.
##Conluimos que los datos provienen de una distribución normal
#Aplicamos la correlacion de pearson

if (p1$p.value < 0.05 | p2$p.value < 0.05){
  cat("Los datos no provienen de una distribución normal")
  cat("Use una prueba no paramétrica")
}

###Para distribuciones normales usar "pearson"
###Para distribuciones no normales usar "spearman"  no parametrica

##Aplicando correlación de spearman
cor.test(M_O_JAUJA$pp_m,M_O_JAUJA$pp_o, method = "spearman",exact=F)


##pvlaue < 0.05 la prueba es estadisticamente significativa

#En este caso, un valor de 0.40 sugiere que hay una relación positiva moderada entre las variables. Esto significa que a medida que el valor de una variable aumenta, es probable que el valor de la otra variable también aumente, pero no necesariamente de forma lineal.
names(M_O_JAUJA)

#En type en este caso aplicar no-parametric 
#Si a ustedes les sale que son distribuciones normales usar "parametric"
graph <- ggscatterstats(data = M_O_JAUJA, x =pp_o,  y =pp_m, type = "no-parametric")+
  xlab("Pp Observado")+
  ylab("Pp modelo HadGEM2-ES")+
  ggtitle("Correlación Precipitación Observado vs ModeloHadGEM2  Estación Jauja")



ggsave(plot=graph,file="cor_jauja_pp.png", width = 10, height = 5)

##################Calculo del RMSE
#install.packages("Metrics")
library(Metrics)
##Asi es la sintaxis rmse(datos_reales,datos_predichos)
rmse(M_O_JAUJA$pp_o, M_O_JAUJA$pp_m)

##################Calculo del BIAS

##Asi es la sintaxis bias(datos_reales,datos_predichos)
Fgmutils::bias(M_O_JAUJA$pp_o, M_O_JAUJA$pp_m)

#########Calculo del MAE
mae(M_O_JAUJA$pp_o, M_O_JAUJA$pp_m)

