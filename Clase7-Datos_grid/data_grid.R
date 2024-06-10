################################ Correlaciones 

setwd("C:/Users/ASUS/Desktop/Catedra_2024_1/Clase7-Datos_grid")
getwd()
#install.packages("metR")
library(metR)     #### Manejo de datos netcdf en formato tabla (mucha facilidad de trabajo)
library(terra)    #### Manejo de todo tipo de datos (netcdf,tif, shape,hdf, hdf5, etc...)

####Para ver que variables y dimensiones tengo
metR::GlanceNetCDF("sst.mnmean.nc")

##############################Manejo de datos grillados con terra

#Abrir archivo netcdf como un cubo raster multicapa
tsm_day <- terra::rast("sst.mnmean.nc",subds ="sst" )
tsm_day

###Indexar alguna capa
tsm_day[[1]]

###Ploteo basico

#library(rasterVis)
#levelplot(tsm_day[[1]],margin=F)
terra::plot(tsm_day[[1]])

terra::plot(tsm_day[[1]],main="TSM Enero 1854 ERSSTV5")
terra::contour(tsm_day[[1]],add=T)

#install.packages("paletter")
library(paletteer)
#paletteer_c("ggthemes::Red-Blue Diverging", 30)

terra::plot(tsm_day[[1]],main="TSM Enero 1854 ERSSTV5",col=rev(paletteer_c("ggthemes::Red-Blue-White Diverging", 30)))
terra::contour(tsm_day[[1]],add=T)

#install.packages("rasterVis")
library(rasterVis)

####Graficado basico
levelplot(tsm_day[[1]],margin=F)


levelplot(tsm_day[[1]],margin=F,col.regions=rev(paletteer_c("ggthemes::Red-Blue-White Diverging", 30)),xlab="Longitud",ylab="Latitud",main="TSM Enero 1854 ERSSTV5",maxpixels=1e20,pretty=T)

###Escala de colores
levelplot(tsm_day[[1]],margin=F,col.regions=rev(paletteer_c("ggthemes::Red-Blue-White Diverging", 30)),xlab="Longitud",ylab="Latitud",main="CTSM Enero 1854 ERSSTV5",maxpixels=1e20,pretty=T,at=c(-Inf,seq(0,30,2),Inf))

###Mapa del mundo
#install.packages("maps")
library(maps)
#install.packages("sp")
library(sp)
#install.packages("maptools")
library(maptools)

##Obtención de mapa
countries=maps::map('world2',plot=F)
countries=map2SpatialLines(countries)

mycolorkey <- list(labels=list(tri.lower = TRUE, tri.upper = TRUE,labels=c(seq(0,30,2)),
                               at=seq(0,30,2)))

levelplot(tsm_day[[1]],margin=F,col.regions=rev(paletteer_c("ggthemes::Red-Blue-White Diverging", 30)),xlab="Longitud",ylab="Latitud",main="TSM Enero 1854 ERSSTV5",maxpixels=1e20,pretty=T,at=c(-Inf,seq(0,30,2),Inf),colorkey=mycolorkey)+
  latticeExtra::layer(sp.lines(countries))+ contourplot(tsm_day[[1]],at = seq(0,30,2),lwd = 1,labels = list(cex = 0.8),maxpixels=1e50)


#####Calculo dde climatología mensual

#### Filtrar periodo base
rango <- terra::time(tsm_day)

which(rango >= as.Date("2023-01-01"))
terra::plot(tsm_day[[which(rango >= as.Date("2023-01-01"))]])


tsm_91_20 <- tsm_day[[which(terra::time(tsm_day) >= as.Date("1991-01-01") &  terra::time(tsm_day) <= as.Date("2020-12-31"))]]

####Calculo de climatologia 
tsm_clim <- terra::tapp(tsm_91_20, "months", fun=mean)
tsm_clim

names(tsm_clim)<- c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")


levelplot(tsm_clim,margin=F,col.regions=rev(paletteer_c("ggthemes::Red-Blue-White Diverging", 30)),xlab="Longitud",ylab="Latitud",main="Climatología Temperatura Superficial del Mar ERSSTV5",maxpixels=1e20,pretty=T,at=c(-Inf,seq(0,30,2),Inf),colorkey=mycolorkey)+
  latticeExtra::layer(sp.lines(countries))


###Convirtiendo a dataframe
dic <- terra::as.data.frame(tsm_clim[[12]],xy=T)
head(dic)

library(ggplot2)
library(plotly)
#library(data.table)


world <- list(geom_polygon(data=map_data("world2"),aes(long,lat,group=group),linewidth=0.2,color="black",fill="white"),coord_quickmap(),metR::scale_x_longitude(),metR::scale_y_latitude())

names(dic)

##Metodo ggplot: por pixeles
g <- ggplot(data=dic,aes(x=x,y=y))+
  geom_tile(aes(fill=Diciembre))+
  ggtitle("Climatología Temperatura Superfical del Mar \n Diciembre ERSSTV5")+
  xlab("Longitud")+
  ylab("Latitud")+
  theme(plot.title = element_text(hjust = 0.5,face = "bold"),legend.key.height = unit(2,"cm"))+
  scale_fill_gradientn(colours=rev(paletteer_c("ggthemes::Red-Blue-White Diverging", 30)))+world+labs(fill = "°C")

ggplotly(g)  


##Metodo ggplot: por contornos
ggplot(data=dic,aes(x=x,y=y))+
  metR::geom_contour_fill(aes(z=Diciembre),breaks=seq(0,30,2) )+
  ggtitle("Climatología Temperatura Superfical del Mar \n Diciembre ERSSTV5")+
  metR::geom_contour2(aes(z=Diciembre),breaks=seq(0,30,2))+
  metR::geom_text_contour(aes(z=Diciembre),breaks=c(seq(0,30,2)),stroke=0.2)+
  xlab("Longitud")+
  ylab("Latitud")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,face = "bold"),legend.key.height = unit(2,"cm"))+
  scale_fill_gradientn(colours=rev(paletteer_c("ggthemes::Red-Blue-White Diverging", 30)),breaks=seq(0,30,2),limits = c(0, 30))+world+
  guides(fill=guide_colorsteps(title="°C"))


#terra y coonvirtiendo a dataframe

#########################################################################
#########################################################################
#########################################################################
############### Método metR: netcdf a dataframe

metR::GlanceNetCDF("sst.mnmean.nc")

tsm <- metR::ReadNetCDF("sst.mnmean.nc",var="sst",subset = list(time =c("1981-01-01", "2010-12-01")))

class(tsm)
###Filtrando Diciembre
unique(tsm$time)

tsm <- data.frame(tsm)
class(tsm)

tsm$time <- as.Date(tsm$time)
names(tsm)
##### Calculo de datos mensuales
library(dplyr)
library(lubridate)

View(clim_tsm)
######Calculo de climatologia
clim_tsm <- tsm %>% group_by(lat,lon,month(time,label = T, abbr = F)) %>% 
  summarise(sst = mean(sst))

names(clim_tsm)
names(clim_tsm) <- c("Lat","Lon","Mes","sst")


####### Mapa multiple climatologia mensual

ggplot(data=clim_tsm,aes(x=Lon,y=Lat))+
  metR::geom_contour_fill(aes(z=sst),breaks=seq(0,30,2) )+
  ggtitle("Climatología Temperatura Superfical del Mar \n Diciembre ERSSTV5")+
  xlab("Longitud")+
  ylab("Latitud")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,face = "bold"),legend.key.height = unit(2,"cm"))+
  scale_fill_gradientn(colours=rev(paletteer_c("ggthemes::Red-Blue-White Diverging", 30)),breaks=seq(0,30,2),limits = c(0,30))+world+
  guides(fill=guide_colorsteps(title="°C"))+facet_wrap(~Mes, ncol = 3)

ggsave("clim_tsm.png",width = 18, height = 10,dpi=500)



######################Correlaciones espaciales
######Continente a océano

######Calculo de medias mensuales por año 


###########Estación jauja
Est <- read.csv("ho00000503.csv")

##Diario a mensual
Est_men <- Est %>% group_by(year,month) %>% 
  summarise(Pp = sum(Pp,na.rm=T),
            Tmax= mean(Tmax,na.rm=T),
            Tmin= mean(Tmin,na.rm=T))

###Filtrando 1981-2010
Est_men$Fecha <- as.Date(paste0(Est_men$year,"-",Est_men$month,"-01"))

Est_men <- Est_men[Est_men$Fecha>=as.Date("1981-01-01") & Est_men$Fecha<=as.Date("2010-12-31"),]

tail(Est_men)
names(Est_men)
names(tsm)

names(tsm) <- c("Fecha", "lat",  "lon"  ,"sst" )

###Uniendo dataframe grillado y datos puntuales

library(dplyr)

table_f<- tsm %>% 
  inner_join(Est_men, by = c("Fecha")) 


######Correlación total
stat_table <- table_f %>% group_by(lon,lat) %>% 
  summarise(R = cor(sst,Tmax,method = "spearman",
                    use = "pairwise.complete.obs"))

names(stat_table) <- c("Lon","Lat","R")

ggplot(data=stat_table,aes(x=Lon,y=Lat))+
  geom_raster(aes(fill=R),breaks=seq(-1,1,0.1) )+
  ggtitle("Correlación de Spearman Tmax Jauja Observado vs  TSM ERSSTV5")+
  xlab("Longitud")+
  ylab("Latitud")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,face = "bold"),legend.key.height = unit(2,"cm"))+
  scale_fill_gradientn(colours=rev(paletteer_c("ggthemes::Classic Red-White-Black", 30)),breaks=seq(-1,1,0.1),limits = c(-1,1))+world+
  guides(fill=guide_colorsteps(title="R"))

ggsave("corr_tsm_tmax_jauja.png",width = 18, height = 10,dpi=500)

#######Correlación por mes
stat_table_m <- table_f %>% group_by(lon,lat,month(Fecha,label = T, abbr = F)) %>% 
  summarise(R = cor(sst,Tmax,method = "spearman",
                    use = "pairwise.complete.obs"))

names(stat_table_m) <- c("Lon","Lat","Mes","R")
tail(tsm)

ggplot(data=stat_table_m,aes(x=Lon,y=Lat))+
  metR::geom_contour_fill(aes(z=R),breaks=seq(-1,1,0.1) )+
  ggtitle("Correlación de Spearman Pp Observado por Mes vs  TSM ERSSTV5")+
  xlab("Longitud")+
  ylab("Latitud")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,face = "bold"),legend.key.height = unit(2.5,"cm"))+
  scale_fill_gradientn(colours=rev(paletteer_c("ggthemes::Classic Red-White-Black", 30)),breaks=seq(-1,1,0.1),limits = c(-1,1))+world+
  guides(fill=guide_colorsteps(title="R"))+facet_wrap(~Mes, ncol = 3)

ggsave("clim_tsm.png",width = 18, height = 10,dpi=500)


#######Adicional
########EOF  (Empirical Orthogonal Function) 

tsm <- metR::ReadNetCDF("sst.mnmean.nc",var="sst",subset = list(time =c("1981-01-01", "2010-12-01")))

library(data.table)
class(tsm)
tsm2 <- setDT(tsm)

###Los eof no pueden tener NA 
###Los eof es conveniente trabajr con anomalias 

tsm2 <- na.omit(tsm2) %>% group_by(lat,lon,month(time)) %>% mutate(sst = (sst - mean(sst))) 

tsm2 <- setDT(tsm2)

#### 
tsm2[, sst := sst * sqrt(cos(lat * pi/180))]

eof <- metR::EOF(sst ~ lon + lat | time, data = tsm2, n = 1:4)
eof


eof$left
#min(eof$left$sst[eof$left$PC == "PC1"])
#min(eof$left$sst[eof$left$PC == "PC2"])

ggplot(data=eof$left[eof$left$PC == "PC1",],aes(x=lon,y=lat))+
  geom_raster(aes(fill=sst))+
  ggtitle("EOF 1")+
  xlab("Longitud")+
  ylab("Latitud")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,face = "bold"),legend.key.height = unit(2,"cm"))+
  metR::scale_fill_divergent(breaks=seq(-0.05,0.02,0.01))+world+
  guides(fill=guide_colorsteps(title=""))

eof$right$time <- as.Date(eof$right$time)

ggplot(data=eof$right[eof$right$PC == "PC1",],aes(x=time,y=sst))+
  geom_line(col="dodgerblue2",size=0.6)+
  ggtitle("Serie de tiempo PC 1")+
  xlab("Longitud")+
  ylab("Latitud")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,face = "bold"),legend.key.height = unit(2,"cm"))+
  geom_hline(yintercept=0)+facet_wrap(~PC, ncol = 2)+
  scale_x_date(date_breaks = "3 years", date_labels = "%Y")




