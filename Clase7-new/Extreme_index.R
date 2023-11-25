##Directorio de trabajo
setwd("C:/Users/ASUS/Desktop/catedra_2023_2/Clase7")
getwd()
######Creación de ficheros de entrada

##Lectura de series de climatol
pre <- read.table("pre_1965-2019_series.csv",sep=",",header=T)
tmax <- read.table("tmax_1965-2019_series.csv",sep=",",header=T)
tmin <- read.table("tmin_1965-2019_series.csv",sep=",",header=T)
#View(tmax)
##Extraccion de año, mes y dia

year <- as.numeric(format(as.Date(pre$Date),'%Y'))
month <- as.numeric(format(as.Date(pre$Date),'%m'))
day <- as.numeric(format(as.Date(pre$Date),'%d'))

##Viendo que series tenian datos y fuero homogeneizadas
unique(substr(colnames(pre[,-1]),1,10))

unique(substr(colnames(tmax[,-1]),1,10))

unique(substr(colnames(tmin[,-1]),1,10))

##Seleccionando las series homogeneizadas que cumplan con la condicion +% de datos originales, menor SNHT, menor RMSE

pre <- data.frame(pre[,-1])
##Reemplazar por estaciones del grupo
tmax <- data.frame(ho00000503=tmax$ho00000503.3,ho00000541=tmax$ho00000541.5,
                   ho00000542=tmax$ho00000542.6,ho00000548=tmax$ho00000548,
                   ho00000554=NA,ho00000635=tmax$ho00000635.3,ho00000648=NA)

tmin <- data.frame(ho00000503=tmin$ho00000503.5,ho00000541=tmin$ho00000541.2,
                   ho00000542=tmin$ho00000542.5,ho00000548=tmin$ho00000548.2,
                   ho00000554=NA,ho00000635=tmin$ho00000635.2,
                   ho00000648=tmin$ho00000648)


##Generando fichero por estacion

archivo <- substr(list.files(pattern = "^ho000.*\\.txt$"),1,10)

estacion<- list()
for (i in 1:length(archivo)){
  estacion[[i]] <- data.frame(year,month,day,Pp=pre[,i],Tmax=tmax[,i],Tmin=tmin[,i])
  #Exportando fichero
  write.table(estacion[[i]],file =paste0(archivo[i],".csv"),sep = ",",row.names=F)
}


##################################
########################### Cálculo de índices climáticos


##Lectura de archivo
data <- estacion[[1]]   #####ho00000503, estacion[[2]] seria h00000541 y asi seguria

##Estblecienco nombred de columnas
colnames(data) <- c("Año","Mes","Dia","Pp","Tmax","Tmin")

##Columna de fechas
data$Fecha <- as.Date(paste0(data$Año,"-",data$Mes,"-",data$Dia))



head(data,10)


##############Indices climaticos

#####Calculo "manual"
##cdd: Mayor número de días secos consecutivos PP < 1 mm por año
cddf <- NULL

for (j in min(data$Año):max(data$Año)){
  dsec <- 0
  cdd2 <- 0
  #Recorre la longitud de la columna Pp por año
  for(i in 1:length(data$Pp[data$Año == j])) {
    #Condicional ¿El dia es seco?
    if(!is.na(data$Pp[data$Año == j][i]) && data$Pp[data$Año == j][i] < 1) {
      # Agrega dias secos
      dsec <- dsec+ 1
      # Comparacion de secuencias, queda el mas largo
      if(dsec > cdd2) {
        cdd2 <- dsec
      }
      #¿Dia no seco? entonces reinica el contador
      #Recordar que cdd2 va mantener la mayor cantidad de dias secos
    } else {
      dsec <- 0
    }
  }
  #Agrupar por año
  df <- data.frame(Año = j,CDD = cdd2)
  cddf <- rbind(cddf,df)
}

cddf

#Graficado
# Serie de tiempo
plot(cddf$Año,cddf$CDD,type="l",col="green",main="Estacion CDD .....")

library(ggplot2)
library(plotly)

# serie de tiempo: ggplot
g_cdd <- ggplot(cddf,aes(x=Año,y=CDD))+
  geom_line(col="green",lwd=0.7)+
  theme_light()+ggtitle("Indice CDD Estacion ..............")+
  scale_x_continuous(breaks = seq(1965,2019,2))+
  theme(plot.title = element_text(hjust = 0.5))
ggplotly(g_cdd) 

# boxplot 
box_cdd <- ggplot(cddf,aes(y=CDD))+ geom_boxplot()
ggplotly(box_cdd)                                                                               

#........................


#############################################
##################Paquete RClimDex

###Instalación directa no funciona
###install.packages("RClimDex")

#install.packages(devtools)
library(devtools)

##Instalando RClimdex desde github
#devtools::install_github("ECCC-CDAS/RClimDex")

library(RClimDex)
#install.packages("PCICt")
library(PCICt)
library(tidyverse)

##Inicializando RClimdex (Shiny app)
rclimdex.start()




####################Paquete climdex.pcic

#devtools::install_github("pacificclimate/climdex.pcic")
library(climdex.pcic)

#############Estableciendo fechas
Fecha <- as.character(data$Fecha)

Fecha <- as.PCICt(Fecha, cal="365_day")

#########Creando fichero interno de datos para climdex.pcic

ci <- climdexInput.raw(data$Tmax,
                       data$Tmin, data$Pp,
                       Fecha,Fecha,Fecha, base.range=c(1965, 2019),
                       northern.hemisphere = F)

#########################Cálculo de todos los índices

##cdd: Mayor número de días secos consecutivos PP < 1 mm por año
cdd <- climdex.cdd(ci)
cddf <- data.frame(cdd)
cddf$Year <- 1965:2019

library(ggplot2)

ggplot(data=cddf,aes(x= Year,y= cdd))+
  coord_cartesian()+
  geom_line(aes(x= Year,y= cdd),col="green")+
  theme_light()+ggtitle("Indice ... Estacion ..............")+
  theme(plot.title = element_text(hjust = 0.5))


##prcptot: Precipitación total anual  
prcptot <- climdex.prcptot(ci)
prcptotf <- data.frame(prcptot)

##Mayor número de días húmedos consecutivos PP >=1 mm por año

cwd <- climdex.cwd(ci)
cwdf <- data.frame(cwd)

##Rango de temperatura mensual
dtr <- climdex.dtr(ci)
dtr <- data.frame(dtr)

##Numero de dias de heladas anual
fd <- climdex.fd(ci)
fd <- data.frame(fd)

##Duración de la temporada de crecimiento
gsl <- climdex.gsl(ci)
gsl <- data.frame(gsl)

##..........................
id <- climdex.id(ci)
id <- data.frame(id)

r10mm <- climdex.r10mm(ci) 
r10mm <- data.frame(r10mm)

r20mm  <- climdex.r20mm(ci) 
r20mm <- data.frame(r20mm)

r95ptot <- climdex.r95ptot(ci)
r95ptot <- data.frame(r95ptot)

r99ptot <- climdex.r99ptot(ci)
r99ptot <- data.frame(r99ptot)

rnnmm <- climdex.rnnmm(ci)
rnnmm <- data.frame(rnnmm)

rx1day <- climdex.rx1day(ci)
rx1day <- data.frame(rx1day)

rx5day <- climdex.rx5day(ci)
rx5day <- data.frame(rx5day)

sdii <- climdex.sdii(ci)
sdii <- data.frame(sdii)

su <- climdex.su(ci)
su <- data.frame(su)

tn10p <- climdex.tn10p(ci)
tn10p <- data.frame(tn10p)

tn90p <- climdex.tn90p(ci)
tn90p <- data.frame(tn90p)

tnn <- climdex.tnn(ci)
tnn <- data.frame(tnn)

tnx <- climdex.tnx(ci)
tnx <- data.frame(tnx)

tr <- climdex.tr(ci)
tr <- data.frame(tr)

tx10p <- climdex.tx10p(ci)
tx10p <- data.frame(tx10p)

tx90p <- climdex.tx90p(ci)
tx90p <- data.frame(tx90p)

txn <- climdex.txn(ci)
txn <- data.frame(txn)

txx <- climdex.txx(ci)
txx <- data.frame(txx)

wsdi <- climdex.wsdi(ci)
wsdi <- data.frame(wsdi)

csdi <- climdex.csdi(ci)
csdi <- data.frame(csdi)





############

#########################Metodo completo, calculo de todos los indices directamente


######OJO: Aqui ponees todas tus estaciones homogeneizadas, en este caso son 7, ustedes modifiquen con sus estaciones

files <- c("ho00000503.csv","ho00000541.csv","ho00000542.csv","ho00000548.csv","ho00000554.csv","ho00000635.csv","ho00000648.csv")
estacion <- list()


for (k in 1:length(files)){
  estacion[[k]] <-read.csv(files[[k]],sep = ",")
}


#install.packages("climdex.pcic")
library(climdex.pcic)
#install.packages("reshape2")
library(reshape2)
#install.packages("htmlwidgets")
library(htmlwidgets)
#install.packages("plotly")
library(plotly)
library(ggplot2)
#############Hasta aqui





####Continua

#Aqui ponemos todas las funciones disponibles de climdex.pcic
#Son 27 y estan mas arriba solo completenlos con todos los que necesiten
funciones <- list("climdex.cdd","climdex.csdi","climdex.dtr" ,"climdex.prcptot", "climdex.cwd", "climdex.r10mm","climdex.r95ptot","climdex.fd","climdex.gsl","climdex.id","climdex.r20mm")


for (j in 1:length(funciones)){
  #Objeto nulo como inicializador
  index_df <- NULL
  for (i in 1:length(files)){ 
    
    #Nombres de columnas
    colnames(estacion[[i]]) <- c("Año","Mes","Dia","Pp","Tmax","Tmin")
    
    ##Vector de fechas
    Fecha <- as.Date(paste0(estacion[[i]]$Año,"-",estacion[[i]]$Mes,"-",estacion[[i]]$Dia))
    
    #install.packages("climdex.pcic")
    library(climdex.pcic)
    
    #############Convirtiendo a formato PCIC 
    Fecha_pcic <- as.character(Fecha)
    
    Fecha_pcic <- as.PCICt(Fecha_pcic, cal="365_day")
    #########Creando fichero interno de datos para climdex.pcic
    ci <- climdexInput.raw(as.numeric(estacion[[i]]$Tmax),
                           as.numeric(estacion[[i]]$Tmin),as.numeric(estacion[[i]]$Pp),
                           Fecha_pcic,Fecha_pcic,Fecha_pcic, base.range=c(1965, 2019),
                           northern.hemisphere = F)
    
    #Aplicando do.callc (llama a una funcion como string)
    index <- do.call(funciones[[j]], list(ci))
  
  
    #Agrupando cada archivo de cada estacion
    index_df <- cbind(index_df,index)
    
    index_df2 <- data.frame(index_df)
    
    #Si es la ultima estacion aplicar lo demas
    if (i==length(estacion)){
      colnames(index_df2) <-  c("Jauja","Oyon","Picoy","Matucana","Tarma","Huayao","Pilchaca")
      ##Si los resultados son mensuales
      if (nrow(index_df2 > 55)){
        #Fechas para el nuevo dataframe
        Fecha <- as.Date(paste0(rownames(index_df2),"-07-16"))
        index_df2 <- data.frame(Fecha = Fecha,index_df2)
        
        #Creando titulo y eje Y como mayusculas
        Name_index <- toupper(substr(funciones[[j]],9,nchar(funciones[[j]])))
        
        # Crear los subplots con facet_wrap()
        #melt agrupa todas las estaciones en una sola columna
        
        graph <- ggplot(melt(index_df2, id.vars = "Fecha",variable.name = "Estacion"), aes(x = Fecha, y = value)) +
          geom_line(aes(col=Estacion)) +
          facet_wrap(~ Estacion, ncol = 1, scales = "free_y")+
          
          
          ##########OJO aqui modifcar los colores con la cantidad de estaciones
          ##########En este caso son 7 , si teien mas estaciones agregar mas colores
          
          
          scale_color_manual(values = c("red", "blue", "green","black","violet","orange",
                                        "cyan4"))+ ylab(toupper(substr(funciones[[j]],9,nchar(funciones[[j]]))))+ggtitle(paste0("Índice ",Name_index," 1965 - 2019"))+
          theme_bw()+
          theme(plot.title = element_text(hjust = 0.5,face = "bold"),
                strip.background = element_rect(fill = "cadetblue1"),  strip.text = element_text(color = "black", face = "bold"))+
          scale_x_date(date_labels = "%Y", date_breaks = "3 year")+
          theme(axis.title = element_text(face = "bold"))
        
        
        
        ggsave(plot=graph,file=paste0(funciones[[j]],".png"), width = 9, height = 9)
        inte <- ggplotly(graph)
        htmlwidgets::saveWidget(inte,paste0(funciones[[j]],"_i_.html"))
      
        #En el caso que sean anuales
      }else{
        Fecha <- as.Date(paste0(rownames(index_df2),"-16"))
        index_df2 <- data.frame(Fecha= Fecha,index_df2)
        
        Name_index <- toupper(substr(funciones[[j]],9,nchar(funciones[[j]])))
        
        # Crear los subplots con facet_wrap()
        
        
        graph <- ggplot(melt(index_df2, id.vars = "Fecha",variable.name = "Estacion"), aes(x = Fecha, y = value)) +
          geom_line(aes(col=Estacion)) +
          facet_wrap(~ Estacion, ncol = 1, scales = "free_y")+
          
          ##########OJO aqui modifcar los colores con la cantidad de estaciones
          ##########En este caso son 7 , si teien mas estaciones agregar mas colores
          
          scale_color_manual(values = c("red", "blue", "green","black","violet","orange",
                                        "cyan4"))+ ylab(toupper(substr(funciones[[j]],9,nchar(funciones[[j]]))))+ggtitle(paste0("Índice ",Name_index," 1965 - 2019"))+
          theme_bw()+
          theme(plot.title = element_text(hjust = 0.5,face = "bold"),
                strip.background = element_rect(fill = "cadetblue1"),  strip.text = element_text(color = "black", face = "bold"))+
          scale_x_date(date_labels = "%Y", date_breaks = "3 year")+
          theme(axis.title = element_text(face = "bold"))
     
        ggsave(plot=graph,file=paste0(funciones[[j]],".png"), width = 9, height = 9)
        inte <- ggplotly(graph)
        htmlwidgets::saveWidget(inte,paste0(funciones[[j]],"_i_.html"))
        
      }
      
    }

    write.csv(index_df2 , file = paste0(funciones[[j]],".csv"), row.names = FALSE)
    
  }
} 




