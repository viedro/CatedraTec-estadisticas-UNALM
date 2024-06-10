###Indices extremos con datos de modelos

###Estableciendo directorio de trabajo
setwd("C:/Users/ASUS/Desktop/Catedra_2024_1/Clase6-Extremos_fut")

###Ruta donde estan los modelos
path="C:/Users/ASUS/Desktop/Catedra_2024_1/Clase5-Validacion"

###Listar archivos de modelos para la variable de pp
archivos_pp <- list.files(path=path,pattern = ".*pp.*rcp85.*\\.nc$", full.names = T)
archivos_pp

archivos_tmax <- list.files(path=path,pattern = ".*tmax.*rcp85.*\\.nc$", full.names = T)
archivos_tmax

archivos_tmin <- list.files(path=path,pattern = ".*tmin.*rcp85.*\\.nc$", full.names = T)
archivos_tmin

#install.packages("stringr")
library(stringr)
###Extrayendo nombres de modelos de los modelos listados
Name_model <- str_extract(archivos_tmax, "(?<=d12k_).*(?=_rcp85)")

###Importando el archivo est.csv generado al homogeneizar nuestros datos en bruto
obs <- read.csv("Est.csv")
obs

colnames(obs) <- c("file","lon","lat","alt","code","name")
obs


#############################################

i=1
j=3

#install.packages("metR")
library(metR)

#####Lectura de modelos listados anteriormente: precipitacion
mod_sta_pp <- list()
for (i in 1:nrow(obs)){
  ini <-NULL
  for (j in 1:length(archivos_pp)){
    model <- metR::ReadNetCDF(archivos_pp[j],subset=list(lat=obs$lat[i],lon=obs$lon[i]))
    if (j ==1){
      ini <- cbind(ini,model)
    }else{
      ini <- cbind(ini,model$pr)
    }
  }
  colnames(ini)[4:ncol(ini)]<- Name_model
  ini$time <- as.Date(ini$time)
  mod_sta_pp[[i]] <- ini
  write.csv(mod_sta_pp[[i]], file = paste0("model_rcp_pp_",obs$name[i],".csv"), row.names = FALSE)
  
}
obs
mod_sta_pp

#####################tmin: temp minima
mod_sta_tmin <- list()
for (i in 1:length(obs$file)){
  ini <-NULL
  for (j in 1:length(archivos_tmin)){
    model <- ReadNetCDF(archivos_tmin[j],subset=list(lat=obs$lat[i],lon=obs$lon[i]))
    if (j ==1){
      ini <- cbind(ini,model)
    }else{
      ini <- cbind(ini,model$tasmin)
    }
  }
  colnames(ini)[4:ncol(ini)]<- Name_model
  ini$time <- as.Date(ini$time)
  mod_sta_tmin[[i]] <- ini
  write.csv(mod_sta_tmin[[i]], file = paste0("model_rcp_tmin_",obs$name[i],".csv"), row.names = FALSE)
  
}

mod_sta_tmin

######################tmax: temp maxima
mod_sta_tmax <- list()
for (i in 1:length(obs$file)){
  ini <-NULL
  for (j in 1:length(archivos_tmax)){
    model <- ReadNetCDF(archivos_tmax[j],subset=list(lat=obs$lat[i],lon=obs$lon[i]))
    if (j ==1){
      ini <- cbind(ini,model)
    }else{
      ini <- cbind(ini,model$tasmax)
    }
  }
  colnames(ini)[4:ncol(ini)]<- Name_model
  ini$time <- as.Date(ini$time)
  mod_sta_tmax[[i]] <- ini
  write.csv(mod_sta_tmax[[i]], file = paste0("model_rcp_tmax_",obs$name[i],".csv"), row.names = FALSE)
  
}

mod_sta_tmax


#########################Metodo completo, calculo de todos los indices directamente


#install.packages("climdex.pcic")
library(climdex.pcic)
#install.packages("reshape2")
library(reshape2)
#install.packages("htmlwidgets")
library(htmlwidgets)
#install.packages("plotly")
library(plotly)
library(ggplot2)


#Aqui ponemos todas las funciones disponibles de climdex.pcic
#Son 27 y estan mas arriba solo completenlos con todos los que necesiten
funciones <- list("climdex.cdd", "climdex.prcptot", "climdex.cwd", "climdex.dtr", "climdex.fd", "climdex.gsl", "climdex.id", "climdex.r10mm","climdex.csdi")

#funciones <- list("climdex.cdd", "climdex.prcptot", "climdex.cwd", "climdex.dtr", "climdex.fd", "climdex.gsl", "climdex.id", "climdex.r10mm","climdex.csdi", "climdex.r20mm","climdex.r95ptot","climdex.r99ptot","climdex.rnnmm","climdex.rx1day","climdex.rx5day","climdex.sdii","climdex.su","climdex.tn10p","climdex.tn90p","climdex.tnn","climdex.tnx","climdex.tr","climdex.tx10p","climdex.tx90p","climdex.txn","climdex.txx","climdex.wsdi")


###Poner los colores de acuerdo a la cantidad de estaciones que se tiene, en este caso 7.
###Si hay mas estaciones, agrega mas colores
color <- c("red", "blue", "green","black","violet","orange",
           "cyan4")

##########################################################################
###################################Para el modelo ACCES1-0
##########################################################################
i=1
j=1

for (j in 1:length(funciones)){
  #Objeto nulo como inicializador
  index_df <- NULL
  for (i in 1:length(mod_sta_tmax)){ 
    
    ##Vector de fechas
    Fecha <- as.Date(mod_sta_tmax[[1]]$time)
    
    #install.packages("climdex.pcic")
    library(climdex.pcic)
    
    #############Convirtiendo a formato PCIC 
    Fecha_pcic <- as.character(Fecha)
    Fecha_pcic <- as.PCICt(Fecha_pcic, cal="365_day")
    #########Creando fichero interno de datos para climdex.pcic
    ci <- climdexInput.raw(as.numeric(mod_sta_tmax[[i]]$`ACCESS1-0`),
                           as.numeric(mod_sta_tmin[[i]]$`ACCESS1-0`),as.numeric(mod_sta_pp[[i]]$`ACCESS1-0`),
                           Fecha_pcic,Fecha_pcic,Fecha_pcic, base.range=c(2006, 2065),
                           northern.hemisphere = F)
    
    #Aplicando do.callc (llama a una funcion como string)
    index <- do.call(funciones[[j]], list(ci))
    
    #Agrupando cada archivo de cada estacion
    index_df <- cbind(index_df,index)
    
    index_df2 <- data.frame(index_df)
    
    #Si es la ultima estacion aplicar lo demas
    if (i==length(obs$name)){
      colnames(index_df2) <-  obs$name
      
      if (nrow(index_df2 > 60)){
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
          
          scale_color_manual(values = color)+ ylab(toupper(substr(funciones[[j]],9,nchar(funciones[[j]]))))+ggtitle(paste0("Índice ",Name_index," Modelo ACCESS1-0 2006 - 2065"))+
          theme_bw()+
          theme(plot.title = element_text(hjust = 0.5,face = "bold"),
                strip.background = element_rect(fill = "cadetblue1"),  strip.text = element_text(color = "black", face = "bold"))+
          scale_x_date(date_labels = "%Y", date_breaks = "3 year")+
          theme(axis.title = element_text(face = "bold"))+geom_smooth(method = "lm")
        
        ggsave(plot=graph,file=paste0(funciones[[j]],"ACCESS1-0.png"), width = 9, height = 9)
        inte <- ggplotly(graph)
        htmlwidgets::saveWidget(inte,paste0(funciones[[j]],"Acces1-0_i_.html"))
        
        #En el caso que sean mensuales
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
          
          scale_color_manual(values = color)+ ylab(toupper(substr(funciones[[j]],9,nchar(funciones[[j]]))))+ggtitle(paste0("Índice ",Name_index," Modelo ACCESS1-0 2006 - 2065"))+
          theme_bw()+
          theme(plot.title = element_text(hjust = 0.5,face = "bold"),
                strip.background = element_rect(fill = "cadetblue1"),  strip.text = element_text(color = "black", face = "bold"))+
          scale_x_date(date_labels = "%Y", date_breaks = "3 year")+
          theme(axis.title = element_text(face = "bold"))+geom_smooth(method = "lm")
        
        ggsave(plot=graph,file=paste0(funciones[[j]],"ACCESS1-0.png"), width = 9, height = 9)
        inte <- ggplotly(graph)
        htmlwidgets::saveWidget(inte,paste0(funciones[[j]],"Acces1-0_i_.html"))
      }
      
    }
    
    
    write.csv(index_df2 , file = paste0(funciones[[j]],"Acces1-0_i_.csv"), row.names = FALSE)
    
  }
} 

########################################################################3
############################################################################3

########################PARa el modelo HadGEM2-ES

for (j in 1:length(funciones)){
  #Objeto nulo como inicializador
  index_df <- NULL
  for (i in 1:length(mod_sta_tmax)){ 
    
    ##Vector de fechas
    Fecha <- as.Date(mod_sta_tmax[[1]]$time)
    
    #install.packages("climdex.pcic")
    library(climdex.pcic)
    
    #############Convirtiendo a formato PCIC 
    Fecha_pcic <- as.character(Fecha)
    Fecha_pcic <- as.PCICt(Fecha_pcic, cal="365_day")
    #########Creando fichero interno de datos para climdex.pcic
    ci <- climdexInput.raw(as.numeric(mod_sta_tmax[[i]]$`HadGEM2-ES`),
                           as.numeric(mod_sta_tmin[[i]]$`HadGEM2-ES`),as.numeric(mod_sta_pp[[i]]$`HadGEM2-ES`),
                           Fecha_pcic,Fecha_pcic,Fecha_pcic, base.range=c(2006, 2065),
                           northern.hemisphere = F)
    
    #Aplicando do.callc (llama a una funcion como string)
    index <- do.call(funciones[[j]], list(ci))
    
    #Agrupando cada archivo de cada estacion
    index_df <- cbind(index_df,index)
    
    index_df2 <- data.frame(index_df)
    
    #Si es la ultima estacion aplicar lo demas
    if (i==length(obs$name)){
      colnames(index_df2) <-  obs$name
      ##Si los resultados son anuales
      if (nrow(index_df2 > 60)){
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
          
          
          scale_color_manual(values = color)+ ylab(toupper(substr(funciones[[j]],9,nchar(funciones[[j]]))))+ggtitle(paste0("Índice ",Name_index," Modelo HadGEM2-ES 2006 - 2065"))+
          theme_bw()+
          theme(plot.title = element_text(hjust = 0.5,face = "bold"),
                strip.background = element_rect(fill = "cadetblue1"),  strip.text = element_text(color = "black", face = "bold"))+
          scale_x_date(date_labels = "%Y", date_breaks = "3 year")+
          theme(axis.title = element_text(face = "bold"))+geom_smooth(method = "lm")
        
        ggsave(plot=graph,file=paste0(funciones[[j]],"HadGEM2-ES.png"), width = 9, height = 9)
        inte <- ggplotly(graph)
        htmlwidgets::saveWidget(inte,paste0(funciones[[j]],"HadGEM2-ES_i_.html"))
        
        #En el caso que sean mensuales
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
          
          scale_color_manual(values = color)+ ylab(toupper(substr(funciones[[j]],9,nchar(funciones[[j]]))))+ggtitle(paste0("Índice ",Name_index," Modelo HadGEM2-ES 2006 - 2065"))+
          theme_bw()+
          theme(plot.title = element_text(hjust = 0.5,face = "bold"),
                strip.background = element_rect(fill = "cadetblue1"),  strip.text = element_text(color = "black", face = "bold"))+
          scale_x_date(date_labels = "%Y", date_breaks = "3 year")+
          theme(axis.title = element_text(face = "bold"))+geom_smooth(method = "lm")
        
        ggsave(plot=graph,file=paste0(funciones[[j]],"HadGEM2-ES.png"), width = 9, height = 9)
        inte <- ggplotly(graph)
        htmlwidgets::saveWidget(inte,paste0(funciones[[j]],"HadGEM2-ES_i_.html"))
      }
      
    }
    
    
    write.csv(index_df2 , file = paste0(funciones[[j]],"HadGEM2-ES_i_.csv"), row.names = FALSE)
    
  }
} 


########################################################################3
##############################Para el modelo MPI-ESM-LR
##########################################################################
for (j in 1:length(funciones)){
  #Objeto nulo como inicializador
  index_df <- NULL
  for (i in 1:length(mod_sta_tmax)){ 
    
    ##Vector de fechas
    Fecha <- as.Date(mod_sta_tmax[[1]]$time)
    
    #install.packages("climdex.pcic")
    library(climdex.pcic)
    
    #############Convirtiendo a formato PCIC 
    Fecha_pcic <- as.character(Fecha)
    Fecha_pcic <- as.PCICt(Fecha_pcic, cal="365_day")
    #########Creando fichero interno de datos para climdex.pcic
    ci <- climdexInput.raw(as.numeric(mod_sta_tmax[[i]]$`MPI-ESM-LR`),
                           as.numeric(mod_sta_tmin[[i]]$`MPI-ESM-LR`),as.numeric(mod_sta_pp[[i]]$`MPI-ESM-LR`),
                           Fecha_pcic,Fecha_pcic,Fecha_pcic, base.range=c(2006, 2065),
                           northern.hemisphere = F)
    
    #Aplicando do.callc (llama a una funcion como string)
    index <- do.call(funciones[[j]], list(ci))
    
    #Agrupando cada archivo de cada estacion
    index_df <- cbind(index_df,index)
    
    index_df2 <- data.frame(index_df)
    
    #Si es la ultima estacion aplicar lo demas
    if (i==length(obs$name)){
      colnames(index_df2) <-  obs$name
      ##Si los resultados son anuales
      if (nrow(index_df2 > 60)){
        #Fechas para el nuevo dataframe
        Fecha <- as.Date(paste0(rownames(index_df2),"-07-16"))
        index_df2 <- data.frame(Fecha = Fecha,index_df2)
        
        #Creando titulo y eje Y como mayusculas
        Name_index <- toupper(substr(funciones[[j]],9,nchar(funciones[[j]])))
        
        # Crear los subplots con facet_wrap()
        #melt agrupa todas las estaciones en una sola columna
        graph <- ggplot(melt(index_df2, id.vars = "Fecha",variable.name = "Estacion"), aes(x = Fecha, y = value)) +
          geom_line(aes(col=Estacion)) +
          facet_wrap(~ Estacion, ncol = 2, scales = "free_y")+
          
          
          ##########OJO aqui modifcar los colores con la cantidad de estaciones
          ##########En este caso son 7 , si teien mas estaciones agregar mas colores
          
          
          scale_color_manual(values = color)+ ylab(toupper(substr(funciones[[j]],9,nchar(funciones[[j]]))))+ggtitle(paste0("Índice ",Name_index," Modelo MPI-ESM-LR 2006 - 2065"))+
          theme_bw()+
          theme(plot.title = element_text(hjust = 0.5,face = "bold"),
                strip.background = element_rect(fill = "cadetblue1"),  strip.text = element_text(color = "black", face = "bold"))+
          scale_x_date(date_labels = "%Y", date_breaks = "5 year")+
          theme(axis.title = element_text(face = "bold"))+geom_smooth(method = "lm")
        
        ggsave(plot=graph,file=paste0(funciones[[j]],"MPI-ESM-LR.png"), width = 14, height = 9)
        inte <- ggplotly(graph)
        htmlwidgets::saveWidget(inte,paste0(funciones[[j]],"MPI-ESM-LR_i_.html"))
        
        #En el caso que sean mensuales
      }else{
        Fecha <- as.Date(paste0(rownames(index_df2),"-16"))
        index_df2 <- data.frame(Fecha= Fecha,index_df2)
        
        Name_index <- toupper(substr(funciones[[j]],9,nchar(funciones[[j]])))
        
        # Crear los subplots con facet_wrap()
        graph <- ggplot(melt(index_df2, id.vars = "Fecha",variable.name = "Estacion"), aes(x = Fecha, y = value)) +
          geom_line(aes(col=Estacion)) +
          facet_wrap(~ Estacion, ncol = 2, scales = "free_y")+
          
          ##########OJO aqui modifcar los colores con la cantidad de estaciones
          ##########En este caso son 7 , si teien mas estaciones agregar mas colores
          
          scale_color_manual(values = color)+ ylab(toupper(substr(funciones[[j]],9,nchar(funciones[[j]]))))+ggtitle(paste0("Índice ",Name_index," Modelo MPI-ESM-LR 2006 - 2065"))+
          theme_bw()+
          theme(plot.title = element_text(hjust = 0.5,face = "bold"),
                strip.background = element_rect(fill = "cadetblue1"),  strip.text = element_text(color = "black", face = "bold"))+
          scale_x_date(date_labels = "%Y", date_breaks = "5 year")+
          theme(axis.title = element_text(face = "bold"))+geom_smooth(method = "lm")
        
        ggsave(plot=graph,file=paste0(funciones[[j]],"MPI-ESM-LR.png"), width = 14, height = 9)
        inte <- ggplotly(graph)
        htmlwidgets::saveWidget(inte,paste0(funciones[[j]],"MPI-ESM-LR_i_.html"))
      }
      
    }
    
    
    write.csv(index_df2 , file = paste0(funciones[[j]],"MPI-ESM-LR_i_.csv"), row.names = FALSE)
    
  }
} 



###########Proyeccion tmax

i=1
for (i in 1:nrow(obs)){
  graph2 <- ggplot(melt(mod_sta_tmax[[i]][,c(1,4:6)], id.vars = "time",variable.name = "Fuente"), aes(x = time, y = value)) +
    geom_line(aes(col=Fuente)) +
    facet_wrap(~ Fuente, ncol = 1, scales = "free_y")+ggtitle(paste0("Estación ",obs$name[i]," Temperatura Máxima 2006 - 2065"))+
    theme_bw()+xlab("Fecha")+
    theme(plot.title = element_text(hjust = 0.5,face = "bold"),
          strip.background = element_rect(fill = "white"),  strip.text = element_text(color = "black", face = "bold"))+
    scale_x_date(date_labels = "%Y", date_breaks = "3 year")+
    theme(axis.title = element_text(face = "bold"))+geom_smooth(method = "lm")+ylab("")
  
  ggsave(plot=graph2,file=paste0("serie_proy_tmax_mod_",obs$name[i],".png"), width = 9, height = 9)
  inte <- ggplotly(graph2)
  htmlwidgets::saveWidget(inte,paste0("serie_proy_tmax_mod_",obs$name[i],"_i_.html"))
}



###########Proyeccion tmin

for (i in 1:length(obs$file)){
  graph2 <- ggplot(melt(mod_sta_tmin[[i]][,c(1,4:6)], id.vars = "time",variable.name = "Fuente"), aes(x = time, y = value)) +
    geom_line(aes(col=Fuente)) +
    facet_wrap(~ Fuente, ncol = 1, scales = "free_y")+ggtitle(paste0("Estación ",obs$name[i]," Temperatura Mínima 2006 - 2065"))+
    theme_bw()+xlab("Fecha")+
    theme(plot.title = element_text(hjust = 0.5,face = "bold"),
          strip.background = element_rect(fill = "cadetblue1"),  strip.text = element_text(color = "black", face = "bold"))+
    scale_x_date(date_labels = "%Y", date_breaks = "3 year")+
    theme(axis.title = element_text(face = "bold"))+geom_smooth(method = "lm")+ylab("")
  
  ggsave(plot=graph2,file=paste0("serie_proy_tmin_mod_",obs$name[i],".png"), width = 9, height = 9)
  inte <- ggplotly(graph2)
  htmlwidgets::saveWidget(inte,paste0("serie_proy_tmin_mod_",obs$name[i],"_i_.html"))
}


###########Proyeccion precipitacion


for (i in 1:length(obs$file)){
  graph2 <- ggplot(melt(mod_sta_pp[[i]][,c(1,4:6)], id.vars = "time",variable.name = "Fuente"), aes(x = time, y = value)) +
    geom_line(aes(col=Fuente)) +
    facet_wrap(~ Fuente, ncol = 1, scales = "free_y")+ggtitle(paste0("Estación ",obs$name[i]," Precipitación 2006 - 2065"))+
    theme_bw()+xlab("Fecha")+
    theme(plot.title = element_text(hjust = 0.5,face = "bold"),
          strip.background = element_rect(fill = "cadetblue1"),  strip.text = element_text(color = "black", face = "bold"))+
    scale_x_date(date_labels = "%Y", date_breaks = "3 year")+
    theme(axis.title = element_text(face = "bold"))+geom_smooth(method = "lm")
  
  ggsave(plot=graph2,file=paste0("serie_proy_pp_mod_",obs$name[i],".png"), width = 9, height = 9)
  inte <- ggplotly(graph2)
  htmlwidgets::saveWidget(inte,paste0("serie_proy_pp_mod_",obs$name[i],"_i_.html"))
}


#install.packages("xts")
library(xts)

########Descomposicion de una serie de tiempo

series <- xts(mod_sta_tmax[[1]][,4],order.by = as.Date(mod_sta_tmax[[1]]$time ))
class(series)

tmax_men <- apply.monthly(series, FUN = mean, na.rm = TRUE)

#my_df <- data.frame(index(tmax_men), coredata(tmax_men))


serie_men <- ts(tmax_men$`ACCESS1-0`,frequency = 12, start=c(2006,1))

dec_men <- decompose(serie_men, type="additive")

plot(dec_men)


mean(dec_men$random,na.rm=T)





