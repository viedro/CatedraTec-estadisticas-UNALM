
##################Validación de GCM

#Directorio de trabajo
setwd("C:/Users/ASUS/Desktop/Catedra_2024_1/Clase5-Validacion")
getwd()

#Lista de archivos de estaciones (resultado de homogeneización)
#Nota: este archivo sale de la primera parte del codigo de calculo de indices extremos
files <- list.files(pattern = "^ho0.*\\.csv$")


#install.packages("readxl")
library(readxl)
#Metadatos
meta <- read_excel("Estaciones_Meteorologicas_Peru.xlsx",col_names = F)
meta

#Nombres de columnas
colnames(meta) <- c("code","lat_grad","lat_min","lat_seg","lon_grad","lon_min","lon_seg","alt","nombre")

#Seleccionar solo los metadatos de nuestras estaciones
est <- NULL
for (i in 1:length(files)){
  est <- rbind(est,meta[meta$code == substr(files[i],1,10),])
}
est


#Reemplazando el objeto
meta <- est

#Funcion para convertir coordenadas sexagesimales a decimales
sexagesimal <- function(x, y, z, digits = 4){
  value <- as.numeric(x+(-y-z/60)/60)
  return(as.numeric(format(round(value, digits), nsmall = digits)))
}

head(meta)
#Convertimos a grados decimales
meta$LATITUDE <- sexagesimal(est$lat_grad,est$lat_min,est$lat_seg, digits = 4)
meta$LATITUDE
meta$LONGITUDE <- sexagesimal(est$lon_grad,est$lon_min,est$lon_seg, digits = 4)
meta$LONGITUDE 


#install.packages("metR")
library(metR)

#install.packages("ggstatsplot")
library(ggstatsplot)

# Carga el paquete stringr
#install.packages("stringr")
library(stringr)

#install.packages("rempsyc")
#Permite hacer tablas en formato APA
library(rempsyc)
library(ggplot2)

#pr_day_CESM2_historical_19700101-20141231.nc
##########escogemos la variable a trabjar
#var="pp"
#var="tmin"
#var="tmax"

###Listar archivos de modelos para la variable de pp,tmax,tmin
archivos <- list.files(pattern = paste0(".*",var,".*hist.*\\.nc$"))
archivos

# Usa str_extract() para obtener la parte de la cadena que está después de "d12k_" y antes de "_hist"
name_model <- str_extract(archivos, "(?<=d12k_).*(?=_hist)")

#name_model <- str_extract(archivos, "(?<=day_).*(?=_historical)")
#install.packages("Fgmutils")
library(Fgmutils)
library(Metrics)
library(dplyr)
################# Validacion diaria


tabla2 <- data.frame(ini =rep(0,length(files)))

####modelos
for (j in 1:length(archivos)){
  tabla <- NULL
  ini <- NULL
 
  #observado - estaciones
  for (i in 1:length(files)){
    #Lectura de estaciones
    data <- read.csv(files[i])
    #Nombres de columnas
    colnames(data) <- c("Year","Month","Day","pp","tmax","tmin")
    
    #Creando columna de fecha
    data$date <- as.Date(paste0(data$Year,"-",data$Month,"-",data$Day))
    
    #Filtro rango 
    data <- data[data$date >= as.Date("1981-01-01") & data$date <= as.Date("2005-12-31"),]
    tail(data)
    length(data$pp)
    model <- ReadNetCDF(archivos[j],subset=list(lat=meta$LATITUDE[i],lon=meta$LONGITUDE[i]))
    
    
    if (var== "pp"){
      data <- data.frame(Date=data$date,var_obs=data$pp,var_model= model$pr)
    }else if (var== "tmax"){
      data <- data.frame(Date=data$date,var_obs=data$tmax,var_model= model$tasmax)
    }else{
      data <- data.frame(Date=data$date,var_obs=data$tmin,var_model= model$tasmin)
    }
    
    data2 <- data.frame(data,estacion = meta$nombre[i])
    
    ini <- rbind(ini,data2)
    
    
    
    rmse <- rmse(data$var_obs, data$var_model)
    mae <- mae(data$var_obs, data$var_model)
    bias <- Fgmutils::bias(data$var_obs, data$var_model)
    cor <- cor(data$var_obs, data$var_model,method="spearman")
    
    fin <- data.frame(cor, rmse, mae, bias)
    
    
    colnames(fin) <- c(paste0(name_model[j],".r"), paste0(name_model[j],".RMSE"), paste0(name_model[j],".MAE"), paste0(name_model[j],".BIAS"))
    tabla <- rbind(tabla,fin)
    
  }
  
  graph <- grouped_ggscatterstats(
    data             = ini,
    x                = var_obs,
    y                = var_model,
    type             = "no-parametric",
    grouping.var     = estacion,
    marginal         = F,
    xlab             = "Observado",
    ylab             = paste0("Modelo ",name_model[j]),
    plotgrid.args = list(ncol=2))
  
  ggsave(plot=graph,file=paste0(var,"_d_",name_model[j],".png"), width = 12, height = 10)
  
  tabla2 <- cbind(tabla2,tabla) 
}

tabla2 <- tabla2[,-1]

tabla2



final_table <- cbind("Estación"=meta$nombre,tabla2)
View(final_table)
nice_table(final_table)

my_table <- nice_table(final_table, separate.header = TRUE, italics = seq(final_table),
                       title = c("Tabla 1", paste0("Medidas de validación serie diaria ",var," Sierra Centro, modelos vs observado")))


#print(my_table, preview = "docx")

#install.packages("flextable")

flextable::save_as_docx(my_table, path = paste0("tabla_dia_",var,".docx"))

write.csv(final_table, file = paste0(var,"_dia.csv"), row.names = FALSE)


####################################################################
##########################################################################
###############################################################################
#Validacion mensual


tabla2 <- data.frame(ini =rep(0,length(files)))


for (j in 1:length(archivos)){
  tabla <- NULL
  ini <- NULL

  for (i in 1:length(files)){
    #Lectura de estaciones
    data <- read.csv(files[i])
    #Nombres de columnas
    colnames(data) <- c("Year","Month","Day","pp","tmax","tmin")
    
    
    #Diario a mensual para tmin,tmax y pp
    data<- data %>% group_by(Year,Month) %>%
      summarize(tmax = mean(tmax), tmin = mean(tmin),pp=sum(pp))
    
    #Creando columna de fecha
    data$date <- as.Date(paste0(data$Year,"-",data$Month,"-",1))
    
    #Filtro rango 
    data <- data[data$date >= as.Date("1981-01-01") & data$date <= as.Date("2005-12-31"),]
    
    
    
   
    model <- ReadNetCDF(archivos[j],subset=list(lat=meta$LATITUDE[i],lon=meta$LONGITUDE[i]))
    model$Year <- as.numeric(format(model$time,"%Y"))
    model$Month <- as.numeric(format(model$time,"%m"))
    
    if (var== "pp"){
      model <- model %>% group_by(Year,Month) %>%
        summarise(pr=sum(pr))
      
      data <- data.frame(Date=data$date,var_obs=data$pp,var_model= model$pr)
    }else if (var=="tmax"){
      model <- model %>% group_by(Year,Month) %>%
        summarise(tasmax=mean(tasmax))
      
      
      data <- data.frame(Date=data$date,var_obs=data$tmax,var_model= model$tasmax)
    }else{
      model <- model %>% group_by(Year,Month) %>%
        summarise(tasmin=mean(tasmin))
      
      
      data <- data.frame(Date=data$date,var_obs=data$tmin,var_model= model$tasmin)
    }
    
    data2 <- data.frame(data,estacion = meta$nombre[i])
    ini <- rbind(ini,data2)
    
    
    library(Metrics)
    
    rmse <- rmse(data$var_obs, data$var_model)
    mae <- mae(data$var_obs, data$var_model)
    bias <- Fgmutils::bias(data$var_obs, data$var_model)
    cor <- cor(data$var_obs, data$var_model,method="spearman")
    
    fin <- data.frame(cor, rmse, mae, bias)
    
    
    colnames(fin) <- c(paste0(name_model[j],".r"), paste0(name_model[j],".RMSE"), paste0(name_model[j],".MAE"), paste0(name_model[j],".BIAS"))
    tabla <- rbind(tabla,fin)
    
  }
  
  graph <- grouped_ggscatterstats(
    data             = ini,
    x                = var_obs,
    y                = var_model,
    type             = "no-parametric",
    grouping.var     = estacion,
    marginal         = F,
    xlab             = "Observado",
    ylab             = paste0("Modelo ",name_model[j]),
    ggplot.component = list(geom_rug(sides = "b")),
    plotgrid.args = list(ncol=2))
  
  ggsave(plot=graph,file=paste0(var,"_m_",name_model[j],".png"), width = 12, height = 10)
  
  tabla2 <- cbind(tabla2,tabla)
 
}


tabla2 <- tabla2[,-1]

tabla2



final_table <- cbind("Estación"=meta$nombre,tabla2)


my_table <- nice_table(final_table, separate.header = TRUE, italics = seq(final_table),
                       title = c("Tabla 1", paste0("Medidas de validación serie mensual ",var," Sierra Centro, modelos vs observado")))

#print(my_table, preview = "docx")



flextable::save_as_docx(my_table, path = paste0("tabla_men_",var,".docx"))


write.csv(final_table, file = paste0(var,"_mes.csv"), row.names = FALSE)




