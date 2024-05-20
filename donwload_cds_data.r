setwd("C:/Users/isabe/OneDrive/Documentos/GitHub/bird_sounds/files/code")

# Establecer el directorio de trabajo
setwd("C:/Users/isabe/OneDrive/Documentos/GitHub/bird_sounds/files/code")

# Verificar e instalar paquetes necesarios
if (!require(foreach) || !require(ecmwfr) || !require(raster) || !require(sp) || !require(rgdal) || !require(tidyr) || !require(dplyr) || !require(lubridate) || !require(ncdf4) || !require(googleway)) {
  install.packages(c("foreach", "ecmwfr", "raster", "sp", "rgdal", "tidyr", "dplyr", "lubridate", "ncdf4", "googleway"))
}

## Cargar las librerías necesarias
library(foreach)
library(ecmwfr)
library(raster)
library(sp)
library(rgdal)
library(tidyr)
library(dplyr)
library(lubridate)
library(ncdf4)
library(googleway)
library(tidyverse)

# Establecer la clave de usuario y clave de API para CDS
wf_set_key(user ="308579", key = "4d274226-aa76-44b8-a235-013e93b4d869", service = "cds")

# Setear la clave de la API de Google Maps Elevation
api_key <- "AIzaSyDTdfNAwyp4-5R-mgfhSOCwIF_dbvETlAw"

# Leer los datos del archivo CSV
datos <- read_delim("birdsongs_data.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

d<-datos[1:20,]
# Definir las variables a descargar
variables <- c('2m_temperature', 'total_precipitation')

# Inicializar un data frame para almacenar los resultados
resultados <- data.frame(ID = character(), fecha = character(), temperatura = numeric(), precipitacion = numeric(), altitud = numeric()) 

# Iterar sobre cada fila de datos
foreach(i = 1:nrow(d)) %do% {
  # Obtener los valores de la fila actual
  nlat <- d[i, "nlat"]
  slat <- d[i, "slat"]
  wlon <- d[i, "wlon"]
  elon <- d[i, "elon"]
  anio <- as.numeric(d[i, "anio"])
  region <- d[i, "region"]
  fecha <- pull(d[i, "fecha"])
  tx <- as.character(d[i, "ID"])
  
  # Calcular las coordenadas promedio

  
  # Obtener datos de elevación utilizando Google Maps Elevation API
  elevation_data <- tryCatch(
    google_elevation(df_locations = data.frame(lat = as.numeric(nlat), lon = as.numeric(wlon)), key = api_key),
    error = function(e) {
      message("Error obteniendo datos de elevación para la fila ", i, ": ", conditionMessage(e))
      return(NULL)
    }
  )
  
  # Verificar si se obtuvieron datos de elevación
  if (!is.null(elevation_data)) {
    # Extraer la altitud del resultado
    altitud <- elevation_data$results$elevation
  } else {
    altitud <- NA
  }
  
  # Configurar la solicitud de descarga
  request <- list(
    "dataset_short_name" = "reanalysis-era5-single-levels",
    "product_type" = "reanalysis",
    "variable" = variables,
    "year" = anio,
    "month" =  month(as.Date(fecha)),
    "day" = day(as.Date(fecha)),
    "time" = c(paste0("0",0:9,":00"),paste0(10:23,":00")),
    "area" = paste(nlat, wlon, slat, elon, sep = "/"),  #(N, W, S, E) nlat wlon slat elon
    "format" = "netcdf",
    "target" = paste0("era5_", region, "_", anio, "_", tx, ".nc")
  )
  
  # Realizar la solicitud y descargar los datos
  file <- tryCatch(
    wf_request(
      user = "308579",   # ID de usuario (para autenticación)
      request = request,  # la solicitud
      transfer = TRUE,     # descargar el archivo
      path = "C:/Users/isabe/OneDrive/Documentos/GitHub/bird_sounds/files/era5data"       # almacenar los datos en el directorio de trabajo actual
    ),
    error = function(e) {
      message("Error descargando datos para la fila ", i, ": ", conditionMessage(e))
      return(NULL)
    }
  )
  
  # Procesar los datos si el archivo se descargó correctamente
  if (!is.null(file)) {
    # Abrir el archivo NetCDF
    nc <- nc_open(file)
    
    # Extraer las variables de interés
    temp_kelvin <- ncvar_get(nc, "t2m")  # Temperatura en Kelvin
    
    # Convertir la temperatura de Kelvin a Celsius
    temp_promedio <- apply(temp_kelvin-273.15, c(1, 2), mean)
    temp_promedio_diaria <- mean(temp_promedio)
    
    # Extraer la precipitación total
    precipitacion <- ncvar_get(nc, "tp")
    
    # Calcular la precipitación total
    precipitacion_total <- apply(precipitacion, c(1, 2), sum) * 1000  # Convertir a mm
    pre_total_diaria <- mean(precipitacion_total)
    
    # Agregar los resultados al data frame
    resultados <- bind_rows(resultados, data.frame(
      ID = tx,
      fecha = as.character(fecha),
      temperatura = temp_promedio_diaria,
      precipitacion = pre_total_diaria,
      altitud = altitud
    ))
    
    # Eliminar el archivo NetCDF descargado
    unlink(file)
  }
}

# Guardar los resultados en un archivo CSV
write_csv(resultados, "climate_variables.csv")

# Imprimir los resultados
print(resultados)
