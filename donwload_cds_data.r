# Establecer el directorio de trabajo
setwd("./GitHub/bird_sounds/files")

# Verificar e instalar paquetes necesarios
if (!require(foreach) || !require(ecmwfr) || !require(googleway)) {
  install.packages(c("foreach", "ecmwfr", "googleway"))
}

# Cargar las librerías necesarias
library(foreach)
library(ecmwfr)
library(googleway)
library(tidyverse)

# Establecer la clave de usuario y clave de API para CDS
wf_set_key(user ="inserte_su_usuario", key = "inserte_key", service = "cds")

# Setear la clave de la API de Google Maps Elevation
api_key <- "inserte_key"

# Leer los datos del archivo CSV
datos <- read_delim("birdsongs_data.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Definir las variables a descargar
variables <- c('2m_temperature', 'total_precipitation')

# Inicializar un data frame para almacenar los resultados
resultados <- data.frame(ID = character(), fecha = character(), temperatura = numeric(), precipitacion = numeric(), altitud = numeric()) 

# Inicializar un data frame para almacenar los resultados de altitud
altitudes <- data.frame(ID = character(), altitud = numeric())

# Iterar sobre cada fila de datos para obtener los datos de altitud
foreach(i = 1:nrow(datos)) %do% {
  # Obtener los valores de la fila actual
  nlat <- datos[i, "nlat"]
  slat <- datos[i, "slat"]
  wlon <- datos[i, "wlon"]
  elon <- datos[i, "elon"]
  tx <- as.character(datos[i, "ID"])
  
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
    
    # Agregar la altitud al dataframe de altitudes
    altitudes <- bind_rows(altitudes, data.frame(ID = tx, altitud = altitud))
  } else {
    # Si no se obtienen datos de altitud, asignar NA
    altitudes <- bind_rows(altitudes, data.frame(ID = tx, altitud = NA))
  }
}

# Iterar sobre cada fila de datos para descargar y procesar los datos climáticos
foreach(i = 1:nrow(datos)) %do% {
  # Obtener los valores de la fila actual
  nlat <- datos[i, "nlat"]
  slat <- datos[i, "slat"]
  wlon <- datos[i, "wlon"]
  elon <- datos[i, "elon"]
  anio <- as.numeric(datos[i, "anio"])
  region <- datos[i, "region"]
  fecha <- datos[i, "fecha"]
  tx <- as.character(datos[i, "ID"])
  
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
      path = "era5data"       # almacenar los datos en el directorio de trabajo actual
    ),
    error = function(e) {
      message("Error descargando datos para la fila ", i, ": ", conditionMessage(e))
      return(NULL)
    }
  )
  
  # Listar todos los archivos .nc en la carpeta especificada
  nc_files <- list.files(path = "./GitHub/bird_sounds/files/era5data", pattern = "\\.nc$", full.names = TRUE)
  
  # Inicializar un data frame para almacenar los resultados
  resultados <- data.frame(ID = character(), fecha = character(), temperatura = numeric(), precipitacion = numeric()) 
  
  for (file in nc_files) {
    # Verificar si se obtuvo el archivo correctamente
    if (!is.null(file)) {
      # Abrir el archivo NetCDF
      nc <- nc_open(file)
      
      # Extraer las variables de interés
      temp_kelvin <- ncvar_get(nc, "t2m")  # Temperatura en Kelvin
      precipitacion <- ncvar_get(nc, "tp")  # Precipitación
      
      # Calcular la temperatura y precipitación promedio diaria
      temp_promedio <- apply(temp_kelvin - 273.15, c(1, 2), mean)
      temp_promedio_diaria <- mean(temp_promedio)
      
      precipitacion_total <- apply(precipitacion, c(1, 2), sum) * 1000  # Convertir a mm
      precipitacion_diaria <- mean(precipitacion_total)
      
      # Cerrar el archivo NetCDF
      nc_close(nc)
      
      # Obtener información del archivo para ID y fecha
      filename <- basename(file)
      info <- strsplit(filename, "_")
      if (length(info[[1]]) >= 3) {
        ID <- gsub(".nc", "", info[[1]][4])  # Suponiendo que el ID está en la cuarta posición del nombre del archivo
        fecha <- substr(info[[1]][3], 1, 4)  # Extrayendo el año de la tercera posición del nombre del archivo
      } else {
        warning(paste("No se pudo extraer información de ID y fecha para el archivo:", filename))
        next  # Saltar a la siguiente iteración del bucle
      }
      
      # Agregar los resultados al data frame de resultados
      resultados <- bind_rows(resultados, data.frame(
        ID = ID,
        fecha = fecha,
        temperatura = temp_promedio_diaria,
        precipitacion = precipitacion_diaria
      ))
    }
  }
  
resultados <- merge(resultados, altitudes, by = "ID")

write_csv(resultados, "climate_variables.csv")

# Imprimir los resultados
print(resultados)
  