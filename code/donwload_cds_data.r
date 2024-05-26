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
wf_set_key(user ="_", key = "key", service = "cds")

# Setear la clave de la API de Google Maps Elevation
api_key <- "key"

# Leer los datos del archivo CSV
datos <- read_delim("birdsongs_data.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

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
# Definir las variables a descargar
variables <- c('2m_temperature', 'maximum_2m_temperature_since_previous_post_processing', 'minimum_2m_temperature_since_previous_post_processing',
               'total_precipitation')

foreach(i = 1:nrow(datos)) %do% {
  # Obtener los valores de la fila actual
  nlat <- datos[i, "nlat"]
  slat <- datos[i, "slat"]
  wlon <- datos[i, "wlon"]
  elon <- datos[i, "elon"]
  anio <- as.numeric(datos[i, "anio"])
  region <- datos[i, "region"]
  fecha <- pull(datos[i, "fecha"])
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
}
  
library(ncdf4)
  # Listar todos los archivos .nc en la carpeta especificada
  nc_files <- list.files(path = "C:\\Users\\isabe\\OneDrive\\Documentos\\GitHub\\bird_sounds\\files\\era5data", pattern = "\\.nc$", full.names = TRUE)
  
  # Inicializar un data frame para almacenar los resultados
  resultados <- data.frame(ID = character(), year = character(), temperature = numeric(), precipitation = numeric(), max_temp=numeric(),min_temp=numeric() ) 
  
  for (file in nc_files) {
    # Verificar si se obtuvo el archivo correctamente
    if (!is.null(file)) {
      # Abrir el archivo NetCDF
      nc <- nc_open(file)
      
      # Extraer las variables de interés
      temp_kelvin <- ncvar_get(nc, "t2m")  # Temperatura en Kelvin
      precipitacion <- ncvar_get(nc, "tp")  # Precipitación
      temp_mn2t <- ncvar_get(nc, "mn2t")
      temp_mx2t <- ncvar_get(nc, "mx2t")
      
      
      # Calcular la temperatura y precipitación promedio diaria
      temp_mn2t_promedio <- apply(temp_mn2t - 273.15, c(1, 2), mean)
      temp_mn2t_promedio_diaria <- mean(temp_mn2t_promedio)
      
      temp_mx2t_promedio <- apply(temp_mx2t - 273.15, c(1, 2), mean)
      temp_mx2t_promedio_diaria <- mean(temp_mx2t_promedio)
      
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
        year = fecha,
        temperature = temp_promedio_diaria,
        precipitation = precipitacion_diaria,
        max_temp= temp_mx2t_promedio_diaria,
        min_temp=temp_mn2t_promedio_diaria 
      ))
    }
  }
  
  library(lubridate)

  fecha <- tibble(ID = character(), date = character(), region = character())
  
  foreach(i = 1:nrow(datos)) %do% {
    # Obtener los valores de la fila actual
    fecha_actual <- as.character(datos[i, "fecha"])  # Convert to character
    tx <- as.character(datos[i, "ID"])
    region <- as.character(datos[i, "region"])  # Close the parenthesis
    
    # Agregar la información al marco de datos 'fecha'
    fecha <- bind_rows(fecha, tibble(ID = tx, date = fecha_actual, region = region))
  }
  
  fecha <- fecha %>% mutate(date = dmy(date))

  
 
  resultados <- merge(resultados,altitudes)%>% rename(elevation=altitud)

  resultados <- merge(resultados, fecha)


  write_csv(resultados, "climate_variables.csv")


# Imprimir los resultados
View(resultados)
  
  