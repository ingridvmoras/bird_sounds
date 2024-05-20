library(tidyverse)

# Definir el conjunto fijo de columnas
columnas_fijas <- c("Selection", "View", "Channel", "Begin Time (s)", 
                    "End Time (s)", "Low Freq (Hz)", "High Freq (Hz)", 
                    "Delta Time (s)", "Peak Freq (Hz)", "total elementos", 
                    "tipo de nota", "canto", "nombre archivo")

# Función para leer cada archivo y ajustar las columnas según el conjunto fijo
read_file_and_adjust <- function(file_path) {
  data <- read_delim(file_path, delim = "\t", col_types = cols(.default = "c")) # Leer todas las columnas como character
  # Reordenar y ajustar las columnas según el conjunto fijo
  data <- data %>%
    select(all_of(columnas_fijas))
  # Agregar nombre del archivo como una columna
  data$nombre_archivo <- basename(file_path)
  return(data)
}

# Leer y combinar los datos de todos los archivos
datos_completos <- map_dfr(list.files(path = "~/GitHub/bird_sounds/files/birdsongs_variables", pattern = "\\.txt$", full.names = TRUE),
                           ~ tryCatch({
                             read_file_and_adjust(.x)
                           }, error = function(e) {
                             message(paste("Error en el archivo:", basename(.x), ":", conditionMessage(e)))
                             return(NULL)
                           }))

# Escribir a CSV
write_csv(datos_completos, "birdsongs_variables.csv")

