library(tidyverse)

climate_variables <- climate_variables <- read_delim("climate_variables.csv", 
                                                     delim = ";", escape_double = FALSE, col_types = cols(date = col_date(format = "%d/%m/%Y")), 
                                                     trim_ws = TRUE)
                                
dataset <- read_csv("final_dataset.csv")
dataset <- dataset %>% drop_na()

summary(climate)

summary(dataset)

dataset$date <- as.Date(dataset$date, format = "%Y-%m-%d")
dataset$month <- lubridate::month(dataset$date)
dataset$year<- as.numeric(dataset$year)

region1<- dataset %>% filter(!region %in% c("Casanare", "Cesar", "Putumayo"))
region2<- dataset %>% filter(region %in% c("Casanare", "Cesar", "Putumayo"))

region1 <- region1 %>%
  mutate(quarter = case_when(
    month %in% c(12, 1, 2) ~ 1,
    month %in% c(3, 4, 5) ~ 2,
    month %in% c(6, 7, 8) ~ 3,
    month %in% c(9, 10, 11) ~ 4
  ))

prom_region <- region1 %>%
  group_by(quarter, year) %>%
  summarise(
    temperature= mean(temperature, na.rm = TRUE),
    max_temp= mean(max_temp, na.rm = TRUE),
    min_temp = mean(min_temp, na.rm = TRUE),
    precipitation = mean(precipitation, na.rm = TRUE)
  )

# Combinar los promedios con el conjunto de datos original
region1 <- region1 %>%
  left_join(prom_region, by = c("quarter", "year")) %>%   left_join(prom_region, by = c("quarter", "year")) %>%
  select(-matches("(.x)$"))

region1<- region1 %>% select(-matches("(.y)$"))


#Ahora haremos lo mismo para los datos de la región 2 (Cordoba, Cesar y Putumayo)

region2 <- region2 %>%
  mutate(season = case_when(
    month %in% c(12, 1, 2, 3) ~ 1,  # Temporada seca
    TRUE ~ 2  # Temporada lluviosa
  ))

# Subconjunto para temporada seca (meses 12, 1, 2, 3)
temporada_seca_region2 <- region2 %>% filter(season == 1) %>% 
  summarise(
    temperature = mean(temperature),
    precipitation = mean(precipitation),
    max_temp = mean(max_temp),
    min_temp = mean(min_temp)
  )

# Subconjunto para temporada lluviosa (resto de meses)
temporada_lluviosa_region2 <- region2 %>% filter(season == 2) %>% 
  summarise(
    temperature = mean(temperature),
    precipitation = mean(precipitation),
    max_temp = mean(max_temp),
    min_temp = mean(min_temp)
  )

# Asignar valores de temporada
temporada_seca_region2 <- temporada_seca_region2 %>% mutate(season = 1)
temporada_lluviosa_region2 <- temporada_lluviosa_region2 %>% mutate(season = 2)

# Función para actualizar las variables
update_variables_season <- function(region_data, season_data, season_number) {
  region_data <- region_data %>%
    mutate(
      temperature = ifelse(season == season_number, season_data$temperature, temperature),
      precipitation = ifelse(season == season_number, season_data$precipitation, precipitation),
      max_temp = ifelse(season == season_number, season_data$max_temp, max_temp),
      min_temp = ifelse(season == season_number, season_data$min_temp, min_temp)
    )
  return(region_data)
}

# Actualizar las variables en region2 con los datos de la temporada seca
region2 <- update_variables_season(region2, temporada_seca_region2, 1)

# Actualizar las variables en region2 con los datos de la temporada lluviosa
region2 <- update_variables_season(region2, temporada_lluviosa_region2, 2)

write.csv(region1,"region1.csv")
write.csv(region2,"region2.csv")


# ggplot(climate_variables, aes(x = temperature)) +
#   geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
#   labs(x = "Temperatura", y = "Frecuencia", title = "Distribución de la temperatura")
# 
# ggplot(climate_variables, aes(x = temperature, y = precipitation)) +
#   geom_point() +
#   labs(x = "Temperatura", y = "Precipitación", title = "Relación entre temperatura y precipitación")
# 
# table(climate_variables$region)
# 
# # Gráfico de barras para la variable 'region'
# ggplot(climate_variables, aes(x = region)) +
#   geom_bar(fill = "lightgreen") +
#   labs(x = "Región", y = "Frecuencia", title = "Distribución registros por región")+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
# 
# climate_variables$month <- lubridate::month(climate_variables$date)
# precipitacion_por_mes <- aggregate(precipitation ~ month, data = climate_variables, FUN = mean)
# 
# # Gráfico de barras de la precipitación por mes
# ggplot(precipitacion_por_mes, aes(x = factor(month), y = precipitation)) +
#   geom_bar(stat = "identity", fill = "skyblue") +
#   labs(x = "Mes", y = "Precipitación promedio", title = "Distribución de la precipitación por mes") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# 
# 
# 
# 
# temperatura_por_mes <- aggregate(temperature ~ month, data = climate_variables, FUN = mean)
# 
# # Gráfico de barras de la precipitación por mes
# ggplot(temperatura_por_mes, aes(x = factor(month), y = temperature)) +
#   geom_bar(stat = "identity", fill = "skyblue") +
#   labs(x = "Mes", y = "Temperatura promedio", title = "Distribución de la temperatura por mes") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# 
# climate_variables$date <- as.Date(climate_variables$date, format = "%Y-%m-%d")
# 
# # Crear el gráfico de series de tiempo
# ggplot(climate_variables, aes(x = date, y = temperature, color = region)) +
#   geom_line() +
#   labs(x = "Fecha", y = "Temperatura", title = "Variación de la temperatura por región") +
#   theme_minimal()
# 
# cundinamarca_data <- subset(climate_variables, region == "Cundinamarca")
# 
# # Convertir la columna 'fecha' a tipo Date si no lo está
# cundinamarca_data$date <- as.Date(cundinamarca_data$date, format = "%Y-%m-%d")
# 
# # Crear el gráfico de serie de tiempo para Cundinamarca
# ggplot(cundinamarca_data, aes(x = date, y = temperature)) +
#   geom_line() +
#   labs(x = "Fecha", y = "Temperatura", title = "Variación de la temperatura en Cundinamarca") +
#   theme_minimal()
# 
# climate_variables$month <- month(climate_variables$date)
# 
# # Crear el gráfico de series de tiempo por mes
# ggplot(climate_variables, aes(x = month, y = temperature)) +
#   geom_line() +
#   labs(x = "Mes", y = "Temperatura", title = "Variación de la temperatura por mes") +
#   theme_minimal()
# 
# climate_variables$month <- month(climate_variables$date)
# 
# # Crear el gráfico de barras de la precipitación por mes
# ggplot(climate_variables, aes(x = factor(month), y = precipitation)) +
#   geom_bar(stat = "identity", fill = "skyblue") +
#   labs(x = "Mes", y = "Precipitación", title = "Distribución de la precipitación por mes") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# ggplot(climate_variables, aes(x = factor(month), y = temperature)) +
#   geom_bar(stat = "identity", fill = "skyblue") +
#   labs(x = "Mes", y = "Precipitación", title = "Distribución de la temperatura por mes") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
