library(ggplot2)
library(tidyverse)
library(cowplot)
library(lubridate)
library(ggplot2)
library(factoextra)
library(lme4)
library(cowplot)
library(dlookr)
library(modelsummary)
library(tinytable)
library(gt)
library(car)

region1_transformed$season <- as.factor(region1_transformed$season)



# ANCOVA para low_freq_hz
ancova_low_freq_hz <- aov(low_freq_hz ~ season + pca_scores + elevation + precipitation, data = region1_transformed)
summary(ancova_low_freq_hz)

# ANCOVA para high_freq_hz
ancova_high_freq_hz <- aov(high_freq_hz ~ season + pca_scores + elevation + precipitation, data = region1_transformed)
summary(ancova_high_freq_hz)

# ANCOVA para delta_time_s
ancova_delta_time_s <- aov(delta_time_s ~ season + pca_scores + elevation + precipitation, data = region1_transformed)
summary(ancova_delta_time_s)

# ANCOVA para peak_freq_hz
ancova_peak_freq_hz <- aov(peak_freq_hz ~ season + pca_scores + elevation + precipitation, data = region1_transformed)
summary(ancova_peak_freq_hz)s

# ANCOVA para total_elements
ancova_total_elements <- aov(total_elements ~ season + pca_scores + elevation + precipitation, data = region1_transformed)
summary(ancova_total_elements)


# Función para verificar los supuestos de normalidad de los residuos
check_normality <- function(model) {
  # Obtener los residuos del modelo
  residuals <- residuals(model)
  
  # Crear un dataframe con los residuos
  df_resid <- data.frame(Residuals = residuals)
  
  # Gráfico QQ con ggplot2
  ggplot(df_resid, aes(sample = Residuals)) +
    geom_qq() +
    geom_qq_line() +
    labs(title = "Q-Q Plot de los Residuos")
  
  # Prueba de Shapiro-Wilk
  shapiro_test <- shapiro.test(residuals)
  print(shapiro_test)
}


# Aplicar a cada modelo ANCOVA
ancova_models <- list(
  ancova_low_freq_hz = ancova_low_freq_hz,
  ancova_high_freq_hz = ancova_high_freq_hz,
  ancova_delta_time_s = ancova_delta_time_s,
  ancova_peak_freq_hz = ancova_peak_freq_hz,
  ancova_total_elements = ancova_total_elements
)

# Verificar normalidad de los residuos para cada modelo

lapply(ancova_models, check_normality)
