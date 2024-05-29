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


region1 <- region1 %>% mutate(season = ifelse(quarter %in% c(1, 3), 1, 2))  %>% arrange(season) %>% 
  mutate(pca_scores = air_temperature$Componente ) %>% drop_na()

region1$season <- factor(region1$season)

#Revisar la distribución de los datos 
region1_selected <- region1 %>%
  select(pca_scores, precipitation, elevation, high_freq_hz, low_freq_hz, peak_freq_hz, cad,delta_time_s, season)

plots <- map(names(region1_selected), ~ ggplot(region1, aes(x = .data[[.x]])) +
               geom_histogram(bins = 20, fill = "orange", color = "black") +
               labs(title = paste("Histogram of", .x), x = .x, y = "Frequency") +
               theme_minimal())


plot_1<-plot_grid(plotlist = plots, ncol = 2) 

dlookr::find_skewness(region1_selected, value = TRUE,thres = 0.1)
dlookr::find_outliers(region1_selected)

region2_selected <- region2 %>%
                            select(pca_scores, precipitation, elevation, high_freq_hz, low_freq_hz, peak_freq_hz, cad, delta_time_s,season)

plots2 <- map(names(region2_selected), ~ ggplot(region2, aes(x = .data[[.x]])) +
               geom_histogram(bins = 20, fill = "blue", color = "black") +
               labs(title = paste("Histogram of", .x), x = .x, y = "Frequency") +
               theme_minimal())

plot_2<-plot_grid(plotlist = plots2, ncol = 2) 

find_skewness(region2_selected, value = TRUE)
find_outliers(region2_selected)


#Modelo para region 1



glm_gaussian1 <- lmer(low_freq_hz + high_freq_hz + cad + delta_time_s + peak_freq_hz ~ pca_scores + elevation + precipitation + (1 | season),
                     data = region1)
glm_gaussian1b <- lmer(low_freq_hz + high_freq_hz + cad + delta_time_s + peak_freq_hz ~ pca_scores + elevation + precipitation + (1 | season),
                      data = region1,
                      control = lmerControl(optimizer = "bobyqa")
                      )

summary(glm_gaussian1)


#modelo para region 2

region2$season <- factor(region2$season)


glm_gaussian2 <- lmer(low_freq_hz + high_freq_hz + cad + delta_time_s + peak_freq_hz ~ pca_scores + precipitation + (1 | season),
                     data = region2)
glm_gaussian2b <- lmer(low_freq_hz + high_freq_hz + cad + delta_time_s + peak_freq_hz ~ pca_scores + precipitation + (1 | season),
                      data = region2,
                      control = lmerControl(optimizer = "bobyqa"))



summary(glm_gaussian2)

#Indices Region 1
# AIC(glm_gaussian1)
# 36293.84
#AIC(glm_gaussian1_transformed)
# 30451.99
#AIC(glm_gaussian1_standardized)
# 11868.68

#Indices Region 2
#AIC(glm_gaussian2)
#3831.223
#AIC(glm_gaussian2_transformed)
#728.953
#AIC(glm_gaussian2_standardized)
#1185.98

#Transformar variables de respuesta

#region1



region1_transformed <- region1_selected %>%
  mutate(
    precipitation_log = transform(precipitation, method = "log+1"),
    high_freq_hz_sq = transform(high_freq_hz, method = "x^2"),
    low_freq_hz_inv = transform(low_freq_hz, method = "1/x"),
    cad_log = transform(cad, method = "log+1"),
    elevation_bc = transform(elevation, method = "Box-Cox"),
    delta_time_s_log = transform(delta_time_s, method = "log+1")) %>% 
  mutate(across(c(precipitation_log, high_freq_hz_sq, low_freq_hz_inv, cad_log, elevation_bc, delta_time_s_log), scale))

  

  

plotst<- map(names(region1_transformed), ~ ggplot(region1_transformed, aes(x = .data[[.x]])) +
               geom_histogram(bins = 20, fill = "orange", color = "black") +
               labs(title = paste("Histogram of", .x), x = .x, y = "Frequency") +
               theme_minimal())


plot_5<-plot_grid(plotlist = plotst, ncol = 2) 
 
#modelo con variables transformadas 
glm_gaussian1_transformed <- lmer(
  low_freq_hz_inv + high_freq_hz_sq + cad_log + delta_time_s_log + peak_freq_hz ~ 
    pca_scores + elevation_bc + precipitation_log + (1 | season),
  data = region1_transformed,
  control = lmerControl(optimizer = "bobyqa")
)

summary(glm_gaussian1_transformed)

#modelo con variables estandarizadas

region1_standardized <- region1 %>%
  select(low_freq_hz, high_freq_hz, cad, delta_time_s, peak_freq_hz, pca_scores, elevation, precipitation) %>%
  scale()


region1_combined <- cbind(region1_standardized, season = region1$season) %>% as.data.frame()

# Ajustar el modelo lineal mixto con variables estandarizadas
glm_gaussian1_standardized <- lmer(
  low_freq_hz + high_freq_hz + cad + delta_time_s + peak_freq_hz ~ 
    pca_scores + elevation + precipitation + (1 | season),
  data = region1_combined,
  control = lmerControl(optimizer = "bobyqa")  # Cambiar el algoritmo de optimización a "bobyqa"
)

# Resumen del modelo
summary(glm_gaussian1_standardized)






#region2



region2_transformed <- region2_selected %>%
  mutate(
    precipitation_log = transform(precipitation, method = "log+1"),
    low_freq_hz_inv = transform(low_freq_hz, method = "1/x"),
    delta_time_s_log = transform(delta_time_s, method = "log+1")) %>%
  mutate(across(c(precipitation_log, high_freq_hz, low_freq_hz_inv, cad, elevation, delta_time_s_log), scale))






plotst<- map(names(region2_transformed), ~ ggplot(region1_transformed, aes(x = .data[[.x]])) +
               geom_histogram(bins = 20, fill = "blue", color = "black") +
               labs(title = paste("Histogram of", .x), x = .x, y = "Frequency") +
               theme_minimal())


plot_6<-plot_grid(plotlist = plotst, ncol = 2) 

#modelo con variables transformadas 
glm_gaussian2_transformed <- lmer(
  low_freq_hz + high_freq_hz + cad + delta_time_s + peak_freq_hz + low_freq_hz_inv  ~ 
    pca_scores + precipitation_log +(1 | season),
  data = region2_transformed,
  control = lmerControl(optimizer = "bobyqa")  
)


summary(glm_gaussian2_transformed)

region2_standardized <- region2 %>%
  select(low_freq_hz, high_freq_hz, cad, delta_time_s, peak_freq_hz, pca_scores, elevation, precipitation) %>%
  scale()


region2_combined <- cbind(region2_standardized, season = region2$season) %>% as.data.frame()

# Ajustar el modelo lineal mixto con variables estandarizadas
glm_gaussian2_standardized <- lmer(
  low_freq_hz + high_freq_hz + cad + delta_time_s + peak_freq_hz ~ 
    pca_scores + precipitation + (1 | season),
  data = region2_combined,
  control = lmerControl(optimizer = "bobyqa")  # Cambiar el algoritmo de optimización a "bobyqa"
)

# Resumen del modelo
summary(glm_gaussian2_standardized)

aic_results <- tribble(
  ~Región, ~Modelo, ~AIC,
  "Región 1", "Sin transformar", 36293.84,
  "Región 1", "Transformado", 30451.99,
  "Región 1", "Estandarizado", 11868.68,
  "Región 2", "Sin transformar", 3831.223,
  "Región 2", "Transformado", 3822.686,
  "Región 2", "Estandarizado", 1185.98
)

print(aic_results)

panels <- list(
  "Region 1" = list(
    "(I)" = glm_gaussian1,
    "(II)" = glm_gaussian1b,
    "(III)" = glm_gaussian1_standardized,
    "(IV)" = glm_gaussian1_transformed
  ),
  "Region 2" = list(
    "(I)" = glm_gaussian2,
    "(II)" = glm_gaussian2b,
    "(III)" = glm_gaussian2_standardized,
    "(IV)" = glm_gaussian2_transformed
  )
)
models<- list(
  "(I)" = glm_gaussian1,
  "(II)" = glm_gaussian1b,
  "(III)" = glm_gaussian1_standardized,
  "(IV)" = glm_gaussian1_transformed,"(I)" = glm_gaussian2,
  "(II)" = glm_gaussian2b,
  "(III)" = glm_gaussian2_standardized,
  "(IV)" = glm_gaussian2_transformed)


tab<-modelsummary(
  models,
  fmt = fmt_significant(2),
  statistic = c("{std.error}"),
  stars = TRUE, 
  coef_rename = c(
    "pca_scores" = "Air temperature (C)", 
    "elevation" = "Elevation (amsl)",
    "precipitation" = "Precipitation (mm)", 
    "elevation_bc" = "Elevation (Box-Cox)",
    "precipitation_log" = "Precipitation (Log+1)"
  ),  output = "gt", gof_map = c("aic", "bic")
  
)


tab %>% tab_spanner(label = 'Region 1', columns = 2:5) %>%  tab_spanner(label = 'Region 2', columns = 6:9) 
  

region2<- region2 %>% rename("air_temperature"='pca_scores')
region2_selected<- region2_selected %>% rename("air_temperature"='pca_scores')
region2_selected<- data.frame(region2_selected) %>%
  mutate(region = "region 2")
region1_selected<- data.frame(region1_selected) %>%
  mutate(region = "region 1")



datasummary_balance(~season,
                    data = region2)

datasummary_correlation(region2_selected, method = 'spearman',output = 'gt')

tt(region2_selected)
datasummary(All(region1_selected) ~ season *(Mean + SD + N + Var),
            data = region1_selected, title = 'Statistics of Region 1',
            )

datasummary_skim(region2_selected, 
                type = "numeric", 
                by = 'season',
                fun_numeric = list(
                  Unique = NUnique, 
                  Mean = Mean, 
                  SD = SD, 
                  Min = Min, 
                  Max = Max, 
                  Histogram = function(x) NULL
                ),
                output = "gt"
)


  
  
  

modelplot(models, coef_omit = 'Interc') 
