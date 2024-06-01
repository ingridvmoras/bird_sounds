library(ggplot2)
library(tidyverse)
library(cowplot)
library(lubridate)
library(ggplot2)
library(factoextra)
library(broom)
library(lme4)

climate <- climate_variables <- read_delim("climate_variables.csv", 
                                                     delim = ";", escape_double = FALSE, col_types = cols(date = col_date(format = "%d/%m/%Y")), 
                                                     trim_ws = TRUE)
                                
dataset <- read_csv("final_dataset.csv")
dataset <- dataset %>% drop_na()
cadencia<- read_csv("cad_dataset.csv")

summary(climate)

summary(dataset)

dataset$date <- as.Date(dataset$date, format = "%Y-%m-%d")
dataset$month <- lubridate::month(dataset$date)
dataset$year<- as.numeric(dataset$year)

cadencia<-merge(cadencia,climate)

cadencia$date <- as.Date(cadencia$date, format = "%Y-%m-%d")
cadencia$month <- lubridate::month(cadencia$date)
cadencia$year<- lubridate::year(cadencia$date)

region1<- dataset %>% filter(!region %in% c("Casanare", "Cesar", "Putumayo"))
region2<- dataset %>% filter(region %in% c("Casanare", "Cesar", "Putumayo"))

r1<- cadencia %>% filter(!region %in% c("Casanare", "Cesar", "Putumayo"))%>%
  mutate(season = case_when(
    month %in% c(12, 1, 2,6,7,8) ~ 1,
    month %in% c(3, 4, 5,9,10,11) ~ 2) )

r2<- cadencia %>% filter(region %in% c("Casanare", "Cesar", "Putumayo"))%>% mutate(season = case_when(
  month %in% c(12, 1, 2,3) ~ 1,
  month %in% c(4:11) ~ 2))




region1<- region1 %>% select(-matches("(.y)$"))


region1 <- region1 %>%
  mutate(quarter = case_when(
    month %in% c(12, 1, 2) ~ 1,
    month %in% c(3, 4, 5) ~ 2,
    month %in% c(6, 7, 8) ~ 3,
    month %in% c(9, 10, 11) ~ 4
  ))

prom_region1 <- region1 %>%
  group_by(year, quarter) %>%
  summarise(
    temperature = mean(temperature, na.rm = TRUE),
    max_temp = mean(max_temp, na.rm = TRUE),
    min_temp = mean(min_temp, na.rm = TRUE),
    precipitation = mean(precipitation, na.rm = TRUE)
  ) %>%
  ungroup()


# Combinar los promedios con el conjunto de datos original
region1 <- region1 %>%
  left_join(prom_region1, by = c("quarter", "year")) %>%   
  select(-matches("(.x)$"))
  

region1<- region1 %>% select(-matches("(.y)$"))



#Exploracion de los datos 


region1$quarter <- as.factor(region1$quarter)
region1$year <- as.factor(region1$year)
region1 <- region1 %>%
  mutate(season = ifelse(quarter %in% c(1, 3), "dry_season", "wet_season")) %>% arrange(season)

summary_stats1 <- region1 %>%
  group_by(quarter, year, singing) %>%
  summarise(num_unique_IDs = n_distinct(ID)) %>%
  summarise(total_unique_signings = sum(num_unique_IDs))


summary1 <- region1 %>%
  group_by(season, year, singing) %>%
  summarise(num_unique_IDs = n_distinct(ID)) %>%
  summarise(total_unique_signings = sum(num_unique_IDs))
summary_stats1$total_unique_signings <- as.numeric(summary_stats1$total_unique_signings)

plot1 <- ggplot(summary_stats1, aes(x = quarter, y = year, fill = total_unique_signings)) +
  geom_tile() +
  scale_fill_distiller(palette = "YlOrRd", direction = 1) +  # Reverse the color scale
  labs(title = "Number of Signings per Quarter-Year") +
  theme_minimal()


plot2 <- ggplot(summary1, aes(x = season, y = year, fill = total_unique_signings)) +
  geom_tile() +
  scale_fill_distiller(palette = "YlOrRd", direction = 1) + 
  labs(title = "Number of Signings per Season-Year") +
  theme_minimal()


plot_1<-plot_grid(plot1, plot2, labels = "AUTO")
print(plot_1)

summary_stats11 <- region1 %>%
  group_by(quarter, singing) %>%
  summarise(num_unique_IDs = n_distinct(ID)) %>%
  summarise(total_unique_signings = sum(num_unique_IDs))


summary11 <- region1 %>%
  group_by(season, singing) %>%
  summarise(num_unique_IDs = n_distinct(ID)) %>%
  summarise(total_unique_signings = sum(num_unique_IDs))



plot_quarter_singing <- ggplot(summary_stats1, aes(x = quarter, y = total_unique_signings, fill = quarter)) +
  geom_boxplot(linetype = "solid", coef = 1.5) + 
  scale_fill_brewer(palette = "YlOrRd", name = "Quarter") +# Líneas punteadas
  labs(title = "Singings per Quarter-Singing", x = "Quarter", y = "Number of Singings") +
  theme_minimal()   # Remove legend


plot_season_singing <- ggplot(summary1, aes(x = season, y = total_unique_signings, fill = season)) +
  geom_boxplot(linetype = "solid", coef = 1.5) +
  scale_fill_brewer(palette = "YlOrRd", name = "Season") +# Líneas punteadas
  labs(title = "Signings per Season-Singing", x = "Season", y = "Number of Singings") +
  theme_minimal() 


plot_2 <- plot_grid(plot_quarter_singing, plot_season_singing, nrow = 2)

print(plot_2)

pdf(paste0("C:\\Users\\isabe\\OneDrive\\Documentos\\GitHub\\bird_sounds\\results\\figures",Sys.Date(), "_region1_exploratory.pdf"), width = 9.5, height = 6.25)


plot(plot_1)
plot(plot_2)
plot(plot_3)

# Termina el dispositivo de gráficos PDF
dev.off()
 
#PCA variables climaticas 


datos_seca <- region1 %>%
  filter(season == "dry_season") %>%
  select(temperature, min_temp, max_temp)

datos_humedo <- region1 %>%
  filter(season == "wet_season") %>%
  select(temperature, min_temp, max_temp)

library(FactoMineR)

pca_seca <- PCA(X = datos_seca, scale.unit = TRUE, graph = TRUE)

fviz_pca_var(pca_seca, alpha.var="contrib")+
  theme_minimal()

plot(pca_seca, choix="ind")

pca_humedo <- PCA(X = datos_humedo, scale.unit = TRUE, graph = TRUE)

fviz_screeplot(pca_humedo, ncp=3)

fviz_pca_var(pca_humedo, alpha.var="contrib")+
  theme_minimal()
summary(pca_seca)

summary(pca_humedo)


temp_diaria_seca <- pca_seca$ind$coord[, 1]
temp_diaria_humedo <- pca_humedo$ind$coord[, 1]


air_temperature <- tibble(
  Componente = c(temp_diaria_seca, temp_diaria_humedo),
  season = c(rep("dry_season", length(temp_diaria_seca)), rep("wet_season", length(temp_diaria_humedo)))
)

air_temperature<-air_temperature %>% arrange(season)


plot_3<- ggplot(air_temperature , aes(x = season, y = Componente, fill = season)) +
  geom_boxplot() +
  labs(x = "Season", y = "Principal Component (Air temperature)") +   scale_fill_brewer(palette = "YlOrRd")+
  theme_minimal()

# Mostrar el boxplot
print(plot_3)


correlation_matrix <- cor.test(air_temperature$Componente, region1$elevation, method = "spearman")

correlation_matrix2 <- cor.test(air_temperature$Componente, region1$precipitation, method = "spearman")

# Verificar la correlación entre temperatura del aire y precipitación 


correlation <- correlation_matrix$estimate #-0.5475581 

correlation2 <- correlation_matrix2$estimate #-0.03770287 

#Estadisticos de tendencia central por temporada 
selected_vars <- c('low_freq_hz','high_freq_hz','peak_freq_hz','delta_time_s','quarter','total_elements')


region1 <- mutate_at(region1, selected_vars, as.numeric)

# Calcula las estadísticas descriptivas 

summary_statd <- summary(dry)
summary_statw <- summary(wet)


#Ahora haremos lo mismo para los datos de la región 2 (Cordoba, Cesar y Putumayo)

region2 <- region2 %>%
  mutate(season = case_when(
    month %in% c(12, 1, 2,3) ~ 1,
    month %in% c(4:11) ~ 2,
  ))

prom_region2 <- region2 %>%
  group_by(season, year) %>%
  summarise(
    temperature= mean(temperature, na.rm = TRUE),
    max_temp= mean(max_temp, na.rm = TRUE),
    min_temp = mean(min_temp, na.rm = TRUE),
    precipitation = mean(precipitation, na.rm = TRUE)
  )

# Combinar los promedios con el conjunto de datos original
region2 <- region2 %>%
  left_join(prom_region2, by = c("season", "year")) %>% 
  select(-matches("(.x)$"))

region2<- region2 %>% rename_with(~ gsub("\\.y$", "", .x), ends_with(".y")) %>% arrange(season)


summary2 <- region2 %>%
  group_by(season, year, singing) %>%
  summarise(num_unique_IDs = n_distinct(ID)) %>%
  summarise(total_unique_signings = sum(num_unique_IDs))

barplot <- ggplot(summary2, aes(x = factor(year), y = total_unique_signings, fill = factor(season))) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(title = "Number of Signings per Year", x = "Year", y = "Number of Signings", fill = "Season") +
  theme_minimal() +scale_fill_brewer(palette = "Paired")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

print(barplot)


summary21 <- region2 %>%
  group_by(season, singing) %>%
  summarise(num_unique_IDs = n_distinct(ID)) %>%
  summarise(total_unique_signings = sum(num_unique_IDs))

barplot2 <- ggplot(summary21, aes(x = factor(season), y = factor(total_unique_signings), fill = factor(season))) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(title = "Signings per Season-Singing", x = "Season", y = "Number of Signings", fill = "Season") +
  theme_minimal() +scale_fill_brewer(palette = "Paired")



plot_4 <- plot_grid(barplot, barplot2, nrow = 2)

print(plot_4)

pdf(paste0("C:\\Users\\isabe\\OneDrive\\Documentos\\GitHub\\bird_sounds\\results\\figures",Sys.Date(), "_region2_exploratory.pdf"), width = 9.5, height = 6.25)


plot(plot_4)
plot(plot_5)


# Termina el dispositivo de gráficos PDF
dev.off()
 
#PCA variables climaticas 


datos_seca <- region2 %>%
  filter(season == 1) %>%   
  select(temperature, min_temp, max_temp)

datos_humedo <- region2 %>%
  filter(season == 2) %>%
  select(temperature, min_temp, max_temp)

library(FactoMineR)

pca_seca <- PCA(X = datos_seca, scale.unit = TRUE, graph = TRUE)

fviz_pca_var(pca_seca, alpha.var="contrib")+
  theme_minimal()

plot(pca_seca, choix="ind")

pca_humedo <- PCA(X = datos_humedo, scale.unit = TRUE, graph = TRUE)

fviz_screeplot(pca_humedo, ncp=3)
fviz_pca_var(pca_humedo, alpha.var="contrib")+
  theme_minimal()
summary(pca_seca)

summary(pca_humedo)


temp_diaria_seca <- pca_seca$ind$coord[, 1]
temp_diaria_humedo <- pca_humedo$ind$coord[, 1]


air_temperature <- tibble(
  Componente = c(temp_diaria_seca, temp_diaria_humedo),
  season = c(rep("dry_season", length(temp_diaria_seca)), rep("wet_season", length(temp_diaria_humedo)))
)

air_temperature<-air_temperature %>% arrange(season)


plot_5<- ggplot(air_temperature , aes(x = season, y = Componente, fill = season)) +
  geom_boxplot() +
  labs(x = "Season", y = "Principal Component (Air temperature)") +   scale_fill_brewer(palette = "YlOrRd")+
  theme_minimal() +scale_fill_brewer(palette = "Paired")

# Mostrar el boxplot
print(plot_5)


correlation_matrix <- cor.test(air_temperature$Componente, region2$elevation, method = "spearman")

correlation_matrix2 <- cor.test(air_temperature$Componente, region2$precipitation, method = "spearman")

# Verificar la correlación entre temperatura del aire y precipitación 


correlation <- correlation_matrix$estimate #-0.7515965 

correlation2 <- correlation_matrix2$estimate #-0.06647962

#Estadisticos de tendencia central por temporada 
selected_vars <- c('low_freq_hz','high_freq_hz','peak_freq_hz','delta_time_s')

region2 <- mutate_at(region2, selected_vars, as.numeric)

# Calcula las estadísticas descriptivas 

region2 <- region2 %>% arrange(season) %>% 
  mutate(pca_scores = air_temperature$Componente ) %>% drop_na()

summary_statd <- summary(filter(region2, region2$season==1))
summary_statw <- summary(filter(region2, region2$season==2))
