library(tidyverse)

setwd("C:/Users/isabe/OneDrive/Documentos/GitHub/bird_sounds/files")

climate<-read_csv('climate_variables.csv')
singings<-read_delim("birdsongs_variables.csv", 
                                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

singings <- singings %>% 
  filter(View == 'Spectrogram 1') %>%
  group_by(ID) %>% mutate(canto=as.character(canto)) %>% 
  mutate(`total elementos` = as.character(if_else(is.na(`total elementos`), "0", as.character(`total elementos`)))
  ) %>%  select(4:which(names(.) == "ID")) %>%   group_by(`tipo de nota`, ID) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")

data<- merge(climate, singings)

write_csv(data, "final_data.csv")

dataset<-read_delim("final_data.csv", 
                                  delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(dataset)
