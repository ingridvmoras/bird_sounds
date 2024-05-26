library(tidyverse)

setwd("C:/Users/isabe/OneDrive/Documentos/GitHub/bird_sounds/files")


climate<- read_delim("climate_variables.csv", 
                                delim = ";", escape_double = FALSE, col_types = cols(date = col_date(format = "%d/%m/%Y")), 
                                trim_ws = TRUE)

singings<-read_delim("birdsongs_variables.csv", 
                                            delim = ";", escape_double = FALSE, trim_ws = TRUE)


cad <- singings %>%  filter(View == 'Spectrogram 1') %>%
  filter(`note` == "cad") %>%
  select(`Delta Time (s)`, singing, ID) %>% rename(cad=`Delta Time (s)` ) 

singings <- singings %>%
  filter(View == 'Spectrogram 1') %>%
  filter(note %in% c('a', 't', 'd')) %>%  select(6:13)



data<- merge(climate, singings)


dataset<-merge(data,cad) %>% rename(
  low_freq_hz = `Low Freq (Hz)`,
  high_freq_hz = `High Freq (Hz)`,
  delta_time_s = `Delta Time (s)`,
  peak_freq_hz = `Peak Freq (Hz)`)



write_csv(dataset, "final_dataset.csv")


View(dataset)
