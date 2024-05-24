library(tidyverse)

dataset<-read_delim("final_data.csv", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(dataset)
