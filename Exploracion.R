library(tidyverse)
library(lubridate)



madrid_2017 <- read.csv("csvs_per_year/madrid_2017.csv") %>% select(CO, date, station) %>% filter(!is.na(CO) %>% mutate (Mes = month(date))) %>% select(-station)



