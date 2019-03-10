rm(list = ls())
setwd('~/Desktop/19W/qss_30/project/')

library(tidyverse)
library(knitr)

disp <- read_csv('./data/disp_merged_states_clean_all.csv')
disp %>% 
  group_by(`DEMIL Code`) %>% 
  na.omit() %>% 
  summarize(count = n(),
            spend = round(sum(`Acquisition Value`)/1000),
            quant = sum(Quantity)) %>% 
  arrange(desc(count)) %>% 
  kable()
