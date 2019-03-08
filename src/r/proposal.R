rm(list = ls()) 
setwd('~/Desktop/19W/qss_30/project/')

library(tidyverse)
library(scales)

df <- read_csv("1033.csv")
theme_set(theme_bw())
options(scipen=10000)

pdf("proposal_plot.pdf") 
df %>% 
  mutate(year = format(as.Date(`Ship Date`, format = "%m/%d/%y"), "%Y")) %>% 
  group_by(year) %>% 
  summarize(total_expenditures = sum(`Acquisition Value`)) %>% 
  ggplot(., aes(x = year, y = total_expenditures, group = 1)) +
  geom_point() +
  geom_line() + 
  labs(title = "1033 Program Expenditures by Year",
       y = "Expenditures ($)", x = "Year") +
  scale_y_continuous(label=comma) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
dev.off()
