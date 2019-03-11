rm(list = ls())
setwd('~/Desktop/19W/qss_30/project/')

library(tidyverse)
library(jtools)
library(MASS)
library(stargazer)
library(cowplot)
library(knitr)

ucr <- read_csv('./data/ucr_merged_clean.csv')
disp <- read_csv('./data/disp_merged_states_clean_all.csv')
fe <- read_csv('./data/fatal_encounters_clean.csv')
controls <- read_csv('./data/controls_cleaner.csv')
merged <- read_csv('./data/analysis_ready.csv')

# Fatal encounters
# Deaths over time by race, counts
# Top 3 race-counts only
pdf('./writing/figures/fe_race_timeseries.pdf')
fe %>%
  rename(race = `Subject's race`,
         year = `Date (Year)`) %>%
  filter(
    race == "African-American/Black" |
      race == "European-American/White" | race == "Hispanic/Latino"
  ) %>%
  mutate(race = ifelse(
    race == "African-American/Black",
    "Black",
    ifelse(race == "European-American/White", "White", "Hispanic")
  )) %>%
  group_by(race, year) %>%
  summarize(deaths = n()) %>%
  ggplot(., aes(x = year, y = deaths, linetype = race)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(2000, 2019),
                     breaks = seq(2000, 2020, 2)) +
  scale_y_continuous(limits = c(0, 800),
                     breaks = seq(0, 800, 100)) +
  labs(y = "Police Killings", x = "\nYear") +
  scale_linetype_manual(
    name = "Race",
    guide = guide_legend(direction = "horizontal",
                         title.position = "top"),
    breaks = c("White", "Black", "Hispanic"),
    values = c("longdash", "dotted", "solid")
  ) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.key.width = unit(1.5, "cm"),
    legend.position = "bottom",
    legend.title.align = 0.5,
    legend.justification = "center",
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black")
  )
dev.off()

# Fatal encounters
# Table
# Deaths over time by race, counts
# Top 3 race-counts only
fe %>%
  rename(Race = `Subject's race`,
         year = `Date (Year)`) %>%
  filter(
    Race == "African-American/Black" |
      Race == "European-American/White" |
      Race == "Hispanic/Latino" |
      Race == "Native American/Alaskan" |
      Race == "Asian/Pacific Islander" |
      Race == "Middle Eastern"
  ) %>%
  group_by(Race) %>%
  summarize(Deaths = n()) %>%
  ungroup() %>%
  mutate(`% of Total` = 100.0 * round(Deaths / sum(Deaths), 3)) %>%
  arrange(desc(Deaths)) %>%
  kable("latex", booktabs = T)

# DISP
pdf('./writing/figures/leso_timeseries.pdf')
disp %>%
  filter(group_small != "Unavailable") %>%
  mutate(Controlled = ifelse(group_small == "Weapons" | group_small == "Vehicular", "Controlled", "Uncontrolled")) %>% 
  mutate(Year = as.numeric(substring(`Ship Date`, 1, 4)),
         Cost = `Acquisition Value` * Quantity,
         Cost = ifelse(is.na(Cost), 0, Cost)) %>%
  group_by(Year, Controlled) %>%
  summarize(total_cost = sum(Cost)/1000000) %>%
  ggplot(., aes(x = Year, y = total_cost, linetype = Controlled)) +
  scale_x_continuous(limits = c(1990, 2018),
                     breaks = seq(1990, 2018, 2)) +
  geom_line(size = 1) +
  labs(y = "Total Cost ($M)", x = "\nYear") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.key.width = unit(1.5, "cm"),
    legend.position = "bottom",
    legend.title.align = 0.5,
    legend.justification = "center",
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black")
  )
dev.off()

disp %>%
  filter(group_small != "Unavailable") %>%
  na.omit() %>%
  group_by(group_small) %>%
  mutate(cost = `Acquisition Value` * Quantity) %>% 
  summarize(
    Cost = sum(cost),
    Value = sum(`Acquisition Value`),
    Quantity = sum(Quantity),
  ) %>%
  ungroup() %>%
  mutate(
    `Cost (%)` = 100 * round(Cost / sum(Cost), 3),
    `Value (%)` = 100 * round(Value / sum(Value), 3),
    `Quantity (%)` = 100 * round(Quantity / sum(Quantity), 3)
  ) %>%
  arrange(desc(Cost)) %>%
  kable("latex", booktabs = T)

pdf('./writing/figures/disp_density.pdf')
disp %>%
  filter(group_small != "Unavailable") %>%
  na.omit() %>%
  group_by(group_small) %>%
  mutate(cost = `Acquisition Value` * Quantity,
         cost = cost / 1000000) %>% 
  filter(quantile(cost, 0.95) < cost) %>% 
  ggplot(., aes(x = cost, y=..scaled..)) + 
  geom_density() +
  labs(x = "Total Cost ($M)", 
       y = "Scaled Density")
dev.off()

# Crime rates table
ucr %>%
  group_by(year) %>%
  na.omit() %>%
  summarise(violent_crime = sum(violent_crime)) %>%
  ungroup() %>%
  arrange(year) %>%
  mutate(
    violent_crime_lag = lag(violent_crime),
    violent_crime_change = 100.0 * round((violent_crime - violent_crime_lag) / violent_crime, 3)
  ) %>%
  dplyr::select(year, violent_crime, violent_crime_change) %>%
  rename(
    Year = year,
    `Violent Crime` = violent_crime,
    `Change (%)` = violent_crime_change
  ) %>%
  kable("latex", booktabs = T)

