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

# Fatal encounters for descriptive stats
fe_descriptive <- fe %>%
  dplyr::select(
    `Subject's gender`,
    `Subject's race`,
    `Subject's age`,
    `Date (Year)`,
    `Cause of death`,
    `Location of death (zip code)`
  ) %>%
  na.omit()
stargazer(fe_descriptive)

# Fatal encounters
# Deaths over time by race, counts
# Top 3 race-counts only
fe %>%
  rename(race = `Subject's race`,
         year = `Date (Year)`) %>%
  filter(
    race == "African-American/Black" |
      race == "European-American/White" | race == "Hispanic/Latino"
  ) %>%
  group_by(race, year) %>%
  summarize(deaths = n()) %>%
  ggplot(., aes(x = year, y = deaths, linetype = race)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(2000, 2019),
                     breaks = seq(2000, 2020, 2)) +
  scale_y_continuous(limits = c(0, 800),
                     breaks = seq(0, 800, 100)) +
  labs(y = "Fatalities", x = "Year")

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
  kable()

# Fatal encounters
# Deaths over time by cause, counts
# Top 3 causes only
fe %>%
  rename(Cause = `Cause of death`,
         year = `Date (Year)`) %>%
  filter(Cause == "Gunshot" |
           Cause == "Vehicle" | Cause == "Tasered") %>%
  group_by(cause, year) %>%
  summarize(deaths = n()) %>%
  ggplot(., aes(x = year, y = deaths, linetype = Cause)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(2000, 2019),
                     breaks = seq(2000, 2020, 2)) +
  scale_y_continuous(limits = c(0, 1400),
                     breaks = seq(0, 1400, 200)) +
  labs(y = "Fatalities", x = "Year")

# Fatal encounters
# Deaths over time by cause, counts
fe %>%
  rename(Cause = `Cause of death`) %>%
  group_by(Cause) %>%
  summarize(Deaths = n()) %>%
  ungroup() %>%
  mutate(`% of Total` = 100.0 * round(Deaths / sum(Deaths), 3)) %>%
  arrange(desc(Deaths)) %>%
  kable()

# fe %>%
#   rename(Race = `Subject's race`,
#          Cause = `Cause of death`,
#          Year = `Date (Year)`) %>%
#   filter(Cause == "Gunshot" | Cause == "Vehicle" | Cause == "Tasered") %>%
#   filter(
#     Race == "African-American/Black" |
#       Race == "European-American/White" |
#       Race == "Hispanic/Latino"
#   ) %>%
#   group_by(Race, Cause, Year) %>%
#   summarize(Deaths = n()) %>%
#   ungroup() %>%
#   ggplot(., aes(x = Year, y = Deaths)) +
#   geom_bar(stat = "identity") +
#   facet_wrap(. ~ Race, nrow = 1)

# DISP
disp %>%
  filter(group_small != "Unavailable") %>%
  mutate(Year = as.numeric(substring(`Ship Date`, 1, 4))) %>%
  group_by(Year, group_small) %>%
  summarize(expenditure_value = sum(`Acquisition Value`)) %>%
  ggplot(., aes(x = Year, y = expenditure_value, color = group_small)) +
  scale_x_continuous(limits = c(1990, 2018),
                     breaks = seq(1990, 2018, 2)) +
  geom_line(size = 1)

disp %>%
  filter(group_small != "Unavailable") %>%
  na.omit() %>%
  group_by(group_small) %>%
  summarize(
    `Expenditure ($US)` = sum(`Acquisition Value`),
    Quantity = sum(Quantity)
  ) %>%
  ungroup() %>%
  mutate(
    `Expenditure (%)` = 100 * round(`Expenditure ($US)` / sum(`Expenditure ($US)`), 3),
    `Quantity (%)` = 100 * round(Quantity / sum(Quantity), 3)
  ) %>%
  arrange(desc(`Expenditure ($US)`)) %>%
  kable()

# Crime rates
ucr %>%
  group_by(year) %>%
  na.omit() %>%
  summarise(violent_crime = sum(violent_crime)) %>%
  ggplot(., aes(x = year, y = violent_crime, group = 1)) +
  geom_line(size = 1) +
  labs(y = "Violent Crimes", x = "Year")

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
  rename(Year = year, 
         `Violent Crime` = violent_crime, 
         `Violent Crime (% Change)` = violent_crime_change) %>% 
  kable()
