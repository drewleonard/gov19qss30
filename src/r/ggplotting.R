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
  dplyr::select(`Subject's gender`, `Subject's race`, `Subject's age`, `Date (Year)`, `Cause of death`, `Location of death (zip code)`) %>% 
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
  filter(Cause == "Gunshot" | Cause == "Vehicle" | Cause == "Tasered") %>% 
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
# merged %>%
#   filter(group_small != "Unavailable") %>%
#   mutate(Controlled = ifelse(group_small == "Weapons" | group_small == "Vehicular", "Controlled", "Non-Controlled")) %>%
#   ggplot(., aes(x = year, y = expenditure_quantity, fill = Controlled)) +
#   geom_bar(stat = 'identity', position = "dodge")
