rm(list = ls())
setwd('~/Desktop/19W/qss_30/project/')

library(tidyverse)

# Read in four data-sets
controls <- read_csv('./data/controls_cleaner.csv')
ucr <- read_csv('./data/ucr_merged_clean.csv')
disp <- read_csv('./data/disp_merged_states_clean_all.csv')
fe <- read_csv('./data/fatal_encounters_clean.csv')

# Controls
controls <- controls %>%
  mutate(population_black = prop_black * population)

# Violent crime by county and year
ucr_flat <- ucr %>%
  filter(county_long != "Unavailable" &
           county_short != "Unavailable") %>%
  rename(state = State) %>%
  group_by(county_long, state, year) %>%
  summarize(violent_crime = sum(violent_crime)) %>%
  filter(!is.na(violent_crime))

# 1033 expenditures by county, year, and type
disp_flat <- disp %>%
  filter(county_long != "Unavailable" &
           county_short != "Unavailable") %>%
  rename(state = State) %>%
  mutate(
    year = as.numeric(substring(`Ship Date`, 1, 4)),
    cost = `Acquisition Value`,
    controlled = ifelse(group_small == "Weapons" | group_small == "Vehicular", "controlled", "noncontrolled")
  ) %>%
  group_by(county_long, state, year, controlled) %>%
  summarize(cost_total_logged = log(sum(cost))) %>%
  spread(controlled, cost_total_logged) %>% 
  mutate(controlled = ifelse(is.na(controlled), 0, controlled),
         noncontrolled = ifelse(is.na(noncontrolled), 0, noncontrolled))

# Civilian killings by county and year
fe_flat <- fe %>%
  filter(county_long != "Unavailable" &
           county_short != "Unavailable") %>%
  rename(state = `Location of death (state)`,
         year = `Date (Year)`) %>%
  group_by(county_long, state, year) %>%
  summarize(fatalities = n())

# Merge two control dataframes
# 1. ucr_flat: violent crime
# 2. controls: various demographics
merged <-
  merge(ucr_flat,
        controls,
        by = c("year", "state", "county_long"))

# Add civilian killings
# Use all control counties ...
# and assume that some have no fatal encounters
merged <-
  merge(merged,
        fe_flat,
        by = c("year", "state", "county_long"),
        all.x = TRUE)

# Add expenditures
# Assume that some counties have no expenditures
merged <-
  merge(merged,
        disp_flat,
        by = c("year", "state", "county_long"),
        all.x = TRUE)

# Give zero values to county's without expenditures and fatalities
# I do this because some county--year observations do not have either
# expenditures or fatalities (which is natural)
merged <- merged %>%
  mutate(
    fatalities = ifelse(is.na(fatalities), 0, fatalities),
    controlled = ifelse(is.na(controlled), 0, controlled),
    noncontrolled = ifelse(is.na(noncontrolled), 0, noncontrolled),
    total = controlled + noncontrolled
  )

write.csv(merged, './data/analysis_ready.csv')
