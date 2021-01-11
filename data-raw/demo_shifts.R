## code to prepare `demo_shifts` dataset goes here

library(tidyverse)
library(readxl)

## set-up -------------------------------------------------------------------------------------------------

# download data here: https://metrocouncil.org/Data-and-Maps/Research-and-Data/Thrive-2040-Forecasts.aspx
shifts <- read_xlsx("/Users/escheh/Downloads/Regional-Forecast-Tables-(October-2019).xlsx",
  sheet = "Population",
  skip = 5
) %>%
  filter(
    Region == "Minneapolis-Saint Paul Area",
    Units == "Individuals",
    Category == "Population"
  )

totalpop <- shifts %>%
  filter(
    Race == "All Races",
    Ages == "All Ages"
  ) %>%
  pivot_longer(
    cols = `2010`:`2050`,
    names_to = "Year",
    values_to = "Pop"
  ) %>%
  group_by(Year) %>%
  summarise(TotalPop = sum(Pop))


## shifts in race -------------------------------------------------------------------------------------------------

raceshift <- shifts %>%
  filter(
    Race != "All Races",
    Ages == "All Ages"
  ) %>%
  pivot_longer(
    cols = `2010`:`2050`,
    names_to = "Year",
    values_to = "Pop"
  ) %>%
  group_by(Race, Ages, Year) %>%
  summarize(SubTotalPop = sum(Pop)) %>%
  left_join(totalpop) %>%
  mutate(Percent = SubTotalPop / TotalPop * 100) %>%
  mutate(Type = "raceshift")

## shifts in age -------------------------------------------------------------------------------------------------

ageshift <- shifts %>%
  filter(
    Race == "All Races",
    Ages != "All Ages"
  ) %>%
  mutate(cat_age = case_when(
    Ages == "Ages 0-4" ~ "Ages 0-14",
    Ages == "Ages 5-9" ~ "Ages 0-14",
    Ages == "Ages 10-14" ~ "Ages 0-14",
    Ages == "Ages 15-19" ~ "Ages 15-24",
    Ages == "Ages 20-24" ~ "Ages 15-24",
    Ages == "Ages 25-29" ~ "Ages 25-64",
    Ages == "Ages 30-34" ~ "Ages 25-64",
    Ages == "Ages 35-39" ~ "Ages 25-64",
    Ages == "Ages 40-44" ~ "Ages 25-64",
    Ages == "Ages 45-49" ~ "Ages 25-64",
    Ages == "Ages 50-54" ~ "Ages 25-64",
    Ages == "Ages 55-59" ~ "Ages 25-64",
    Ages == "Ages 60-64" ~ "Ages 25-64",
    Ages == "Ages 65-69" ~ "Ages 65+",
    Ages == "Ages 70-74" ~ "Ages 65+",
    Ages == "Ages 75-79" ~ "Ages 65+",
    Ages == "Ages 80-84" ~ "Ages 65+",
    Ages == "Ages 85+" ~ "Ages 65+"
  )) %>%
  pivot_longer(
    cols = `2010`:`2050`,
    names_to = "Year",
    values_to = "Pop"
  ) %>%
  group_by(Race, cat_age, Year) %>%
  summarize(SubTotalPop = sum(Pop)) %>%
  left_join(totalpop) %>%
  mutate(Percent = SubTotalPop / TotalPop * 100) %>%
  rename(Ages = cat_age) %>%
  mutate(Type = "ageshift")


## write dataset -------------------------------------------------------------------------------------------------
demo_shifts <- bind_rows(raceshift, ageshift)

usethis::use_data(demo_shifts, overwrite = TRUE)
