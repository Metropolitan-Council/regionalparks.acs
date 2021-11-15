
load("./data/census_tract_spatial_visitorstudy.rda")
load("./data/park_trail_geog_oversample.rda")


requireNamespace("readxl", quietly = TRUE)
requireNamespace("fs", quietly = TRUE)
requireNamespace("tigris", quietly = TRUE)
requireNamespace("janitor", quietly = TRUE)

library(tidyverse)
library(fs)
library(sf)
library(tigris)
library(janitor)
library("stringr")
library("cowplot")


acs_temp <- census_tract_spatial_visitorstudy %>% #census_tract_raw %>%
  st_transform(3857) %>% # https://epsg.io/3857\
  mutate(AREA = st_area(.))


## get coverage of block groups falling w/in buffer zones for all
agency_tract_coverage <- coverage_agency(acs_temp)

## set up some helper fxns ---------------
return_weighted_demo_persons <- (function(...) {
  current <- tibble(...)
  current %>%
    transmute(
      agency = agency,
      name = name,
      coverage = coverage,
      GEOID = GEOID,
      adj_2019pop = coverage * pop2019,
      
      # use 2019 small area estimates to weight
      adj_2019hh = coverage * hh2019,
      `18-24` = adj_2019pop * pop_1824,
      `12-17` = adj_2019pop * pop_1217,
      `25-34` = adj_2019pop * pop2534,
      `35-44` = adj_2019pop * pop_3544,
      `45-54` = adj_2019pop * pop_4554,
      `55-64` = adj_2019pop * pop_5564,
      `65-74` = adj_2019pop * pop_6574,
      `75+` = adj_2019pop * pop_75up,
      
      White = racewhite * adj_2019pop,
      `Black/ African/ African American` = raceblack * adj_2019pop,
      `American Indian/ Alaska Native` = raceamind  * adj_2019pop,
      `Asian/ Asian American` = raceasian  * adj_2019pop,
      `Multiple races/ ethnicities` = racemulti  * adj_2019pop,
      # racepacific = pacificnh / poptotal,
      `Other race` = (raceother + racepacific)   * adj_2019pop,
      `Hispanic/ Latinx/ Latino` = racehisp   * adj_2019pop,
      
      # over18 = adj_2019pop * pop_over18,
      English = langenglish * adj_2019pop,
      # Hmong = langhmong * adj_2019pop,
      Spanish = langspanish * adj_2019pop,
      # Somali = langsomali * adj_2019pop,
      `Other language` = langother * adj_2019pop,
      
      `Less than high school graduate` = lesshs * adj_2019pop,
      `High school graduate or GED` = highschool * adj_2019pop,
      `Some post-secondary` = somecolleg * adj_2019pop,
      `2-year degree` = associate * adj_2019pop,
      `4-year degree` = bachelors * adj_2019pop,
      `Graduate or professional degree` = gradprof * adj_2019pop,
      
      #always should be the last variable
      `No disability` = adj_2019pop * nodis_percent,
      `Yes disability` = adj_2019pop * anydis_percent
      
    )
})


## agency average ------------------
## use acs%, 2019 pop est, and tract overlap to get adjusted demographics(adj_*)

agency_avg_tract_oversample <- (agency_tract_coverage %>% filter(coverage > 0)) %>%
  left_join(acs_temp, by = c("GEOID")) %>%
  left_join(tract_pop_2019, by = c("GEOID")) %>%
  select(-geometry) %>%
  mutate(name = agency) %>%
  pmap_df(return_weighted_demo_persons) %>%
  group_by(agency) %>%
  summarise(across(adj_2019pop:`Yes disability`, sum, na.rm = T)) %>%
  # need to sum for each park/agency parcel

  as_tibble() %>%
  gather(
    key = "ACS",
    value = "value",
    -agency
  ) %>%
  mutate(agency = case_when(agency == "St. Paul" ~ "Saint Paul",
                            agency == "Three Rivers" ~ "Three Rivers Park District",
                            TRUE ~ agency))#%>%
  # mutate(value = round(value, 3))

# usethis::use_data(agency_avg_tract_oversample, overwrite = TRUE)
save(agency_avg_tract_oversample, file = "/Users/escheh/Documents/GitHub/park.visitor.survey/data-raw/agency_avg_tract_oversample.rda")

## 1.5 mile buffer ----------------------------------------------------------------------

buff_1.5mi <- buffer_dist_fxn(1.5)

intersect_pct_1.5mi <- buffer_acs_fxn(buff_1.5mi)

buffer_tract_1.5mi_raw <- intersect_pct_1.5mi %>%
  left_join(acs_temp, by = c("GEOID")) %>%
  left_join(tract_pop_2019, by = c("GEOID")) %>%
  select(-geometry) %>%
  pmap_df(return_weighted_demo_persons) %>%
  mutate(distance = 1.5)

buffer_tract_1.5mi <- buffer_tract_1.5mi_raw %>%
  group_by(agency, name) %>%
  summarise(across(adj_2019pop:adj_anydis, sum, na.rm = T)) %>%
  mutate(distance = 1.5)


## Combine long buffer data ---------------------------------------------------------------------
long_buffer_data_tract_oversample <- #bind_rows(
  # buffer_tract_1.0mi,
  buffer_tract_1.5mi %>%#,
  # buffer_tract_3mi
# ) %>%
  as_tibble() %>%
  # select(-geometry) %>%
  gather(
    key = "ACS",
    value = "value",
    -agency,
    -name,
    # -type,
    # -status,
    -distance
  ) %>%
  filter(
    ACS != "adj_2019pop",
    ACS != "adj_2019hh"
  ) %>%
  mutate(agency = case_when(agency == "St. Paul" ~ "Saint Paul",
                            agency == "Three Rivers" ~ "Three Rivers Park District",
                            TRUE ~ agency)) 

# usethis::use_data(long_buffer_data_tract_oversample, overwrite = TRUE)
#save to rda file
save(long_buffer_data_tract_oversample, file = "/Users/escheh/Documents/GitHub/park.visitor.survey/data-raw/long_buffer_data_tract_oversample.rda")
