
load("../data/census_tract_raw.rda")
load("../data/park_trail_geog_LONG.rda")


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


acs_temp <- census_tract_raw %>%
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
      name = name, # if_else(("name" %in% names(current)), name, "no name"),
      type = type, # if_else(("type" %in% names(current)), type, NA_character_),
      status = status, # if_else(("status" %in% names(current)), status, NA_character_),
      coverage = coverage,
      GEOID = GEOID,
      adj_2019pop = coverage * pop2019, # use 2019 small area estimates to weight
      adj_2019hh = coverage * hh2019,
      adj_anydis = adj_2019pop * anydis_percent, # calculate total pop in demo groups (weighted)
      adj_ambdis = adj_2019pop * ambdis_percent, # new
      adj_costburd = adj_2019hh * pcostburdr,
      adj_forborn = adj_2019pop * forborn_percent,
      adj_usborn = adj_2019pop * usborncit_percent
    )
})

return_weighted_demo_persons_AVG <- (function(...) {
  current <- tibble(...)
  current %>%
    transmute(
      agency = agency,
      coverage = coverage,
      GEOID = GEOID,
      adj_2019pop = coverage * pop2019, # use 2019 small area estimates to weight
      adj_2019hh = coverage * hh2019,
      adj_anydis = adj_2019pop * anydis_percent, # calculate total pop in demo groups (weighted)
      adj_ambdis = adj_2019pop * ambdis_percent, # new
      adj_costburd = adj_2019hh * pcostburdr,
      adj_forborn = adj_2019pop * forborn_percent,
      adj_usborn = adj_2019pop * usborncit_percent
    )
})


return_weighted_demo_percents <- (function(...) {
  current <- tibble(...)
  current %>%
    mutate(
      adj_2019pop = round(adj_2019pop, 0), # calculate %s again (from the ppl)
      adj_2019hh = round(adj_2019hh, 0),
      adj_anydis_per = round(adj_anydis / adj_2019pop * 100, 1),
      adj_ambdis_per = round(adj_ambdis / adj_2019pop * 100, 1), # new
      adj_costburd_per = round(adj_costburd / adj_2019hh * 100, 1),
      adj_forborn_per = round(adj_forborn / adj_2019pop * 100, 1),
      adj_usborn_per = round(adj_usborn / adj_2019pop * 100, 1)
    )
})




## agency average ------------------
## use acs%, 2019 pop est, and tract overlap to get adjusted demographics(adj_*)

agency_avg_tract <- (agency_tract_coverage) %>%
  left_join(acs_temp, by = c("GEOID")) %>%
  left_join(tract_pop_2019, by = c("GEOID")) %>%
  select(-geometry) %>%
  pmap_df(return_weighted_demo_persons_AVG) %>%
  group_by(agency) %>%
  summarise(across(adj_2019pop:adj_usborn, sum, na.rm = T)) %>% # need to sum for each park/agency parcel

  pmap_df(return_weighted_demo_percents) %>%
  as_tibble() %>%
  gather(
    key = "ACS", value = "value",
    -agency
  ) %>%
  mutate(value = round(value, 3))

usethis::use_data(agency_avg_tract, overwrite = TRUE)


## 1.0 mile buffer ----------------------------------------------------------------------
buff_1.0mi <- buffer_dist_fxn(1)

intersect_pct_1.0mi <- buffer_acs_fxn(buff_1.0mi)


buffer_tract_1.0mi_raw <- intersect_pct_1.0mi %>%
  left_join(acs_temp, by = c("GEOID")) %>%
  left_join(tract_pop_2019, by = c("GEOID")) %>%
  select(-geometry) %>%
  pmap_df(return_weighted_demo_persons) %>%
  mutate(distance = 1)

buffer_tract_1.0mi <- buffer_tract_1.0mi_raw %>%
  group_by(agency, name, type, status) %>%
  summarise(across(adj_2019pop:adj_usborn, sum, na.rm = T)) %>% # need to sum for each park/agency parcel
  pmap_df(return_weighted_demo_percents) %>%
  mutate(distance = 1)


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
  group_by(agency, name, type, status) %>%
  summarise(across(adj_2019pop:adj_usborn, sum, na.rm = T)) %>% # need to sum for each park/agency parcel
  pmap_df(return_weighted_demo_percents) %>%
  mutate(distance = 1.5)


## 3 mile buffer ----------------------------------------------------------------------

buff_3mi <- buffer_dist_fxn(3)

intersect_pct_3mi <- buffer_acs_fxn(buff_3mi)

buffer_tract_3mi_raw <- intersect_pct_3mi %>%
  left_join(acs_temp, by = c("GEOID")) %>%
  left_join(tract_pop_2019, by = c("GEOID")) %>%
  select(-geometry) %>%
  pmap_df(return_weighted_demo_persons) %>%
  mutate(distance = 3)

buffer_tract_3mi <- buffer_tract_3mi_raw %>%
  group_by(agency, name, type, status) %>%
  summarise(across(adj_2019pop:adj_usborn, sum, na.rm = T)) %>% # need to sum for each park/agency parcel
  pmap_df(return_weighted_demo_percents) %>%
  mutate(distance = 3)

## Combine long buffer data ---------------------------------------------------------------------
long_buffer_data_tract <- bind_rows(
  buffer_tract_1.0mi,
  buffer_tract_1.5mi,
  buffer_tract_3mi
) %>%
  as_tibble() %>%
  # select(-geometry) %>%
  gather(
    key = "ACS", value = "value",
    -agency, -name, -type, -status, -distance
  ) %>%
  filter(
    ACS != "adj_2019pop",
    ACS != "adj_2019hh"
  )

usethis::use_data(long_buffer_data_tract, overwrite = TRUE)
