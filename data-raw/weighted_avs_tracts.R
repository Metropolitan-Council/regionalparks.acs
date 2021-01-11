
load("./data/census_tract_raw.rda")
load("./data/park_trail_geog.rda")


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


## population, small area estimates -----------------------------------------------------------------------
temp <- tempfile()
download.file("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/society_small_area_estimates/xlsx_society_small_area_estimates.zip",
  destfile = temp
)

tract_pop_2019 <- readxl::read_xlsx(unzip(temp, "SmallAreaEstimatesTract.xlsx")) %>%
  janitor::clean_names() %>%
  filter(
    est_year == 2019
  ) %>%
  select(pop_est, hh_est, tr10) %>%
  rename(
    GEOID = tr10,
    pop2019 = pop_est,
    hh2019 = hh_est
  )



agency_filter <- tibble(
  agency = c(
    "Anoka County", # Parks and Recreation",
    "Bloomington", # Parks and Recreation",
    "Carver County", # Parks and Recreation" ,
    "Dakota County", # Parks",
    "MPRB",
    "Ramsey County", # Parks and Recreation" ,
    "Scott County", # Parks",
    "St. Paul", # Parks and Recreation",
    "Three Rivers",
    "Washington County"
  ), # Parks"),
  num = c(1:10)
)


park_trail_geog_temp <- park_trail_geog_LONG %>% # bind_rows(park_trail_geog, .id = "status") %>%
  full_join(agency_filter) %>%
  mutate(
    name = paste(name, num, sep = "_"),
    type = Type,
    status = status2
  ) %>%
  st_transform(3857)

acs_temp <- census_tract_raw %>%
  mutate(
    "usborncit_percent" = `Origin, US-born`,
    "forborn_percent" = `Origin, foreign-born`,
    "anydis_percent" = `Disability, any disability`
  ) %>%
  select(GEOID, usborncit_percent, forborn_percent, anydis_percent, geometry) %>%
  mutate(county = substr(GEOID, start = 3, stop = 5)) %>%
  st_transform(3857) %>% # https://epsg.io/3857\
  mutate(bg_area = st_area(.)) %>%
  select(GEOID, usborncit_percent, forborn_percent, anydis_percent, geometry, bg_area)

agency_boundary <- read_sf("/Volumes/shared/CommDev/Research/Public/GIS/Parks/Park_Operating_Agencies.shp") %>%
  mutate(COMCD_DESC = recode(COMCD_DESC, "Minneapolis" = "MPRB")) %>%
  rename(agency = COMCD_DESC) %>%
  select(agency)


## get coverage of block groups falling w/in buffer zones for all
coverage_agency <- acs_temp %>% # this has geography in it
  select(GEOID, bg_area) %>%
  st_intersection(agency_boundary %>% st_transform(3857)) %>%
  mutate(intersect_area = st_area(.)) %>% # create new column with shape area
  select(GEOID, agency, intersect_area) %>% # only select columns needed to merge
  st_drop_geometry() %>% # drop geometry as we don't need it
  left_join(acs_temp %>% # merge back in with all block groups
    select(GEOID, bg_area)) %>%
  mutate(coverage = as.numeric(intersect_area / bg_area)) %>% # calculate fraction of block group within each agency/park buffer
  as_tibble() %>%
  select(GEOID, agency, coverage)

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
      adj_forborn = adj_2019pop * forborn_percent,
      adj_usborn = adj_2019pop * usborncit_percent
    )
})


return_weighted_demo_percents <- (function(...) {
  current <- tibble(...)
  current %>%
    mutate(
      adj_2019pop = round(adj_2019pop, 0), # calculate %s again (from the ppl)
      adj_anydis_per = round(adj_anydis / adj_2019pop * 100, 1),
      adj_forborn_per = round(adj_forborn / adj_2019pop * 100, 1),
      adj_usborn_per = round(adj_usborn / adj_2019pop * 100, 1)
    )
})

buffer_dist_fxn <- function(miles) { # create buffer geometry of x distance
  buff_Xmi <- park_trail_geog_temp %>%
    st_buffer(dist = 1609.34 * miles) %>% # the 3857 projection uses meters as a distance, so 1.0 mi =
    group_by(agency, name, type, status) %>%
    summarise(geometry = st_union(geom))
  return(buff_Xmi)
}

buffer_acs_fxn <- function(df) { # intersect the buffer of x distance with the acs demographics
  buff_acs <- acs_temp %>%
    select(GEOID, bg_area) %>%
    st_intersection(df) %>% # every trail name gets own intersection
    mutate(intersect_area = st_area(.)) %>% # create new column with shape area
    select(GEOID, agency, name, type, status, intersect_area) %>% # only select columns needed to merge
    st_drop_geometry() %>% # drop geometry as we don't need it
    left_join(acs_temp %>% # merge back in with all block groups
      select(GEOID, bg_area)) %>%
    mutate(coverage = as.numeric(intersect_area / bg_area)) %>% # calculate fraction of block group within each agency/park buffer
    as_tibble() %>%
    select(GEOID, agency, name, type, status, coverage)
  return(buff_acs)
}


## agency average ------------------
## use acs%, 2019 pop est, and bg overlap to get adjusted demographics(adj_*)

agency_avg_tract <- (coverage_agency) %>%
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

# # confirm that each park/agency combo has own buffer. And that multiple parcels within parks are consolidated together
# buff_1.0mi %>%
#   filter(name == "Battle Creek Regional Park_6" |
#            name == "Indian Mounds Regional Park_8") %>%
#   ggplot() +
#   geom_sf(aes(fill = name), alpha = .6) +
#   geom_sf(data = filter(park_trail_geog_temp, name == "Battle Creek Regional Park_6" |
#                           name == "Indian Mounds Regional Park_8"),
#           aes(fill = name))

intersect_pct_1.0mi <- buffer_acs_fxn(buff_1.0mi)

# #check to make sure this geoid has multiple occurrences
# intersect_pct_1.0mi %>%
#   filter(GEOID == "270030501071")

# #confirm that each park/agency combo has created it's own % of block group included
# #note overlapping block groups in this example continued from above
# acs_temp %>%
#   right_join(intersect_pct_1.0mi) %>%
#   filter(name == "Battle Creek Regional Park_6" |
#            name == "Indian Mounds Regional Park_8") %>%
#   ggplot() +
#   geom_sf(aes(fill = coverage), alpha = .6) +
#   facet_wrap(~name) +
#   geom_sf(data = filter(buff_1.0mi, name == "Battle Creek Regional Park_6" |
#                           name == "Indian Mounds Regional Park_8"),
#           aes(col = name),
#           fill = "black") +
#   geom_sf(data = filter(park_trail_geog_temp, name == "Battle Creek Regional Park_6" |
#                           name == "Indian Mounds Regional Park_8"),
#   aes(col = name), fill = 'white', alpha = 1)

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


# agency-level averages for each variable including existing and planned units ----
agency_planned_existing_avgs_tract <- long_buffer_data_tract %>%
  filter(status != "Search") %>%
  group_by(agency, distance, ACS) %>%
  summarize(avg = round(mean(value), 1)) %>%
  filter(stringr::str_detect(ACS, "per"))

usethis::use_data(agency_planned_existing_avgs_tract, overwrite = TRUE)

# helper tibble ----------------------------------------------------------------

tract_vars <- tibble(ACS = c("adj_anydis_per", "adj_forborn_per", "adj_usborn_per"))

usethis::use_data(tract_vars, overwrite = TRUE)
