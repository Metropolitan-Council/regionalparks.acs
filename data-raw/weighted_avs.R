## code to prepare `buffer_distances`, `long_buffer_data`, `agency_avg` datasets goes here

load("./data/block_group.rda")
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

bg_pop_2019 <- readxl::read_xlsx(unzip(temp, "SmallAreaEstimatesBlockGroup.xlsx")) %>%
  janitor::clean_names() %>%
  filter(
    est_year == 2019
  ) %>%
  select(pop_est, hh_est, bg10) %>%
  rename(
    GEOID = bg10,
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

park_trail_geog_temp <- bind_rows(park_trail_geog, .id = "status") %>%
  full_join(agency_filter) %>%
  mutate(
    name = paste(name, num, sep = "_"),
    type = if_else(status == "park" |
      status == "park_planned" |
      status == "park_search", "Park", "Trail"),
    status = case_when(
      status == "park" | status == "trail" ~ "Existing",
      status == "park_planned" | status == "trail_planned" ~ "Planned",
      status == "park_search" | status == "trail_search" ~ "Search"
    )
  ) %>%
  st_transform(3857) # https://epsg.io/3857\


acs_temp <- block_group %>%
  mutate(county = substr(GEOID, start = 3, stop = 5)) %>%
  st_transform(3857) %>% # https://epsg.io/3857\
  mutate(bg_area = st_area(.))

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
      adj_ageunder15 = adj_2019pop * ageunder15_percent, # calculate total pop in demo groups (weighted)
      adj_age15_24 = adj_2019pop * age15_24_percent,
      adj_age25_64 = adj_2019pop * age25_64_percent,
      adj_age65up = adj_2019pop * age65up_percent,
      adj_whitenh = adj_2019pop * whitenh_percent,
      adj_blacknh = adj_2019pop * blacknh_percent,
      adj_asiannh = adj_2019pop * asiannh_percent,
      adj_amindnh = adj_2019pop * amindnh_percent,
      adj_othermultinh = adj_2019pop,
      adj_hisppop = adj_2019pop * hisppop_percent,
      adj_nothisppop = adj_2019pop * nothisppop_percent,
      adj_totalhhi = adj_2019hh * meanhhinc,
      adj_novehicle = adj_2019hh * novehicle_percent,
      adj_lep = adj_2019pop * poorenglish_percent,
      adj_span = adj_2019pop * spanish_percent
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
      adj_ageunder15 = adj_2019pop * ageunder15_percent, # calculate total pop in demo groups (weighted)
      adj_age15_24 = adj_2019pop * age15_24_percent,
      adj_age25_64 = adj_2019pop * age25_64_percent,
      adj_age65up = adj_2019pop * age65up_percent,
      adj_whitenh = adj_2019pop * whitenh_percent,
      adj_blacknh = adj_2019pop * blacknh_percent,
      adj_asiannh = adj_2019pop * asiannh_percent,
      adj_amindnh = adj_2019pop * amindnh_percent,
      adj_othermultinh = adj_2019pop,
      adj_hisppop = adj_2019pop * hisppop_percent,
      adj_nothisppop = adj_2019pop * nothisppop_percent,
      adj_totalhhi = adj_2019hh * meanhhinc,
      adj_novehicle = adj_2019hh * novehicle_percent,
      adj_lep = adj_2019pop * poorenglish_percent,
      adj_span = adj_2019pop * spanish_percent
    )
})


return_weighted_demo_percents <- (function(...) {
  current <- tibble(...)
  current %>%
    mutate(
      adj_2019pop = round(adj_2019pop, 0), # calculate %s again (from the ppl)
      adj_ageunder15_per = round(adj_ageunder15 / adj_2019pop * 100, 1),
      adj_age15_24_per = round(adj_age15_24 / adj_2019pop * 100, 1),
      adj_age25_64_per = round(adj_age25_64 / adj_2019pop * 100, 1),
      adj_age65up_per = round(adj_age65up / adj_2019pop * 100, 1),
      adj_whitenh_per = round(adj_whitenh / adj_2019pop * 100, 1),
      adj_blacknh_per = round(adj_blacknh / adj_2019pop * 100, 1),
      adj_asiannh_per = round(adj_asiannh / adj_2019pop * 100, 1),
      adj_amindnh_per = round(adj_amindnh / adj_2019pop * 100, 1),
      adj_othermultinh_per = round(adj_othermultinh / adj_2019pop * 100, 1),
      adj_hisppop_per = round(adj_hisppop / adj_2019pop * 100, 1),
      adj_nothisppop_per = round(adj_nothisppop / adj_2019pop * 100, 1),
      adj_meanhhi = round(adj_totalhhi / adj_2019hh, 1),
      adj_novehicle_per = round(adj_novehicle / adj_2019hh * 100, 1),
      adj_lep_per = round(adj_lep / adj_2019pop * 100, 1),
      adj_span_per = round(adj_span / adj_2019pop * 100, 1)
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

agency_avg <- (coverage_agency) %>%
  left_join(acs_temp, by = c("GEOID")) %>%
  left_join(bg_pop_2019, by = c("GEOID")) %>%
  select(-geometry) %>%
  pmap_df(return_weighted_demo_persons_AVG) %>%
  group_by(agency) %>%
  summarise(across(adj_2019pop:adj_span, sum, na.rm = T)) %>% # need to sum for each park/agency parcel

  pmap_df(return_weighted_demo_percents) %>%
  as_tibble() %>%
  gather(
    key = "ACS", value = "value",
    -agency
  ) %>%
  mutate(value = round(value, 3))

usethis::use_data(agency_avg, overwrite = TRUE)


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

buffer_block_group_1.0mi_raw <- intersect_pct_1.0mi %>%
  left_join(acs_temp, by = c("GEOID")) %>%
  left_join(bg_pop_2019, by = c("GEOID")) %>%
  select(-geometry) %>%
  pmap_df(return_weighted_demo_persons) %>%
  mutate(distance = 1)

buffer_block_group_1.0mi <- buffer_block_group_1.0mi_raw %>%
  group_by(agency, name, type, status) %>%
  summarise(across(adj_2019pop:adj_span, sum, na.rm = T)) %>% # need to sum for each park/agency parcel
  pmap_df(return_weighted_demo_percents) %>%
  mutate(distance = 1)


## 1.5 mile buffer ----------------------------------------------------------------------

buff_1.5mi <- buffer_dist_fxn(1.5)

intersect_pct_1.5mi <- buffer_acs_fxn(buff_1.5mi)

buffer_block_group_1.5mi_raw <- intersect_pct_1.5mi %>%
  left_join(acs_temp, by = c("GEOID")) %>%
  left_join(bg_pop_2019, by = c("GEOID")) %>%
  select(-geometry) %>%
  pmap_df(return_weighted_demo_persons) %>%
  mutate(distance = 1.5)

buffer_block_group_1.5mi <- buffer_block_group_1.5mi_raw %>%
  group_by(agency, name, type, status) %>%
  summarise(across(adj_2019pop:adj_span, sum, na.rm = T)) %>% # need to sum for each park/agency parcel
  pmap_df(return_weighted_demo_percents) %>%
  mutate(distance = 1.5)


## 3 mile buffer ----------------------------------------------------------------------

buff_3mi <- buffer_dist_fxn(3)

intersect_pct_3mi <- buffer_acs_fxn(buff_3mi)

buffer_block_group_3mi_raw <- intersect_pct_3mi %>%
  left_join(acs_temp, by = c("GEOID")) %>%
  left_join(bg_pop_2019, by = c("GEOID")) %>%
  select(-geometry) %>%
  pmap_df(return_weighted_demo_persons) %>%
  mutate(distance = 3)

buffer_block_group_3mi <- buffer_block_group_3mi_raw %>%
  group_by(agency, name, type, status) %>%
  summarise(across(adj_2019pop:adj_span, sum, na.rm = T)) %>% # need to sum for each park/agency parcel
  pmap_df(return_weighted_demo_percents) %>%
  mutate(distance = 3)

## Combine long buffer data ---------------------------------------------------------------------
long_buffer_data <- bind_rows(
  buffer_block_group_1.0mi,
  buffer_block_group_1.5mi,
  buffer_block_group_3mi
) %>%
  as_tibble() %>%
  select(-geometry) %>%
  gather(
    key = "ACS", value = "value",
    -agency, -name, -type, -status, -distance
  )

usethis::use_data(long_buffer_data, overwrite = TRUE)


# agency-level averages for each variable including existing and planned units
agency_planned_existing_avgs <- long_buffer_data %>% 
  filter(status != "Search") %>% 
  group_by(agency, distance, ACS) %>% 
  summarize(avg = round(mean(value), 1)) %>% 
  filter(stringr::str_detect(ACS, "per")) 

usethis::use_data(agency_planned_existing_avgs, overwrite = TRUE)

## Combine RAW long buffer data ---------------------------------------------------------------------
# #having a change of heart in showing these data. The data that should be shown include: acs demographic percents, buffer overlap, AND 2019 population estimates. Relatively easy to show the acs demo % and the buffer overlap. But will be much harder to show the population estimates. So I am no longer in favor of showing a raw data tab. Happy to continue discussion here.

# long_buffer_data_raw <- bind_rows(
#   buffer_block_group_1.0mi_raw,
#   buffer_block_group_1.5mi_raw,
#   buffer_block_group_3mi_raw
# ) %>%
#   as_tibble() %>%
#   select(
#     agency, name, type, status, distance,
#     GEOID, coverage,
#     ageunder15_percent, age15_24_percent, age25_64_percent, age65up_percent,
#     amindnh_percent, asiannh_percent, blacknh_percent, othermutltnh_percent, whitenh_percent,
#     hisppop_percent, nothisppop_percent,
#     meanhhinc,
#     novehicle_percent, poorenglish_percent, spanish_percent
#   ) %>%
#   gather(
#     key = "ACS", value = "value",
#     -agency, -name, -type, -status, -distance, -GEOID, -coverage
#   ) %>%
#   mutate(ACS = recode(ACS,
#     "ageunder15_percent" = "adj_ageunder15_per",
#     "age15_24_percent" = "adj_age15_24_per",
#     "age25_64_percent" = "adj_age25_64_per",
#     "age65up_percent" = "adj_age65up_per",
#     "whitenh_percent" = "adj_whitenh_per",
#     "blacknh_percent" = "adj_blacknh_per",
#     "asiannh_percent" = "adj_asiannh_per",
#     "amindnh_percent" = "adj_amindnh_per",
#     "othermutltnh_percent" = "adj_othermultinh_per",
#     "hisppop_percent" = "adj_hisppop_per",
#     "nothisppop_percent" = "adj_nothisppop_per",
#     "meanhhinc" = "adj_meanhhi",
#     "novehicle_percent" = "adj_novwhicle_per",
#     "poorenglish_percent" = "adj_lep_per",
#     "spanish_percent" = "adj_span_per"
#   ))
#
# usethis::use_data(long_buffer_data_raw, overwrite = TRUE)



## Combine buffer geometries ---------------------------------------------------------------------

buffer_geo <- (buff_1.0mi %>% mutate(distance = 1.0)) %>%
  bind_rows(buff_1.5mi %>% mutate(distance = 1.5)) %>%
  bind_rows(buff_3mi %>% mutate(distance = 3)) %>%
  st_transform(4326) %>%
  st_as_sf()
usethis::use_data(buffer_geo, overwrite = TRUE)

usethis::use_git_ignore(".DS_Store")

# ###############
# # GEE files
# ###############
# BUFF <- buff_1.5mi %>%
#   filter(status == "park") %>%
#   st_transform(26915) %>%
#   rowid_to_column("num")
# PARKS <- park_trail_geog_temp %>%
#   filter(status == "park") %>%
#   select(-num) %>%
#   st_transform(26915) %>% #https://epsg.io/26915
#   st_combine()
# # write_sf(BUFF, "/Users/escheh/Local work docs/Parks/park buffer less parks/arcBUFF.shp")
# # write_sf(PARKS, "/Users/escheh/Local work docs/Parks/park buffer less parks/arcPARKS.shp")
