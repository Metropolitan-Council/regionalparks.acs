## code to prepare `buffer_distances`, `long_buffer_data_bg`, `agency_avg` datasets goes here

load("../data/block_group_raw.rda")
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


acs_temp <- block_group_raw %>%
  st_transform(3857) %>% # https://epsg.io/3857\
  mutate(AREA = st_area(.))



## get coverage of block groups falling w/in buffer zones for all
agency_bg_coverage <- coverage_agency(acs_temp)


## set up some helper fxns ---------------
return_weighted_demo_persons <- (function(...) {
  current <- tibble(...)
  current %>%
    transmute(
      agency = agency,
      name = name,
      type = type,
      status = status,
      coverage = coverage,
      GEOID = GEOID,
      adj_2019pop = coverage * pop2019,
      # use 2019 small area estimates to weight
      adj_2019hh = coverage * hh2019,
      
      across(ageunder15:age65_up | starts_with(c("lep"))| ends_with(c("nh", "hisppop")) | contains("pov185"), 
             ~.x * adj_2019pop,
             .names = "adj_{.col}"),
      
      across(contains(c("meanhhinc", "noveh")),
             ~.x * adj_2019hh,
             .names = "adj_{.col}")
    )
})

return_weighted_demo_persons_AVG <- (function(...) {
  current <- tibble(...)
  current %>%
    transmute(
      agency = agency,
      coverage = coverage,
      GEOID = GEOID,
      adj_2019pop = coverage * pop2019,
      # use 2019 small area estimates to weight
      adj_2019hh = coverage * hh2019,
      
      across(ageunder15:age65_up | starts_with(c("lep"))| ends_with(c("nh", "hisppop")) | contains("pov185"),
             ~.x * adj_2019pop,
             .names = "adj_{.col}"),

      across(contains(c("meanhhinc", "noveh")),
             ~.x * adj_2019hh,
             .names = "adj_{.col}")
    )
})


return_weighted_demo_percents <- (function(...) {
  current <- tibble(...)
  current %>%
    mutate(
      adj_2019pop = round(adj_2019pop, 0),
      # calculate %s again (from the ppl)
      across(starts_with("adj_") & !contains(c("meanhhinc", "novehicle")),
             ~round(.x / adj_2019pop * 100, 1),
             .names = "{.col}_per"),
      across(starts_with("adj_") & contains(c("meanhhinc", "novehicle")),
             ~round(.x / adj_2019hh * 100, 1),
             .names = "{.col}_per"),
    )
})

## agency average ------------------
## use acs%, 2019 pop est, and bg overlap to get adjusted demographics(adj_*)

agency_avg_bg <- (agency_bg_coverage) %>%
  left_join(acs_temp, by = c("GEOID")) %>%
  left_join(bg_pop_2019, by = c("GEOID")) %>%
  select(-geometry) %>% 
  pmap_df(return_weighted_demo_persons_AVG) %>%
  group_by(agency) %>%
  summarise(across(starts_with("adj_"), sum, na.rm = T)) %>% # need to sum for each park/agency parcel

  pmap_df(return_weighted_demo_percents) %>%
  as_tibble() %>%
  gather(
    key = "ACS",
    value = "value",
    -agency
  ) %>%
  mutate(value = round(value, 3))

usethis::use_data(agency_avg_bg, overwrite = TRUE)


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
  summarise(across(starts_with("adj_"), sum, na.rm = T)) %>% # need to sum for each park/agency parcel
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
  summarise(across(starts_with("adj_"), sum, na.rm = T)) %>% # need to sum for each park/agency parcel
  pmap_df(return_weighted_demo_percents) %>%
  mutate(distance = 1.5)

# buffer_block_group_1.5mi %>% filter(agency == "Scott County") %>% view()


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
  summarise(across(starts_with("adj_"), sum, na.rm = T)) %>% # need to sum for each park/agency parcel
  pmap_df(return_weighted_demo_percents) %>%
  mutate(distance = 3)

# buffer_block_group_3mi %>% filter(agency == "Scott County") %>% view()

## Combine long buffer data ---------------------------------------------------------------------

long_buffer_data_bg <- bind_rows(
  buffer_block_group_1.0mi,
  buffer_block_group_1.5mi,
  buffer_block_group_3mi
) %>%
  as_tibble() %>%
  # select(-geometry) %>%
  gather(
    key = "ACS",
    value = "value",
    -agency,
    -name,
    -type,
    -status,
    -distance
  )

usethis::use_data(long_buffer_data_bg, overwrite = TRUE)


# # agency-level averages for each variable including existing and planned units
# agency_planned_existing_avgs <- long_buffer_data_bg %>%
#   filter(status != "Search") %>%
#   group_by(agency, distance, ACS) %>%
#   summarize(avg = round(mean(value), 1)) %>%
#   filter(stringr::str_detect(ACS, "per"))
#
# usethis::use_data(agency_planned_existing_avgs, overwrite = TRUE)

## Combine RAW long buffer data ---------------------------------------------------------------------
# #having a change of heart in showing these data. The data that should be shown include: acs demographic percents, buffer overlap, AND 2019 population estimates. Relatively easy to show the acs demo % and the buffer overlap. But will be much harder to show the population estimates. So I am no longer in favor of showing a raw data tab. Happy to continue discussion here.

# long_buffer_data_bg_raw <- bind_rows(
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
# usethis::use_data(long_buffer_data_bg_raw, overwrite = TRUE)



## Combine buffer geometries ---------------------------------------------------------------------
buffer_geo <- (buff_1.0mi %>% mutate(distance = 1.0)) %>%
  bind_rows(buff_1.5mi %>% mutate(distance = 1.5)) %>%
  bind_rows(buff_3mi %>% mutate(distance = 3)) %>%
  st_transform(4326) %>%
  st_as_sf()

buffer_geo <- buffer_geo %>%
  separate(name, remove = FALSE, into = c("map_name", "delete"), sep = "_") %>%
  mutate(popup_text = paste0(
    "<b>",
    "Buffer: ",
    status,
    ", ",
    type,
    "</b>",
    "<br>",
    map_name,
    "<br>",
    "<em>",
    agency,
    "</em>"
  ))
usethis::use_data(buffer_geo, overwrite = TRUE)

# usethis::use_git_ignore(".DS_Store")


# filter(buffer_geo, agency == "Scott County") %>% view()


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
