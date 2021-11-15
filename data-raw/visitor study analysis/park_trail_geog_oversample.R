## code to prepare `park_trail_geog` dataset goes here

# pkgload::load_all()
requireNamespace("readxl", quietly = TRUE)
requireNamespace("fs", quietly = TRUE)
requireNamespace("tigris", quietly = TRUE)
requireNamespace("janitor", quietly = TRUE)

library(dplyr)
library(fs)
library(sf)
library(tigris)
library(janitor)

## NameCleaner-----------------------------------------------------------------------
namecleaner <- tibble::tribble(
  ~AGENCY,
  ~consistentagency,
  "Anoka County Parks and Recreation",
  "Anoka County",
  "Anoka County Parks",
  "Anoka County",
  "Anoka County",
  "Anoka County",
  "Bloomington Parks and Recreation",
  "Bloomington",
  "Bloomington",
  "Bloomington",
  "City of Bloomington",
  "Bloomington",
  "Carver County Parks and Recreation",
  "Carver County",
  "Carver County Parks",
  "Carver County",
  "Carver County",
  "Carver County",
  "Ramsey County Parks and Recreation",
  "Ramsey County",
  "Ramsey County",
  "Ramsey County",
  "Dakota County Parks",
  "Dakota County",
  "Dakota County",
  "Dakota County",
  "Minneapolis Park and Recreation Board",
  "MPRB",
  "Minneapolis",
  "MPRB",
  "Washington County Parks",
  "Washington County",
  "Washington County",
  "Washington County",
  "St. Paul Parks and Recreation",
  "St. Paul",
  "St Paul Parks And Recreation",
  "St. Paul",
  "St Paul Parks and Recreation",
  "St. Paul",
  "St. Paul",
  "St. Paul",
  "Scott County / Three Rivers Park District",
  "Scott County",
  # this is the Scott County Regional Trail
  "Scott County/Three Rivers Park District",
  "Scott County",
  "Scott County Parks",
  "Scott County",
  "Scott County",
  "Scott County",
  "Three Rivers Park District",
  "Three Rivers",
  "Three Rivers",
  "Three Rivers"
)

## Parks -----------------------------------------------------------------------
temp <- tempfile()
download.file(
  "ftp://ftp.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/plan_parks_regional/gpkg_plan_parks_regional.zip",
  destfile = temp
)

parks_temp <- sf::read_sf(unzip(temp, "plan_parks_regional.gpkg")) %>%
  left_join(namecleaner) %>%
  filter(STATUS == "Existing",
         Label %in%
           c("Hyland-Bush-Anderson Lakes Park Reserve",
             "Como Zoo", "Marjorie McNeely Conservatory",#"Como Regional Zoo and Conservatory",
             "Lake Elmo Park Reserve",
             "Lebanon Hills Regional Park",
             "North Mississippi Regional Park",
             "Battle Creek Regional Park",
             "Cleary Lake Regional Park",
             "Lake Minnewashta Regional Park"
           )) %>%
  
  mutate(
    consistentagency = if_else((PARKNAME == "Cleary Lake" |
                                  PARKNAME == "Murphy-Hanrehan" |
                                  # PARKNAME == "Spring Lake" |
                                  PARKNAME == "Cedar Lake Farm"), "Scott County", consistentagency),
    consistentagency = if_else(Label == "Spring Lake Regional Park", "Scott County", consistentagency)
  ) %>%
  
  mutate(Label = if_else(Label == "Como Zoo" | Label == "Marjorie McNeely Conservatory", "Como Regional Zoo and Conservatory", Label)) %>%

  group_by(STATUS, Label, consistentagency) %>%
  summarize(do_union = TRUE) %>%
  ungroup() %>%
  select(
    name = Label,
    agency = consistentagency,
    status = STATUS,
  ) %>%
  st_transform(4326) %>%
  st_as_sf() %>%
  
  mutate(name = 
           case_when(name == "Hyland-Bush-Anderson Lakes" & agency == "Bloomington" ~ "Bush and Normandale Lakes Regional Park",
                     name == "North Mississippi Regional Park" & agency == "MPRB" ~ "North Mississippi Regional Park (MPRB)",
                     name == "Cleary Lake Regional Park" ~ "Cleary Lake Regional Park (Scott County)",
                     TRUE ~ name
  )) %>%
  filter(name != "North Mississippi Regional Park",
         name != "Hyland-Bush-Anderson Lakes")
  
fs::file_delete("plan_parks_regional.gpkg")

## Trails ----------------------------------------------------------------------
temp <- tempfile()
download.file(
  "https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/trans_regional_trails_exst_plan/gpkg_trans_regional_trails_exst_plan.zip",
  destfile = temp
)


trails <- sf::read_sf(unzip(temp, "trans_regional_trails_exst_plan.gpkg")) %>%
  filter(Label %in% c("Rice Creek West", "Lake Minnetonka LRT" ),
         STATUS == "Existing (Open to Public)")  %>%
  rename(AGENCY = Agency) %>%
  left_join(namecleaner) %>%
  group_by(NAME, STATUS, Label, consistentagency) %>%
  summarize(do_union = TRUE) %>%
  ungroup() %>%
  select(
    name = NAME,
    agency = consistentagency,
    status = STATUS
  ) %>%
  st_transform(4326) %>%
  st_as_sf() %>%
  sf::st_make_valid() %>%
  
  mutate(name = 
           case_when(name == "Lake Minnetonka LRT Regional Trail" & agency == "Three Rivers" ~ "Lake Minnetonka LRT Regional Trail",
                     name == "Rice Creek West Regional Trail" & agency == "Anoka County" ~ "Rice Creek West Regional Trail (Anoka County)",
                     TRUE ~ NA_character_
           )) %>%
  filter(!is.na(name))

fs::file_delete("trans_regional_trails_exst_plan.gpkg")


## Combine ---------------------------------------------------------------------
park_trail_geog_oversample<- bind_rows(
  parks_temp,
  trails
) 

usethis::use_data(park_trail_geog_oversample, overwrite = TRUE)
# usethis::use_git_ignore(".DS_Store")
