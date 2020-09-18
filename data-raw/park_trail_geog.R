## code to prepare `park_trail_geog` dataset goes here

pkgload::load_all()
requireNamespace("readxl", quietly = TRUE)
requireNamespace("fs", quietly = TRUE)
requireNamespace("tigris", quietly = TRUE)
requireNamespace("janitor", quietly = TRUE)

library(dplyr)
library(fs)
library(sf)
library(tigris)
library(janitor)

## Parks -----------------------------------------------------------------------
temp <- tempfile()
download.file("ftp://ftp.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/plan_parks_regional/gpkg_plan_parks_regional.zip",
  destfile = temp
)

parks_temp <- sf::read_sf(unzip(temp, "plan_parks_regional.gpkg")) %>%
  # filter(STATUS == "Existing") %>%
  mutate(AGENCY = recode(AGENCY, 
                         "Anoka County Parks" = "Anoka County",
                         "Anoka County Parks and Recreation" = "Anoka County",
                         "Bloomington Parks and Recreation" = "Bloomington",
                         "City of Bloomington" = "Bloomington",
                         "Carver County Parks" = "Carver County",
                         "Carver County Parks and Recreation" = "Carver County",
                         "Ramsey County Parks and Recreation" = "Ramsey County",
                         "Dakota County Parks" = "Dakota County",
                         "Minneapolis Park and Recreation Board" = "Minneapolis",
                         "Washington County Parks" = "Washington County",
                         "St Paul Parks And Recreation" = "St. Paul",
                         "St Paul Parks and Recreation" = "St. Paul",
                         "St. Paul Parks and Recreation" = "St. Paul",
                         "Scott County Parks" = "Scott County",
                         "Scott County/Three Rivers Park District" = "Scott County",#this is the Scott County Regional Trail
                         "Scott County Parks" = "Scott County", 
                         "Three Rivers Park District" = "Three Rivers")) %>%
  mutate(STATUS = recode(STATUS, "Existing" = "Park - existing",
                         "In Master Plan" = "Park - planned",
                         "Planned" = "Park - planned")) %>%
  group_by(PARKNAME, STATUS, Label, AGENCY) %>%
  summarize(do_union = TRUE) %>%
  ungroup() %>%
  select(
    name = Label,
    agency = AGENCY,
    status = STATUS,
  ) %>%
  st_transform(4326) %>%
  st_as_sf()

parks <- parks_temp %>%
  filter(status == "Park - existing")

parks_planned <- parks_temp %>%
  filter(status == "Park - planned")

fs::file_delete("plan_parks_regional.gpkg")

## Trails ----------------------------------------------------------------------
temp <- tempfile()
download.file("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/trans_regional_trails_exst_plan/gpkg_trans_regional_trails_exst_plan.zip",
  destfile = temp
)


trails_temp <- sf::read_sf(unzip(temp, "trans_regional_trails_exst_plan.gpkg")) %>%
  # filter(STATUS == "Existing (Open to Public)") %>%
  filter(NAME != "River Crossing") %>%
  mutate(Agency = recode(Agency, 
                         "Anoka County Parks" = "Anoka County",
                         "Anoka County Parks and Recreation" = "Anoka County",
                         "Bloomington Parks and Recreation" = "Bloomington",
                         "City of Bloomington" = "Bloomington",
                         "Carver County Parks" = "Carver County",
                         "Carver County Parks and Recreation" = "Carver County",
                         "Ramsey County Parks and Recreation" = "Ramsey County",
                         "Dakota County Parks" = "Dakota County",
                         "Minneapolis Park and Recreation Board" = "Minneapolis",
                         "Washington County Parks" = "Washington County",
                         "St Paul Parks And Recreation" = "St. Paul",
                         "St Paul Parks and Recreation" = "St. Paul",
                         "St. Paul Parks and Recreation" = "St. Paul",
                         "Scott County Parks" = "Scott County",
                         "Scott County/Three Rivers Park District" = "Scott County",#this is the Scott County Regional Trail
                         "Scott County Parks" = "Scott County", 
                         "Three Rivers Park District" = "Three Rivers")) %>%
  mutate(STATUS = recode(STATUS, "Existing (Open to Public)" = "Trail - existing",
                         "Alternate" = "Trail - planned/closed/alt.",
                         "Existing (Not Open to Public)" = "Trail - planned/closed/alt.",
                         "Planned" = "Trail - planned/closed/alt.")) %>%
  group_by(NAME, STATUS, Label, Agency) %>%
  summarize(do_union = TRUE) %>%
  ungroup() %>%
  select(
    name = NAME,
    agency = Agency,
    status = STATUS
  ) %>%

  st_transform(4326) %>%
  st_as_sf() %>%
  sf::st_make_valid()

trails <- trails_temp %>% filter(status == "Trail - existing")

trails_planned <- trails_temp %>% filter(status == "Trail - planned/closed/alt.")

fs::file_delete("trans_regional_trails_exst_plan.gpkg")


## Trail Search ----------------------------------------------------------------------
temp <- tempfile()
download.file("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/trans_regional_trails_search_cor/gpkg_trans_regional_trails_search_cor.zip",
              destfile = temp
)

trailsearch <- sf::read_sf(unzip(temp, "trans_regional_trails_search_cor.gpkg")) %>%
  group_by(NAME) %>%
  summarize(do_union = TRUE) %>%
  ungroup() %>%
  select(
    name = NAME,
  ) %>%
    mutate(status = "Trail - search") %>%
  st_transform(4326) %>%
  st_as_sf() %>%
  sf::st_make_valid()


fs::file_delete("trans_regional_trails_search_cor.gpkg")


## Park Search ----------------------------------------------------------------------
temp <- tempfile()
download.file("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/plan_parks_regional_search_areas/gpkg_plan_parks_regional_search_areas.zip",
              destfile = temp
)

parksearch <- sf::read_sf(unzip(temp, "plan_parks_regional_search_areas.gpkg")) %>%
group_by(NAME, Label) %>%
  summarize(do_union = TRUE) %>%
  ungroup() %>%
  select(
    name = NAME,
  ) %>%
  mutate(status = "Park - search") %>%
  st_transform(4326) %>%
  st_as_sf() %>%
  sf::st_make_valid()


fs::file_delete("plan_parks_regional_search_areas.gpkg")

## Combine ---------------------------------------------------------------------
park_trail_geog <- list(parks, parks_planned, trails, trails_planned,
                        trailsearch, parksearch)
names(park_trail_geog) <- c(
  "park",
  "park_planned",
  "trail",
  "trail_planned",
  "trail_search",
  "park_search"
)


usethis::use_data(park_trail_geog, overwrite = TRUE)
