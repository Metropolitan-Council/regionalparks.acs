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

parks <- sf::read_sf(unzip(temp, "plan_parks_regional.gpkg")) %>%
  # filter(STATUS == "Existing") %>%
  group_by(PARKNAME, STATUS, Label, AGENCY) %>%
  summarize(do_union = TRUE) %>%
  ungroup() %>%
  mutate(STATUS = recode(STATUS, "Existing" = "Park - existing",
                         "In Master Plan" = "Park - planned",
                         "Planned" = "Park - planned")) %>%
  select(
    name = Label,
    agency = AGENCY,
    status = STATUS,
  ) %>%
  st_transform(4326) %>%
  st_as_sf()

fs::file_delete("plan_parks_regional.gpkg")

## Trails ----------------------------------------------------------------------
temp <- tempfile()
download.file("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/trans_regional_trails_exst_plan/gpkg_trans_regional_trails_exst_plan.zip",
  destfile = temp
)


trails <- sf::read_sf(unzip(temp, "trans_regional_trails_exst_plan.gpkg")) %>%
  # filter(STATUS == "Existing (Open to Public)") %>%
  group_by(NAME, STATUS, Label, Agency) %>%
  summarize(do_union = TRUE) %>%
  ungroup() %>%
  mutate(STATUS = recode(STATUS, "Existing (Open to Public)" = "Trail - existing",
                         "Alternate" = "Trail - planned/closed/alt.",
                         "Existing (Not Open to Public)" = "Trail - planned/closed/alt.",
                         "Planned" = "Trail - planned/closed/alt.")) %>%
  select(
    name = NAME,
    agency = Agency,
    status = STATUS
  ) %>%

  st_transform(4326) %>%
  st_as_sf() %>%
  sf::st_make_valid()


fs::file_delete("trans_regional_trails_exst_plan.gpkg")


## Trail Search ----------------------------------------------------------------------
temp <- tempfile()
download.file("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/trans_regional_trails_search_cor/gpkg_trans_regional_trails_search_cor.zip",
              destfile = temp
)

trailsearch <- sf::read_sf(unzip(temp, "trans_regional_trails_search_cor.gpkg"))# %>%
  group_by(NAME, Label, Agency) %>%
  summarize(do_union = TRUE) %>%
  ungroup() %>%
  select(
    name = NAME,
    agency = Agency
  ) %>%
    mutate(Status = "Trail - search") %>%
  st_transform(4326) %>%
  st_as_sf() %>%
  sf::st_make_valid()


fs::file_delete("trans_regional_trails_search_cor.gpkg")


## Park Search ----------------------------------------------------------------------
temp <- tempfile()
download.file("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/plan_parks_regional_search/gpkg_plan_parks_regional_search.zip",
              destfile = temp
)

parksearch <- sf::read_sf(unzip(temp, "plan_parks_regional_search.gpkg"))# %>%
group_by(NAME, Label, Agency) %>%
  summarize(do_union = TRUE) %>%
  ungroup() %>%
  select(
    name = NAME,
    agency = Agency
  ) %>%
  mutate(Status = "Park - search") %>%
  st_transform(4326) %>%
  st_as_sf() %>%
  sf::st_make_valid()


fs::file_delete("plan_parks_regional_search.gpkg")

## Combine ---------------------------------------------------------------------
park_trail_geog <- list(parks, trails, 
                        trailsearch, parksearch)
names(park_trail_geog) <- c(
  "park",
  "trail",
  "trail_search",
  "park_search"
)


usethis::use_data(park_trail_geog, overwrite = TRUE)
