## code to prepare `park_trail_geog` dataset goes here

pkgload::load_all()
library(dplyr)
library(fs)
library(sf)

## Parks -----------------------------------------------------------------------
temp <- tempfile()
download.file("ftp://ftp.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/plan_parks_regional/gpkg_plan_parks_regional.zip",
  destfile = temp
)

parks <- sf::read_sf(unzip(temp, "plan_parks_regional.gpkg")) %>% 
  filter(STATUS == "Existing") %>%
  group_by(PARKNAME, Label, AGENCY) %>%
  summarize(do_union = TRUE) %>%
  ungroup() %>%
  select(
    name = Label,
    agency = AGENCY
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
  filter(STATUS == "Existing (Open to Public)") %>%
  group_by(NAME, Label, Agency) %>%
  summarize(do_union = TRUE) %>%
  ungroup() %>%
  select(
    name = Label,
    agency = Agency
  ) %>%
  st_transform(4326) %>%
  st_as_sf()


fs::file_delete("trans_regional_trails_exst_plan.gpkg")

## Combine ---------------------------------------------------------------------
park_trail_geog <- list(parks, trails)
names(park_trail_geog) <- c(
  "park",
  "trail"
)


usethis::use_data(park_trail_geog, overwrite = TRUE)
