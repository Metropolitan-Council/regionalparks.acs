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
  ~AGENCY, ~consistentagency,
  "Anoka County Parks and Recreation", "Anoka County",
  "Anoka County Parks", "Anoka County",
  "Anoka County", "Anoka County",
  "Bloomington Parks and Recreation", "Bloomington",
  "Bloomington", "Bloomington",
  "City of Bloomington", "Bloomington",
  "Carver County Parks and Recreation", "Carver County",
  "Carver County Parks", "Carver County",
  "Carver County", "Carver County",
  "Ramsey County Parks and Recreation", "Ramsey County",
  "Ramsey County", "Ramsey County",
  "Dakota County Parks", "Dakota County",
  "Dakota County", "Dakota County",
  "Minneapolis Park and Recreation Board", "MPRB",
  "Minneapolis", "MPRB",
  "Washington County Parks", "Washington County",
  "Washington County", "Washington County",
  "St. Paul Parks and Recreation", "St. Paul",
  "St Paul Parks And Recreation", "St. Paul",
  "St Paul Parks and Recreation", "St. Paul",
  "St. Paul", "St. Paul",
  "Scott County / Three Rivers Park District", "Scott County", # this is the Scott County Regional Trail
  "Scott County/Three Rivers Park District", "Scott County",
  "Scott County Parks", "Scott County",
  "Scott County", "Scott County",
  "Three Rivers Park District", "Three Rivers",
  "Three Rivers", "Three Rivers"
)

## Parks -----------------------------------------------------------------------
temp <- tempfile()
download.file("ftp://ftp.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/plan_parks_regional/gpkg_plan_parks_regional.zip",
  destfile = temp
)

parks_temp <- sf::read_sf(unzip(temp, "plan_parks_regional.gpkg")) %>%
  # filter(STATUS == "Existing") %>%
  left_join(namecleaner) %>%
  mutate(STATUS = recode(STATUS,
    "Existing" = "Park - existing",
    "In Master Plan" = "Park - planned",
    "Planned" = "Park - planned"
  )) %>%
  mutate(
    consistentagency = if_else((PARKNAME == "Cleary Lake" |
      PARKNAME == "Murphy-Hanrehan" |
      # PARKNAME == "Spring Lake" |
      PARKNAME == "Cedar Lake Farm"), "Scott County", consistentagency),
    consistentagency = if_else(Label == "Spring Lake Regional Park", "Scott County", consistentagency)
  ) %>% # becuase Spr.Lake Park Reserve == Dakota, but S.L. Reg. Park = Scott
  group_by(PARKNAME, STATUS, Label, consistentagency) %>%
  summarize(do_union = TRUE) %>%
  ungroup() %>%
  select(
    name = Label,
    agency = consistentagency,
    status = STATUS,
  ) %>%
  st_transform(4326) %>%
  st_as_sf()

parks <- parks_temp %>%
  filter(status == "Park - existing") %>%
  mutate(
    status2 = "Existing",
    Type = "Park"
  )

parks_planned <- parks_temp %>%
  filter(status == "Park - planned") %>%
  mutate(
    status2 = "Planned",
    Type = "Park"
  )

fs::file_delete("plan_parks_regional.gpkg")

## Trails ----------------------------------------------------------------------
temp <- tempfile()
download.file("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/trans_regional_trails_exst_plan/gpkg_trans_regional_trails_exst_plan.zip",
  destfile = temp
)


trails_temp <- sf::read_sf(unzip(temp, "trans_regional_trails_exst_plan.gpkg")) %>%
  # filter(STATUS == "Existing (Open to Public)") %>%
  filter(
    NAME != "River Crossing",
    Agency != "Wright County"
  ) %>% # Crow River Regional Trail doesn't seem to belong to any particular Metro Agency
  mutate(STATUS = recode(STATUS,
    "Existing (Open to Public)" = "Trail - existing",
    "Alternate" = "Trail - planned",
    "Existing (Not Open to Public)" = "Trail - planned",
    "Planned" = "Trail - planned"
  )) %>%
  rename(AGENCY = Agency) %>%
  left_join(namecleaner) %>%
  # mutate(consistentagency = ifelse(name == "Scott County Regional Trail" & is.na(consistentagency),
  #                                  "Scott County Parks", consistentagency)) %>% #should confirm this is the right agency
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
  sf::st_make_valid()

trails <- trails_temp %>%
  filter(status == "Trail - existing") %>%
  mutate(
    status2 = "Existing",
    Type = "Trail"
  )

trails_planned <- trails_temp %>%
  filter(status == "Trail - planned") %>%
  mutate(
    status2 = "Planned",
    Type = "Trail"
  )

fs::file_delete("trans_regional_trails_exst_plan.gpkg")


## Trail Search ----------------------------------------------------------------------
temp <- tempfile()
download.file("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/trans_regional_trails_search_cor/gpkg_trans_regional_trails_search_cor.zip",
  destfile = temp
)

trailsearch <- sf::read_sf(unzip(temp, "trans_regional_trails_search_cor.gpkg")) %>%
  left_join(namecleaner) %>%
  group_by(NAME, consistentagency) %>%
  summarize(do_union = TRUE) %>%
  ungroup() %>%
  select(
    name = NAME,
    agency = consistentagency,
  ) %>%
  mutate(status = "Trail - search") %>%
  st_transform(4326) %>%
  st_as_sf() %>%
  sf::st_make_valid() %>%
  mutate(
    status2 = "Search",
    Type = "Trail"
  )


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
  sf::st_make_valid() %>%
  mutate(
    status2 = "Search",
    Type = "Park"
  )


fs::file_delete("plan_parks_regional_search_areas.gpkg")

## Combine ---------------------------------------------------------------------
park_trail_geog_LONG <- bind_rows(
  parks, parks_planned, trails, trails_planned,
  trailsearch, parksearch
) %>% 
  mutate(popup_text = paste0(
    "<b>", status, "</b>", "<br>",
    name, "<br>", "<em>",
    agency, "</em>"
  ))

usethis::use_data(park_trail_geog_LONG, overwrite = TRUE)

# usethis::use_git_ignore(".DS_Store")
