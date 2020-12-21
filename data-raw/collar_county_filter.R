# we want to include demographic info on tracts + block groups which are in WI but overlap with the largest buffer zones. 


load("./data/block_group_raw.rda")
load("./data/census_tract.rda")
load("./data/park_trail_geog_LONG.rda")

library(tidyverse)
library(sf)



# read in data -------------------------
park_trail_geog_temp <-  park_trail_geog_LONG %>% #bind_rows(park_trail_geog, .id = "status") %>%
  mutate(
    # name = paste(name, num, sep = "_"),
    type = if_else(status == "park" |
                     status == "park_planned" |
                     status == "park_search", "Park", "Trail"),
    status = case_when(
      status == "park" | status == "trail" ~ "Existing",
      status == "park_planned" | status == "trail_planned" ~ "Planned",
      status == "park_search" | status == "trail_search" ~ "Search"
    )
    )%>%
  st_transform(3857) # https://epsg.io/3857\

acs_temp_bg <- block_group_raw %>%
  mutate(county = substr(GEOID, start = 3, stop = 5)) %>%
  st_transform(3857) %>% # https://epsg.io/3857\
  mutate(bg_area = st_area(.))

acs_temp_tract <- census_tract %>%
  mutate(county = substr(GEOID, start = 3, stop = 5)) %>%
  st_transform(3857) %>% # https://epsg.io/3857\
  mutate(tract_area = st_area(.))


# helper fxns -------------------------
buffer_dist_fxn <- function(miles) { # create buffer geometry of x distance
  buff_Xmi <- park_trail_geog_temp %>%
    st_buffer(dist = 1609.34 * miles) %>% # the 3857 projection uses meters as a distance, so 1.0 mi =
    group_by(agency, name, type, status) %>%
    summarise(geometry = st_union(geom))
  return(buff_Xmi)
}

bg_buffer_acs_fxn <- function(df) { # intersect the buffer of x distance with the acs demographics
  buff_acs <- acs_temp_bg %>%
    select(GEOID, bg_area) %>%
    st_intersection(df) %>% # every trail name gets own intersection
    mutate(intersect_area = st_area(.)) %>% # create new column with shape area
    select(GEOID, agency, name, type, status, intersect_area) %>% # only select columns needed to merge
    st_drop_geometry() %>% # drop geometry as we don't need it
    left_join(acs_temp_bg %>% # merge back in with all block groups
                select(GEOID, bg_area)) %>%
    mutate(coverage = as.numeric(intersect_area / bg_area)) %>% # calculate fraction of block group within each agency/park buffer
    as_tibble() %>%
    select(GEOID, agency, name, type, status, coverage)
  return(buff_acs)
}


tract_buffer_acs_fxn <- function(df) { # intersect the buffer of x distance with the acs demographics
  buff_acs <- acs_temp_tract %>%
    select(GEOID, tract_area) %>%
    st_intersection(df) %>% # every trail name gets own intersection
    mutate(intersect_area = st_area(.)) %>% # create new column with shape area
    select(GEOID, agency, name, type, status, intersect_area) %>% # only select columns needed to merge
    st_drop_geometry() %>% # drop geometry as we don't need it
    left_join(acs_temp_tract %>% # merge back in with all block groups
                select(GEOID, tract_area)) %>%
    mutate(coverage = as.numeric(intersect_area / tract_area)) %>% # calculate fraction of block group within each agency/park buffer
    as_tibble() %>%
    select(GEOID, agency, name, type, status, coverage)
  return(buff_acs)
}


## collar county filter -------------------------
# create filter for collar county block groups/tracts which have > 0 coverage of the largest buffer distance
collar_co <- tibble(co = c("025", "049", "059", "079", "085", "131", "141", "143", "171", "109", "095", "093"))
# "Chisago", 025
# "Goodhue", 049
# "Isanti", 059
# "Le Sueur", 079
# "McLeod", 085
# "Rice", 131
# "Sherburne", 141
# "Sibley", 143
# "Wright", 171
# 
# "St. Croix", 109 
# "Polk", 095
# "Pierce", 093



buff_3mi <- buffer_dist_fxn(3)

bg_intersect_pct_3mi <- bg_buffer_acs_fxn(buff_3mi)

bg_include <- bg_intersect_pct_3mi %>%
  mutate(county = substr(GEOID, start = 3, stop = 5)) %>% 
  filter(county %in% collar_co$co) %>%
  group_by(GEOID) %>%
  summarise(bg_include = 1)
  
tract_intersect_pct_3mi <- tract_buffer_acs_fxn(buff_3mi)

tract_include <- tract_intersect_pct_3mi %>%
  mutate(county = substr(GEOID, start = 3, stop = 5)) %>% 
  filter(county %in% collar_co$co) %>%
  group_by(GEOID) %>%
  summarise(tract_include = 1)


collar_filter <- bind_rows(bg_include, tract_include)
  
usethis::use_data(collar_filter, overwrite = TRUE)
