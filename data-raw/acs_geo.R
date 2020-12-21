# script to filter the block groups so we will only graph geometries which are in 7 county core OR overlap with a buffer zone within a collar county

load("./data/block_group_raw.rda")
load("./data/census_tract_raw.rda")
load("./data/collar_filter.rda")
# load("./data/park_trail_geog.rda")

library(tidyverse)
library(sf)



collar_co <- tibble(co = c(025, 049, 059, 079, 085, 131, 141, 143, 171, 109, 095, 093))

# block groups ----------
block_group <- block_group_raw %>%
  mutate(county = substr(GEOID, start = 3, stop = 5),
         flag = if_else(county %in% collar_co$co, "collar", "core")) %>%
  left_join(collar_filter %>% filter(bg_include == 1)) %>%
  filter(flag == "core" | (flag == "collar" & bg_include == 1)) %>%
  select(-county, -flag, -bg_include, -tract_include)

usethis::use_data(block_group, overwrite = TRUE)


# census tracts ----------
census_tract <- census_tract_raw %>%
  mutate(county = substr(GEOID, start = 3, stop = 5),
         flag = if_else(county %in% collar_co$co, "collar", "core")) %>%
  left_join(collar_filter %>% filter(tract_include == 1)) %>%
  filter(flag == "core" | (flag == "collar" & tract_include == 1)) %>%
  select(-county, -flag, -bg_include, -tract_include)

usethis::use_data(census_tract, overwrite = TRUE)

