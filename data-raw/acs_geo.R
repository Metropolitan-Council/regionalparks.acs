# script to filter the block groups so we will only graph geometries which are in 7 county core OR overlap with a buffer zone within a collar county
#
# load("./data/block_group_raw.rda")
# load("./data/census_tract_raw.rda")
# load("./data/collar_filter.rda")

library(tidyverse)
library(sf)

collar_co <- tibble(co = c("025", "049", "059", "079", "085", "131", "141", "143", "171", "109", "095", "093"))

# #core
# anoka, 003
# carver 19
# dakota 39
# hennepin 53
# ramsey 123
# scott 139
# washington 163
# #mn collar
# "Chisago", 025
# "Goodhue", 049
# "Isanti", 059
# "Le Sueur", 079
# "McLeod", 085
# "Rice", 131
# "Sherburne", 141
# "Sibley", 143
# "Wright", 171
# #wi collar
# "St. Croix", 109
# "Polk", 095
# "Pierce", 093


# block groups ----------
block_group <- block_group_raw %>%
  mutate(
    state = substr(GEOID, start = 1, stop = 2),
    county = substr(GEOID, start = 3, stop = 5),
    flag = if_else(county %in% collar_co$co, "collar", "core")
  ) %>%
  left_join(collar_filter %>% filter(bg_include == 1)) %>%
  filter(flag == "core" | (flag == "collar" & bg_include == 1)) # %>%
# select(-county, -flag, -bg_include, -tract_include)

usethis::use_data(block_group, overwrite = TRUE)

block_group_map <- block_group %>%
  mutate(across(c(hh_noveh, lep:othermultinh), ~ .x * 100,
    .names = "{.col}_percent"
  )) %>%
  select(-hh_noveh, -lep:-othermultinh) %>%
  rename(
    "meanhhinc_per" = "meanhhinc"
  )
usethis::use_data(block_group_map, overwrite = TRUE)

# block_group_map %>% #missing
# ggplot()+
# geom_sf(aes(fill = adj_novehicle_per))

# block_group %>% #missing
# ggplot()+
# geom_sf(aes(fill = novehicle_percent))

# block_group_raw %>% #missing
#   ggplot()+
#   geom_sf(aes(fill = novehicle_percent))
#

# census tracts ----------
census_tract <- census_tract_raw %>%
  mutate(
    county = substr(GEOID, start = 3, stop = 5),
    flag = if_else(county %in% collar_co$co, "collar", "core")
  ) %>%
  left_join(collar_filter %>% filter(tract_include == 1)) %>%
  filter(flag == "core" | (flag == "collar" & tract_include == 1)) %>%
  select(-county, -flag, -bg_include, -tract_include)

usethis::use_data(census_tract, overwrite = TRUE)


census_tract_map <- census_tract %>%
  mutate(
    disab_percent = anydis_percent * 100,
    ambdis_percent = ambdis_percent * 100,
    costburd_percent = pcostburdr * 100,
    usborn_percent = usborncit_percent * 100,
    forborn_percent = forborn_percent * 100
  ) %>%
  rename(
    "adj_anydis_per" = "disab_percent",
    "adj_ambdis_per" = "ambdis_percent",
    "adj_costburd_per" = "costburd_percent",
    "adj_usborn_per" = "usborn_percent",
    "adj_forborn_per" = "forborn_percent"
  )

usethis::use_data(census_tract_map, overwrite = TRUE)
