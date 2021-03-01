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
  mutate(
    ageunder15_percent = ageunder15_percent * 100,
    age15_24_percent = age15_24_percent * 100,
    age25_64_percent = age25_64_percent * 100,
    age65up_percent = age65up_percent * 100,
    whitenh_percent = whitenh_percent * 100,
    blacknh_percent = blacknh_percent * 100,
    asiannh_percent = asiannh_percent * 100,
    amindnh_percent = amindnh_percent * 100,
    othermultinh_percent = othermultinh_percent * 100,
    hisppop_percent = hisppop_percent * 100,
    nothisppop_percent = nothisppop_percent * 100,
    pov185_percent = pov185_percent * 100,
    novehicle_percent = novehicle_percent * 100,
    poorenglish_percent = poorenglish_percent * 100,
    spanish_percent = spanish_percent * 100
  ) %>%
  rename(
    "adj_ageunder15_per" = "ageunder15_percent",
    "adj_age15_24_per" = "age15_24_percent",
    "adj_age25_64_per" = "age25_64_percent",
    "adj_age65up_per" = "age65up_percent",
    "adj_whitenh_per" = "whitenh_percent",
    "adj_blacknh_per" = "blacknh_percent",
    "adj_asiannh_per" = "asiannh_percent",
    "adj_amindnh_per" = "amindnh_percent",
    "adj_othermultinh_per" = "othermultinh_percent",
    "adj_hisppop_per" = "hisppop_percent",
    "adj_nothisppop_per" = "nothisppop_percent",
    "adj_meanhhi" = "meanhhinc",
    "adj_185pov_per" = "pov185_percent",
    "adj_novehicle_per" = "novehicle_percent",
    "adj_lep_per" = "poorenglish_percent",
    "adj_span_per" = "spanish_percent"
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
    disab_percent = `Ability, any other disability` * 100,
    ambdis_percent = `Ability, ambulatory disability` * 100,
    costburd_percent = `Socioeconomic, housing cost burdened` * 100, 
    usborn_percent = `Origin, US-born` * 100,
    forborn_percent = `Origin, foreign-born` * 100
  ) %>%
  rename(
    "adj_anydis_per" = "disab_percent",
    "adj_ambdis_per" = "ambdis_percent",
    "adj_costburd_per" = "costburd_percent",
    "adj_usborn_per" = "usborn_percent",
    "adj_forborn_per" = "forborn_percent"
  )

usethis::use_data(census_tract_map, overwrite = TRUE)
