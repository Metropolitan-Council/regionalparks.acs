## code to prepare `pop_forecast` dataset goes here


# 2040 forecasts were adopted May 28, 2014 and updated July 8, 2015.
# Forecasts are periodically revised through Council action. This table includes all revisions through December 2019

requireNamespace("readxl", quietly = TRUE)
requireNamespace("fs", quietly = TRUE)
requireNamespace("tigris", quietly = TRUE)
requireNamespace("janitor", quietly = TRUE)

library(dplyr)
library(fs)
library(sf)
library(tigris)
library(janitor)
library(readxl)
library(councilR)
library("viridis")

# ############
# ## geography information
# ############
#
# UPDATE: DO NOT WANT TO CALCULATE DENSITY BY LAND AREA. PREFERENCE TO STAY MORE TRUE TO THE PUBLISHED DATA AND LIMIT ADDITIONAL "NEW" ANALYSES (even tho information is housed together, it still aggregates multiple data sources).
# UNFORTUNATELY: we still need this geographic information if we want to calculate pop density. So leaving in for the time being.

# need this info to calculate population density ON LAND (i.e. exculding water area w/in geographies)
# also need this info to assign blocks into TAZs (to get the area)

temp <- tempfile()
temp2 <- tempfile()
options(timeout = 200)

download.file("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/society_census2010tiger/shp_society_census2010tiger.zip", destfile = temp)

unzip(zipfile = temp, exdir = temp2)
geo_land <- sf::read_sf(paste0(temp2, pattern = "/Census2010TigerBlock.shp"))

bg_area <- geo_land %>%
  mutate(bg_id = substr(GEOID10, start = 1, stop = 12)) %>%
  group_by(bg_id) %>%
  summarise(
    geometry = st_union(geometry),
    sum_aland = sum(ALAND10), # 2010 Census land area (square meters)
    sum_area = sum(as.numeric(Shape_Area))
  )

# taz_area <- geo_land %>%
#   group_by(TAZ2012) %>%
#   summarise(
#     geometry = st_union(geometry),
#     sum_aland = sum(ALAND10), # 2010 Census land area (square meters)
#     sum_area = sum(as.numeric(Shape_Area))
#   )


##############
### Current Population
#############
temp <- tempfile()
download.file("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/society_small_area_estimates/xlsx_society_small_area_estimates.zip",
  destfile = temp
)

bg_pop_2019_xlsx <- readxl::read_xlsx(unzip(temp, "SmallAreaEstimatesBlockGroup.xlsx")) %>%
  janitor::clean_names() %>%
  filter(
    # state == 27, #contains ONLY metcouncil area
    est_year == 2019
  ) %>%
  select(pop_est, hh_est, bg10)

## ------

bg_pop2019 <- bg_area %>%
  right_join(bg_pop_2019_xlsx, by = c("bg_id" = "bg10")) %>%
  mutate(popdens_2019_mi = round((pop_est / sum_area / 3.861022e-7), 0)) %>% # , #convert m2 to mi2
  # would love to calculate population density over LAND AREA ONLY (use `sum_aland` rather than `sum_area`, however we'd need to mask lakes/river then, and then we should update our buffer "coverage" with water masks too (ie what if 50% of total block area falls w/in buffer zone, but that is only 70% of block land area.?)) In essence, this is just a slippery slope (do we mask non-residential areas? protected wetlands?), so its probably best to keep as simple as we can (dennis convo)
  # popdens_2019_mi = if_else(popdens_2019_mi > 60000, NA_real_, popdens_2019_mi)) %>% #UofM = unreasonable; warehouse district also too high to be reasonable.
  rename(
    PopEst_2019 = pop_est,
    HHEst_2019 = hh_est,
    PopDens_2019 = popdens_2019_mi
  ) %>%
  st_transform(4326) # for leaflet



##############
### Population forecasts
# not currently online, so waiting on this....
#############

temp <- tempfile()
download.file("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/trans_anlys_zones_offical_curent/gpkg_trans_anlys_zones_offical_curent.zip",
  destfile = temp
)


taz_growth <- sf::read_sf(unzip(temp, "trans_anlys_zones_offical_curent.gpkg")) %>%
  # filter(TCFLAG == 1) %>%
  select(
    TAZ,
    POP2010, POP2040,
    Shape_Area
  ) %>%
  rename(TAZ2012 = TAZ) %>%
  arrange(TAZ2012) %>%
  mutate(
    growth_abs_10_40 = POP2040 - POP2010,
    growth_rel_10_40 = if_else(POP2010 > 50, POP2040 / POP2010, NA_real_) # per dennis suggestion, doesn't really make sense to look at this if only a couple families are coming/going
  ) %>%
  mutate(growth_rel_10_40 = if_else(growth_rel_10_40 == "Inf", NA_real_, growth_rel_10_40)) %>%
  mutate(growth_abs_cat = case_when(
    growth_abs_10_40 < 100 ~ "<100",
    growth_abs_10_40 < 500 ~ "100-499",
    growth_abs_10_40 < 1000 ~ "500-999",
    growth_abs_10_40 >= 1000 ~ "1000+"
  )) %>%
  mutate(growth_rel_cat = case_when(
    growth_rel_10_40 < 1 ~ "<1",
    growth_rel_10_40 < 1.25 ~ "1-1.24",
    growth_rel_10_40 < 1.5 ~ "1.25-1.49",
    growth_rel_10_40 >= 1.5 ~ "1.5+"
  )) %>%
  # left_join(taz_area %>% st_drop_geometry()) %>% #shape_area in growth forcast, and uom: m
  mutate(popdens_2040_mi = round((POP2040 / Shape_Area / 3.861022e-7), 0)) %>% # , #convert m2 to mi2
  mutate(dens40_cat = case_when(
    popdens_2040_mi < 100 ~ "<100 ppl/mi",
    popdens_2040_mi < 1000 ~ "100-999 ppl/mi",
    popdens_2040_mi < 2000 ~ "1,000-1,999 ppl/mi",
    popdens_2040_mi < 5000 ~ "2,000-4,999 ppl/mi",
    popdens_2040_mi >= 5000 ~ ">5,000 ppl/mi"
  )) %>%
  st_transform(4326) %>% # for leaflet
  st_as_sf() %>%
  rename(geometry = geom) %>%
  select(-Shape_Area)

taz_growth %>% ggplot() +
  geom_sf(aes(fill = dens40_cat))

taz_growth %>%
  as_tibble() %>%
  summarise(
    abs = quantile(growth_abs_10_40, probs = c(0, .2, .4, .6, .8, 1)),
    rel = quantile(growth_rel_10_40, probs = c(0, .2, .4, .6, .8, 1), na.rm = T),
    pop = quantile(popdens_2040_mi, probs = c(0, .2, .4, .6, .8, 1), na.rm = T)
  )

fs::file_delete("trans_anlys_zones_offical_curent.gpkg")


#######
# combine
#####
est_pop <- bg_pop2019 %>%
  bind_rows(taz_growth)

usethis::use_data(est_pop, overwrite = TRUE)
