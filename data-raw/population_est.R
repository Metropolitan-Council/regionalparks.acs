## code to prepare `pop_forecast` dataset goes here

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


############
## geography information
############

# need this info to calculate population density ON LAND (i.e. exculding water area w/in geographies)
# also need this info to assign blocks into TAZs (to get the area)

temp <- tempfile()
temp2 <- tempfile()

download.file("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/society_census2010tiger/shp_society_census2010tiger.zip",
              destfile = temp
)

unzip(zipfile = temp, exdir = temp2)
geo_land <- sf::read_sf(paste0(temp2, pattern = "/Census2010TigerBlock.shp"))

bg_area <- geo_land %>% 
  mutate(bg_id = substr(GEOID10, start = 1, stop = 12)) %>% 
  group_by(bg_id) %>%
  summarise(geometry = st_union(geometry),
            sum_aland = sum(ALAND10)) 

taz_area <- geo_land %>% 
  group_by(TAZ2012) %>%
  summarise(geometry = st_union(geometry),
            sum_aland = sum(ALAND10)) #2010 Census land area (square meters)
  

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
    state == 27,
    est_year == 2019
  ) %>%
  select(pop_est, bg10)

taz_pop_2019_xlsx <- readxl::read_xlsx(unzip(temp, "SmallAreaEstimatesTAZ.xlsx")) %>%
  janitor::clean_names() %>%
  filter(
    state == 27,
    est_year == 2019
  ) %>%
  select(pop_est, taz_2010)

## QUESTION:  Should these population numbers be used as the 'population' by which the block-group buffer coverage gets multiplied? Of course, would necessarily need to keep the demographic calculations from the ACS pop estimates. But these small area estimates data might be more accurate? from metadata: "These data provide a more precise and timely picture of current conditions than the American Community Survey, another source of small area data...."


## ------

bg_pop_2019 <- bg_area %>% 
  right_join(bg_pop_2019_xlsx, by = c("bg_id" = "bg10")) %>%
  mutate(popdens_2019_mi = round((pop_est / sum_aland / 3.861022e-7), 0), #convert m2 to mi2
         popdens_2019_mi = if_else(popdens_2019_mi > 60000, NA_real_, popdens_2019_mi)) %>% #UofM = unreasonable; warehouse district also too high to be reasonable. 
  st_transform(4326) %>% #for leaflet
  mutate(poptype = "bg_popdens_2019") %>%
  rename(value = popdens_2019_mi) %>%
  select(poptype, value)

taz_pop_2019 <- taz_area %>% 
  right_join(taz_pop_2019_xlsx, by = c("TAZ2012" = "taz_2010")) %>%
  mutate(popdens_2019_mi = round((pop_est / sum_aland / 3.861022e-7), 0)) %>% #convert m2 to mi2
  st_transform(4326) %>% #for leaflet
  mutate(poptype = "taz_popdens_2019") %>%
  rename(value = popdens_2019_mi) %>%
  select(poptype, value)

# taz_pop_2019 %>%
#   ggplot() +
#   geom_sf(aes(fill = value))


pop_data <- rbind(bg_pop_2019, 
                  taz_pop_2019)

# usethis::use_data(pop_data, overwrite = TRUE)



##############
### Population forecasts
#############

temp <- tempfile()
download.file("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/trans_anlys_zones_offical_curent/gpkg_trans_anlys_zones_offical_curent.zip",
  destfile = temp
)


taz_growth <- sf::read_sf(unzip(temp, "trans_anlys_zones_offical_curent.gpkg")) %>%
  filter(TCFLAG == 1) %>%
  select(
    TAZ,
    POP2020, POP2030, POP2040
  ) %>%
  st_transform(4326) %>%
  st_as_sf() %>%
  mutate(
    growth_abs_20_40 = POP2040 - POP2020,
    growth_rel_20_40 = POP2040 / POP2020
  ) %>%
  mutate(growth_abs_cat = case_when(
    growth_abs_20_40 < 100 ~ "<100",
    growth_abs_20_40 < 500 ~ "100-500",
    growth_abs_20_40 < 1000 ~ "500-1000",
    growth_abs_20_40 >= 1000 ~ "1000+"
  )) # ,
# taz_area = st_area(.),
# density2040 = POP2040 / taz_area)

taz_growth %>%
  as_tibble() %>%
  summarise(db = quantile(growth_abs_20_40, probs = c(.25, .5, .75, 1)))

fs::file_delete("trans_anlys_zones_offical_curent.gpkg")

## -------

usethis::use_data(taz_growth, overwrite = TRUE)
