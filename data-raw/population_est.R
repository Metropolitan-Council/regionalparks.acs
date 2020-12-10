## code to prepare `pop_forecast` dataset goes here


# 2040 forecasts were adopted May 28, 2014 and updated July 8, 2015.
# Forecasts are periodically revised through Council action. This table includes all revisions through December 2019

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
library(readxl)
library(councilR)
library('viridis')

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

download.file("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/society_census2010tiger/shp_society_census2010tiger.zip",
              destfile = temp
)

unzip(zipfile = temp, exdir = temp2)
geo_land <- sf::read_sf(paste0(temp2, pattern = "/Census2010TigerBlock.shp"))

bg_area <- geo_land %>%
  mutate(bg_id = substr(GEOID10, start = 1, stop = 12)) %>%
  group_by(bg_id) %>%
  summarise(geometry = st_union(geometry),
            sum_aland = sum(ALAND10),
            sum_area = sum(as.numeric(Shape_Area)))

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

# taz_pop_2019_xlsx <- readxl::read_xlsx(unzip(temp, "SmallAreaEstimatesTAZ.xlsx")) %>%
#   janitor::clean_names() %>%
#   filter(
#     state == 27,
#     est_year == 2019
#   ) %>%
#   select(pop_est, taz_2010)

## QUESTION:  Should these population numbers be used as the 'population' by which the block-group buffer coverage gets multiplied? Of course, would necessarily need to keep the demographic calculations from the ACS pop estimates. But these small area estimates data might be more accurate? from metadata: "These data provide a more precise and timely picture of current conditions than the American Community Survey, another source of small area data...."


## ------

bg_pop2019 <- bg_area %>% 
  right_join(bg_pop_2019_xlsx, by = c("bg_id" = "bg10")) %>%
  mutate(popdens_2019_mi = round((pop_est / sum_area / 3.861022e-7), 0)) %>%#, #convert m2 to mi2
         # popdens_2019_mi = if_else(popdens_2019_mi > 60000, NA_real_, popdens_2019_mi)) %>% #UofM = unreasonable; warehouse district also too high to be reasonable. 
  st_transform(4326)  #for leaflet

# taz_pop_2019 <- taz_area %>% 
#   right_join(taz_pop_2019_xlsx, by = c("TAZ2012" = "taz_2010")) %>%
#   mutate(popdens_2019_mi = round((pop_est / sum_aland / 3.861022e-7), 0)) %>% #convert m2 to mi2
#   st_transform(4326) %>% #for leaflet
#   mutate(poptype = "taz_popdens_2019") %>%
#   rename(value = popdens_2019_mi) %>%
#   select(poptype, value)


# 2019 pop dens -----
pal <- colorBin("viridis",
  bins = c(0, 100, 500, 1000, 1500, 2000, 3000, 5000, 10000, 15000, 140000),
  domain = bg_pop2019$popdens_2019_mi)


leaflet() %>%
  setView(lat = 44.963, lng = -93.22, zoom = 10) %>%
  addProviderTiles("CartoDB.Positron",
                   group = "Carto Positron"
  ) %>%
  addPolygons(data = select(bg_pop2019, popdens_2019_mi),
              stroke = TRUE,
              color = councilR::colors$suppGray,
              opacity = 0.6,
              weight = 0.25,
              fillOpacity = 0.6,
              smoothFactor = 0.2,
              fillColor = ~ pal(bg_pop2019$popdens_2019_mi)) %>%
  addLegend("topright",
            pal = pal,
            values = bg_pop2019$popdens_2019_mi,
            title = "Estimated 2019 pop. density (people/mi2)",
            opacity = 1) 

# 2019 pop -----
pal <- colorBin("viridis",
                bins = c(0, 500, 1000, 1500, 2000, 3000, 5000, 10000),
                domain = bg_pop2019$pop_est)


leaflet() %>%
  setView(lat = 44.963, lng = -93.22, zoom = 10) %>%
  addProviderTiles("CartoDB.Positron",
                   group = "Carto Positron"
  ) %>%
  addPolygons(data = select(bg_pop2019, pop_est),
              stroke = TRUE,
              color = councilR::colors$suppGray,
              opacity = 0.6,
              weight = 0.25,
              fillOpacity = 0.6,
              smoothFactor = 0.2,
              fillColor = ~ pal(bg_pop2019$pop_est)) %>%
  addLegend("topright",
            pal = pal,
            values = bg_pop2019$pop_est,
            title = "Estimated 2019 pop. (people)",
            opacity = 1) 



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



#############
## demographic shifts
#############
#dl from here: https://metrocouncil.org/Data-and-Maps/Research-and-Data/Thrive-2040-Forecasts.aspx
shifts <- read_xlsx("/Users/escheh/Downloads/Regional-Forecast-Tables-(October-2019).xlsx",
                    sheet = "Population",
                    skip = 5) %>%
  filter(Region == "Minneapolis-Saint Paul Area",
         Units == "Individuals",
         Category == "Population")

totalpop <- shifts %>% filter(Race == "All Races",
                  Ages == "All Ages") %>%
  pivot_longer(cols = `2010`:`2050`, 
               names_to = "Year",
               values_to = "Pop") %>%
  group_by(Year) %>%
  summarise(TotalPop = sum(Pop))

raceshift <- shifts %>% filter(Race != "All Races",
                  Ages == "All Ages") %>%
  pivot_longer(cols = `2010`:`2050`, 
               names_to = "Year",
               values_to = "Pop") %>%
  group_by(Race, Ages, Year) %>%
  summarize(SubTotalPop = sum(Pop)) %>%
  left_join(totalpop) %>%
  mutate(Percent = SubTotalPop / TotalPop * 100)

ageshift <- shifts %>% filter(Race == "All Races",
                               Ages != "All Ages") %>%
  mutate(cat_age = case_when(Ages == "Ages 0-4" ~ "Ages 0-14",
                             Ages == "Ages 5-9" ~ "Ages 0-14",
                             Ages == "Ages 10-14"~ "Ages 0-14",
                             Ages == "Ages 15-19"~ "Ages 15-24",
                             Ages == "Ages 20-24"~ "Ages 15-24",
                             Ages == "Ages 25-29"~ "Ages 25-64",
                             Ages == "Ages 30-34"~ "Ages 25-64",
                             Ages == "Ages 35-39"~ "Ages 25-64",
                             Ages == "Ages 40-44"~ "Ages 25-64", 
                             Ages == "Ages 45-49"~ "Ages 25-64",
                             Ages == "Ages 50-54"~ "Ages 25-64",
                             Ages == "Ages 55-59"~ "Ages 25-64",
                             Ages == "Ages 60-64" ~ "Ages 25-64",
                             Ages == "Ages 65-69" ~ "Ages 65+",
                             Ages == "Ages 70-74" ~ "Ages 65+",
                             Ages == "Ages 75-79" ~ "Ages 65+",
                             Ages == "Ages 80-84" ~ "Ages 65+",
                             Ages == "Ages 85+" ~ "Ages 65+")) %>%
  pivot_longer(cols = `2010`:`2050`, 
               names_to = "Year",
               values_to = "Pop") %>%
  group_by(Race, cat_age, Year) %>%
  summarize(SubTotalPop = sum(Pop)) %>%
  left_join(totalpop) %>%
  mutate(Percent = SubTotalPop / TotalPop * 100)


##########
# Figs
##########

## race ---------

# this is how metrostats presents data: https://metrocouncil.org/Data-and-Maps/Publications-And-Resources/MetroStats/Land-Use-and-Development/Steady-Growth-and-Big-Changes-Ahead-Regional-Forec.aspx
raceshift %>%
  mutate(Race = factor(Race, levels = c("Latinx",
                                        "Asian and Other Races",
                                        "Black-NonLatinx",
                                        "White-NonLatinx"
                                        ))) %>%
  ggplot(aes(x = Year, y = SubTotalPop, fill = Race)) +
  geom_bar(stat = "identity",
           color = "black") +
  council_theme() +
  labs(x = "Forecast year",
       y = "Forecasted population",
       fill = "Race & Ethnicity") +
  scale_fill_brewer(palette = "Blues")

# but I like this
raceshift %>%
  mutate(Race = factor(Race, levels = c("Latinx",
                                        "Asian and Other Races",
                                        "Black-NonLatinx",
                                        "White-NonLatinx"
  ))) %>%
  ggplot(aes(x = Year, y = Percent, fill = Race)) +
  geom_bar(stat = "identity",
           col = "black") +
  council_theme() +
  labs(x = "Forecast year",
       y = "Forecasted % of total population",
       fill = "Race & Ethnicity") +
  scale_fill_brewer(palette = "Blues")


## age ---------------
# metrostas way: https://metrocouncil.org/Data-and-Maps/Publications-And-Resources/MetroStats/Land-Use-and-Development/Steady-Growth-and-Big-Changes-Ahead-Regional-Forec.aspx
# COOL - they use the same age breakdowns as we are using here
ageshift %>%
  mutate(cat_age = factor(cat_age, levels = c("Ages 65+",
                                        "Ages 25-64",
                                        "Ages 15-24",
                                        "Ages 0-14"
  ))) %>%
  ggplot(aes(x = Year, y = SubTotalPop, fill = cat_age)) +
  geom_bar(stat = "identity",
           color = "black") + 
  council_theme() +
  labs(x = "Forecast year",
       y = "Forecasted population",
       fill = "Age") +
  scale_fill_brewer(palette = "Greens")

# ee preference
ageshift %>%
  mutate(cat_age = factor(cat_age, levels = c("Ages 65+",
                                              "Ages 25-64",
                                              "Ages 15-24",
                                              "Ages 0-14"
  ))) %>%
  ggplot(aes(x = Year, y = Percent, fill = cat_age)) +
  geom_bar(stat = "identity",
           color = "black") + 
  council_theme() +
  labs(x = "Forecast year",
       y = "Forecasted % of total population",
       fill = "Age") +
  scale_fill_brewer(palette = "Greens")

## -------

usethis::use_data(taz_growth, overwrite = TRUE)
