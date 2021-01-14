## code to prepare `census_tract` dataset goes here

date <- format(Sys.time(), "%Y%m%d")
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

temp <- tempfile()
download.file("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/society_census_acs/xlsx_society_census_acs.zip",
  destfile = temp
)

ct <- readxl::read_xlsx(unzip(temp, "CensusACSTract.xlsx")) %>%
  janitor::clean_names() # %>%
# filter(tcflag == 1)

fs::file_delete("CensusACSTract.xlsx")


## ----------------------------------------------------------------------------------------------------------------------------------------------------
ct_foreign <- ct %>%
  select(geoid, geoid2, poptotal, usborncit, forborncit, forbornnot) %>%
  mutate(
    usborncit_percent = round(usborncit / poptotal, digits = 2),
    forborn_percent = round((forborncit + forbornnot) / poptotal, digits = 2) 
  )


## -----------------------------------------------------------------------------------------------------------------------------------------------------
ct_disability <- ct %>%
  select(geoid, geoid2, poptotal, anydis, ambdis, cdenom) %>%
  mutate(
    ambdis_percent = round(ambdis / cdenom, digits = 2),
    anydis_percent = round(anydis / cdenom, digits = 2) - ambdis_percent
  )


## ------------------------------------------------------------------------------------------------------------------------------------------------------
ct_merge <- right_join(ct_foreign, 
                       ct_disability)


## ----------------------------------------------------------------------------------------------------------------------------------------------------
MNtract <- tigris::tracts(
  state = "MN",
  county = c(
    "Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington",
    "Sherburne", "Isanti", "Chisago", "Goodhue", "Rice", "Le Sueur", "Sibley", "McLeod", "Wright"
  ),
  class = "sf"
) %>%
  select(GEOID)

WItract <- tigris::tracts(
  state = "WI",
  county = c("St. Croix", "Polk", "Pierce"),
  class = "sf"
) %>%
  select(GEOID)

census_tract_spatial <- bind_rows(MNtract, WItract) %>%
  left_join(ct_merge, by = c("GEOID" = "geoid2")) %>%
  st_transform(4326) # for leaflet

census_tract_raw <- census_tract_spatial %>%
  select(
    GEOID,
    "usborncit_percent",
    "forborn_percent",
    "anydis_percent",
    "ambdis_percent",
    geometry
  )

names(census_tract_raw) <- c(
  "GEOID",
  "Origin, US-born",
  "Origin, foreign-born",
  "Ability, any other disability",
  "Ability, ambulatory disability",
  "geometry"
)

county_outlines <- tigris::counties(
  state = "MN",
  class = "sf"
) %>%
  dplyr::filter(NAME %in% c(
    "Hennepin",
    "Dakota",
    "Carver",
    "Ramsey",
    "Anoka",
    "Scott",
    "Washington"
  )) %>%
  dplyr::select(NAME) %>%
  sf::st_transform(4326)

usethis::use_data(county_outlines, overwrite = TRUE)


usethis::use_data(census_tract_raw, overwrite = TRUE)
