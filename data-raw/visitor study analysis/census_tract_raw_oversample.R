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

options(tigris_use_cache = TRUE)

temp <- tempfile()
download.file(
  "https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/society_census_acs/xlsx_society_census_acs.zip",
  destfile = temp
)

ct <- readxl::read_xlsx(unzip(temp, "CensusACSTract.xlsx")) %>%
  janitor::clean_names() # %>%
# filter(tcflag == 1)

fs::file_delete("CensusACSTract.xlsx")



## -----------------------------------------------------------------------------------------------------------------------------------------------------
census_demos <- ct %>%
  # select(geoid, geoid2, poptotal, anydis, ambdis, cdenom) %>%
  transmute(
    geoid = geoid,
    geoid2 = geoid2,
    poptotal = poptotal,
    ambdis_percent = (ambdis / cdenom),
    
    anydis_percent = (anydis / cdenom),
    nodis_percent = 1 - anydis_percent,

    pop_1824 = ((popover18 - (age65up + age40_64 + 
                                     m_25_29 + f_25_29 +
                                     m_30_34 + f_30_34 +
                                     m_35_39 + f_35_39
                                     ))/poptotal ),
    pop_1217 = ( ((m_10_14 + m_15_19 + f_10_14 + f_15_19 ) / 2) / poptotal),
    pop2534 = ( (m_25_29 + m_30_34 + f_25_29 + f_30_34)/ poptotal),
    pop_3544 = ( (m_35_39 + m_40_44 + f_35_39 + f_40_44)/ poptotal),
    pop_4554 = ( (m_45_49 + m_50_54 + f_45_49 + f_50_54)/ poptotal),
    pop_5564 = ( (m_55_59 + m_60_64 + f_55_59 + f_60_64)/ poptotal),
    pop_6574 = ( (m_65_69 + m_70_74 + f_65_69 + f_70_74) / poptotal),
    pop_75up = ( (m_75_79 + m_80_84 + m_over85 + f_75_79 + f_80_84 + f_over85) / poptotal),
    
    pop_over18 = popover18,
    
    lesshs = lesshs / poptotal,
    highschool = highschool / poptotal,
    somecolleg = somecolleg / poptotal,
    associate = associate / poptotal,
    bachelors = bachelors / poptotal,
    gradprof = gradprof / poptotal,
    
    racewhite = whitenh / poptotal,
    raceblack = blacknh / poptotal,
    raceamind = amindnh / poptotal,
    raceasian = asiannh / poptotal,
    racemulti = multracenh / poptotal,
    racepacific = pacificnh / poptotal,
    raceother = othernh / poptotal,
    racehisp = hisppop / poptotal,
    
    langenglish = english / poptotal,
    langspanish = lep_span / poptotal,
    # langsomali = lep_africa / poptotal,
    # langhmong = lep_hmong / poptotal,
    langother = (poptotal - english - lep_span) / poptotal
  )



## ----------------------------------------------------------------------------------------------------------------------------------------------------
MNtract <- tigris::tracts(
  year = 2019,
  state = "MN",
  county = c(
    "Anoka",
    "Carver",
    "Dakota",
    "Hennepin",
    "Ramsey",
    "Scott",
    "Washington",
    "Sherburne",
    "Isanti",
    "Chisago",
    "Goodhue",
    "Rice",
    "Le Sueur",
    "Sibley",
    "McLeod",
    "Wright"
  ),
  class = "sf"
) %>%
  select(GEOID)

WItract <- tigris::tracts(
  year = 2019,
  state = "WI",
  county = c("St. Croix", "Polk", "Pierce"),
  class = "sf"
) %>%
  select(GEOID)

census_tract_spatial_visitorstudy <- bind_rows(MNtract, WItract) %>%
  left_join(census_demos, by = c("GEOID" = "geoid2")) %>%
  st_transform(4326) # for leaflet

# county_outlines <- tigris::counties(
#   year = 2020,
#   state = "MN",
#   class = "sf"
# ) %>%
#   dplyr::filter(NAME %in% c(
#     "Hennepin",
#     "Dakota",
#     "Carver",
#     "Ramsey",
#     "Anoka",
#     "Scott",
#     "Washington"
#   )) %>%
#   dplyr::select(NAME) %>%
#   sf::st_transform(4326)


usethis::use_data(census_tract_spatial_visitorstudy, overwrite = TRUE)

