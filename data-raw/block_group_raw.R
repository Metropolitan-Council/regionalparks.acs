## code to prepare `block_group_raw` dataset goes here
# these data are "raw" because it contains all block groups in the 7 county core area plus all collar counties.
# we only want to map the geometries which are the in the 7 county core OR have an overlap with a park/trail buffer geometry if within a collar county.

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
download.file(
  "https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/society_census_acs/xlsx_society_census_acs.zip",
  destfile = temp
)

bg <- readxl::read_xlsx(unzip(temp, "CensusACSBlockGroup.xlsx")) %>%
  janitor::clean_names() # %>%
# filter(state == 27)

fs::file_delete("CensusACSBlockGroup.xlsx")


## ----------------------------------------------------------------------------------------------------------------------------------------------------
race_key <- tibble(acscode = c("whitenh",
              "blachnh",
              "asiannh",
              "amindnh",
              "pacificnh",
              "othernh",
              "multracenh")) %>%
  mutate(filtercode = paste0("race_", acscode))


bg_merge <- bg %>%
  select(geoid2, 
         # household vars
         hhtotal, 
         hh_noveh, 
         #economic vars
         meanhhinc,
         (starts_with("pov") & !ends_with("rate")),
         #pop vars
         poptotal,
         lep, lep_span,
         starts_with(c("m_", "f_")),
         ends_with(c("nh", "hisppop"))) %>%
  mutate(
    #### economic
    pov185 = povertyn + poverty150 + pov150_185,
    
    ##### age
    ### young -------
    ageunder15 = m_0_4 + f_0_4 +
                     m_5_9 + f_5_9 +
                     m_10_14 + f_10_14,
    # "youth"
    age15_24 = ((m_15_19 + f_15_19 +
                   m_20_24 + f_20_24)),
    # # "young adult"
    # age25_64 = ((m_25_29 + m_30_34 + m_35_39 + m_40_44 + m_45_49 + m_50_54 + m_55_59 + m_60_64 +
    #                f_25_29 + f_30_34 + f_35_39 + f_40_44 + f_45_49 + f_50_54 + f_55_59 + f_60_64)),
    # "younger middle adult"
    age25_44 = ((m_25_29 + m_30_34 + m_35_39 + m_40_44 + 
                   f_25_29 + f_30_34 + f_35_39 + f_40_44)),
    # "older middle adult"
    age45_64 = ((m_45_49 + m_50_54 + m_55_59 + m_60_64 +
                   f_45_49 + f_50_54 + f_55_59 + f_60_64)),
    
    ### race
    othermultinh = pacificnh + othernh + multracenh) %>%
  
  select(-pacificnh, -othernh, -multracenh, 
         -starts_with(c("m_", "f_")),
         -povertyn, -poverty150, -pov150_185) %>% #aggregated, so don't need
  mutate(
    across(hh_noveh, ~round(.x / hhtotal, digits = 2)),
    across(pov185, ~round(.x / povdenom, digits = 2)),
    across(starts_with(c("age", "lep")) | ends_with(c("nh", "hisppop")), ~round(.x / poptotal, digits = 2))) 
  


## -----------------------------------------------------------------------------------------------------------------------------------------------------
MNblock_group <- tigris::block_groups(
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

WIblock_group <- tigris::block_groups(
  year = 2019,
  state = "WI",
  county = c("St. Croix", "Polk", "Pierce"),
  class = "sf"
) %>%
  select(GEOID) # %>%


block_group_raw <- bind_rows(MNblock_group, WIblock_group) %>%
  left_join(bg_merge, by = c("GEOID" = "geoid2")) %>% # left join, so takes out bg in WI that are not within the 3mi buffer zone
  st_transform(4326) # for leaflet

usethis::use_data(block_group_raw, overwrite = TRUE)
