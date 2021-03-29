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
bg_age <- bg %>%
  mutate(
    ageunder15 = ((m_0_4 + f_0_4 +
      m_5_9 + f_5_9 +
      m_10_14 + f_10_14)),
    # "youth"
    age15_24 = ((m_15_19 + f_15_19 +
      m_20_24 + f_20_24)),
    # "young adult"
    age25_64 = ((m_25_29 + m_30_34 + m_35_39 + m_40_44 + m_45_49 + m_50_54 + m_55_59 + m_60_64 +
      f_25_29 + f_30_34 + f_35_39 + f_40_44 + f_45_49 + f_50_54 + f_55_59 + f_60_64))
  ) %>%
  transmute(
    geoid2 = geoid2,
    ageunder15_percent = round(ageunder15 / poptotal, digits = 2),
    age15_24_percent = round(age15_24 / poptotal, digits = 2),
    age25_64_percent = round(age25_64 / poptotal, digits = 2),
    ageunder18_percent = round(ageunder18 / poptotal, digits = 2),
    age18_39_percent = round(age18_39 / poptotal, digits = 2),
    age40_64_percent = round(age40_64 / poptotal, digits = 2),
    age65up_percent = round(age65up / poptotal, digits = 2)
  )


## -----------------------------------------------------------------------------------------------------------------------------------------------------
bg_race <- bg %>%
  transmute(
    geoid2 = geoid2,
    whitenh_percent = round(whitenh / poptotal, digits = 2),
    blacknh_percent = round(blacknh / poptotal, digits = 2),
    asiannh_percent = round(asiannh / poptotal, digits = 2),
    amindnh_percent = round(amindnh / poptotal, digits = 2),
    pacificnh_percent = round(pacificnh / poptotal, digits = 2),
    othernh_percent = round(othernh / poptotal, digits = 2),
    multracenh_percent = round(multracenh / poptotal, digits = 2),
    poc_percent = 1 - whitenh_percent,
    othermultinh_percent = round((pacificnh + othernh + multracenh) / poptotal, digits = 2),
  )


## -----------------------------------------------------------------------------------------------------------------------------------------------------
bg_hisp <- bg %>%
  transmute(
    geoid2 = geoid2,
    hisppop_percent = round(hisppop / poptotal, digits = 2),
    nothisppop_percent = round(nothisppop / poptotal, digits = 2)
  )


# -----------------------------------------------------------------------------------------------------------------------------------------------------
bg_transportation <- bg %>%
  transmute(
    geoid2 = geoid2,
    novehicle_percent = round(hh_noveh / hhtotal, digits = 2)
  )


# -----------------------------------------------------------------------------------------------------------------------------------------------------

bg_lang <- bg %>%
  transmute(
    geoid2 = geoid2,
    poorenglish_percent = round(lep / poptotal, digits = 2),
    spanish_percent = round(lep_span / poptotal, digits = 2)
  )


## -----------------------------------------------------------------------------------------------------------------------------------------------------
bg_income <- bg %>%
  select(geoid2, poptotal, medianhhi, meanhhinc, povertyn, poverty150, pov150_185, povdenom) %>%
  mutate(pov185_percent = round((povertyn + poverty150 + pov150_185) / povdenom, digits = 2))


## -----------------------------------------------------------------------------------------------------------------------------------------------------
bg_merge <- right_join(bg_age, bg_race) %>%
  right_join(bg_hisp) %>%
  right_join(bg_income) %>%
  right_join(bg_transportation) %>%
  right_join(bg_lang)


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
