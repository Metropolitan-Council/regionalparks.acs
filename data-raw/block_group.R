## code to prepare `block_group` dataset goes here

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

temp <- tempfile()
download.file("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/society_census_acs/xlsx_society_census_acs.zip",
  destfile = temp
)

bg <- readxl::read_xlsx(unzip(temp, "CensusACSBlockGroup.xlsx")) %>%
  janitor::clean_names() %>%
  filter(state == 27)

fs::file_delete("CensusACSBlockGroup.xlsx")


## ----------------------------------------------------------------------------------------------------------------------------------------------------
bg_age <- bg %>%
  select(geoid, geoid2, poptotal, ageunder18, age18_39, age40_64, age65up) %>%
  mutate(
    ageunder18_percent = round(ageunder18 / poptotal, digits = 2),
    age18_39_percent = round(age18_39 / poptotal, digits = 2),
    age40_64_percent = round(age40_64 / poptotal, digits = 2),
    age65up_percent = round(age65up / poptotal, digits = 2)
  )


## -----------------------------------------------------------------------------------------------------------------------------------------------------
bg_race <- bg %>%
  select(geoid, geoid2, poptotal, whitenh, blacknh, asiannh, amindnh, pacificnh, othernh, multracenh) %>%
  mutate(
    whitenh_percent = round(whitenh / poptotal, digits = 2),
    blacknh_percent = round(blacknh / poptotal, digits = 2),
    asiannh_percent = round(asiannh / poptotal, digits = 2),
    amindnh_percent = round(amindnh / poptotal, digits = 2),
    pacificnh_percent = round(pacificnh / poptotal, digits = 2),
    othernh_percent = round(othernh / poptotal, digits = 2),
    multracenh_percent = round(multracenh / poptotal, digits = 2),
    poc_percent = 1 - whitenh_percent
  )


## -----------------------------------------------------------------------------------------------------------------------------------------------------
bg_hisp <- bg %>%
  select(geoid, geoid2, poptotal, hisppop, nothisppop) %>%
  mutate(
    hisppop_percent = round(hisppop / poptotal, digits = 2),
    nothisppop_percent = round(nothisppop / poptotal, digits = 2)
  )


## -----------------------------------------------------------------------------------------------------------------------------------------------------
bg_income <- bg %>%
  select(geoid, geoid2, poptotal, medianhhi)


## -----------------------------------------------------------------------------------------------------------------------------------------------------
bg_merge <- right_join(bg_age, bg_race) %>%
  right_join(bg_hisp) %>%
  right_join(bg_income)


## -----------------------------------------------------------------------------------------------------------------------------------------------------

block_group <- tigris::block_groups(
  state = "MN",
  class = "sf"
) %>%
  select(GEOID) %>%
  left_join(bg_merge, by = c("GEOID" = "geoid2"))



usethis::use_data(block_group, overwrite = TRUE)
