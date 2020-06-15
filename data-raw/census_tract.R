## code to prepare `census_tract` dataset goes here

date <- format(Sys.time(), "%Y%m%d")
pkgload::load_all()

library(dplyr)
library(fs)
library(sf)

temp <- tempfile()
download.file("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/society_census_acs/xlsx_society_census_acs.zip",
  destfile = temp
)

ct <- readxl::read_xlsx(unzip(temp, "CensusACSTract.xlsx")) %>%
  janitor::clean_names() %>%
  filter(state == 27)

fs::file_delete("CensusACSTract.xlsx")


## -----------------------------------------------------------------------------------------------------------------------------------------------------
ct_age <- ct %>%
  select(geoid, geoid2, poptotal, f_10_14, f_15_19, m_10_14, m_15_19, ageunder18, age18_39, age40_64, age65up) %>%
  group_by(1:1338) %>%
  mutate(
    age_10_19_percent = round(sum(f_10_14, f_15_19, m_10_14, m_15_19) / poptotal, digits = 2),
    ageunder18_percent = round(ageunder18 / poptotal, digits = 2),
    age18_39_percent = round(age18_39 / poptotal, digits = 2),
    age40_64_percent = round(age40_64 / poptotal, digits = 2),
    age65up_percent = round(age65up / poptotal, digits = 2)
  )


## -----------------------------------------------------------------------------------------------------------------------------------------------------
ct_race <- ct %>%
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
ct_hisp <- ct %>%
  select(geoid, geoid2, poptotal, hisppop, nothisppop) %>%
  mutate(
    hisppop_percent = round(hisppop / poptotal, digits = 2),
    nothisppop_percent = round(nothisppop / poptotal, digits = 2)
  )


## ----------------------------------------------------------------------------------------------------------------------------------------------------
ct_foreign <- ct %>%
  select(geoid, geoid2, poptotal, usborncit, forborncit, forbornnot) %>%
  mutate(
    usborncit_percent = round(usborncit / poptotal, digits = 2),
    forborn_percent = round((forborncit + forbornnot) / poptotal, digits = 2)
  )


## -----------------------------------------------------------------------------------------------------------------------------------------------------
ct_disability <- ct %>%
  select(geoid, geoid2, poptotal, anydis) %>%
  mutate(anydis_percent = round(anydis / poptotal, digits = 2))


## ------------------------------------------------------------------------------------------------------------------------------------------------------
ct_inc <- ct %>%
  select(geoid, geoid2, poptotal, medianhhi)


## ------------------------------------------------------------------------------------------------------------------------------------------------------
ct_merge <- right_join(ct_foreign, ct_disability) %>%
  right_join(ct_age) %>%
  right_join(ct_hisp) %>%
  right_join(ct_inc) %>%
  right_join(ct_race)


## ----------------------------------------------------------------------------------------------------------------------------------------------------

census_tract <- tigris::tracts(
  state = "MN",
  class = "sf"
) %>%
  select(GEOID) %>%
  left_join(ct_merge, by = c("GEOID" = "geoid2"))



usethis::use_data(census_tract, overwrite = TRUE)
