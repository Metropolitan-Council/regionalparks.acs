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

metarea <- tibble(county = c("003", "019", "037", "053", "123", "139", "163"))

bg <- readxl::read_xlsx(unzip(temp, "CensusACSBlockGroup.xlsx")) %>%
  janitor::clean_names() %>%
  filter(state == 27, 
         county %in% metarea$county)

fs::file_delete("CensusACSBlockGroup.xlsx")


## ----------------------------------------------------------------------------------------------------------------------------------------------------
bg_age <- bg %>%
  mutate(
    ageunder15 = ((m_0_4 + f_0_4 + 
                     m_5_9 + f_5_9 +
                     m_10_14 + f_10_14)), # "youth"
    age15_24 = ((m_15_19 + f_15_19 +
                   m_20_24 + f_20_24)), # "young adult"
    age25_64 = ((m_25_29 + m_30_34 + m_35_39 + m_40_44 + m_45_49 + m_50_54 + m_55_59 + m_60_64 + 
                   f_25_29 + f_30_34 + f_35_39 + f_40_44 + f_45_49 + f_50_54 + f_55_59 + f_60_64))) %>%  
  select(geoid, geoid2, poptotal, ageunder18, age18_39, age40_64, age65up, ageunder15, age15_24, age25_64) %>%
  mutate(ageunder15_percent = round(ageunder15 / poptotal, digits = 2),
         age15_24_percent = round(age15_24 / poptotal, digits = 2),
         age25_64_percent = round(age25_64 / poptotal, digits = 2),
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
    othermutltnh_percent = round((pacificnh + othernh + multracenh) / poptotal, digits = 2),
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
# #there is not disability info at the block group level
# bg_disability <- bg %>%
#   select(geoid, geoid2, poptotal, anydis) %>%
#   mutate(anydis_percent = round(anydis / poptotal, digits = 2) * 100)

bg$anydis

# -----------------------------------------------------------------------------------------------------------------------------------------------------
bg_transportation <- bg %>%
  select(geoid, geoid2, vehiclesn, hh_noveh, hhtotal) %>%
  mutate(novehicle_percent = round(hh_noveh / hhtotal, digits = 2) * 100)

# -----------------------------------------------------------------------------------------------------------------------------------------------------
# #surpressed at block group level
# bg_birth <- bg %>%
#   select(geoid, geoid2, forbornnot, forborncit)
summary(bg$forborncit)
summary(bg$forbornnot)

# -----------------------------------------------------------------------------------------------------------------------------------------------------
# summary(bg$lep_russ) #surpressed at bg level

bg_lang <- bg %>%
  select(geoid, geoid2, lep, lep_span, poptotal) %>%
  mutate(poorenglish_percent = round(lep / poptotal, digits = 2) * 100,
         spanish_percent = round(lep_span / poptotal, digits = 2) * 100)

#would be nice to add if geography becomes un-supressed
summary(bg$lep_africa)
summary(bg$lep_chin)
summary(bg$lep_hmong)
summary(bg$lep_viet)

## -----------------------------------------------------------------------------------------------------------------------------------------------------
bg_income <- bg %>%
  select(geoid, geoid2, poptotal, medianhhi)

## -----------------------------------------------------------------------------------------------------------------------------------------------------
bg_forbuffer <- bg %>%
  select(geoid, geoid2, hhtotal, meanhhinc) 

## -----------------------------------------------------------------------------------------------------------------------------------------------------
bg_merge <- right_join(bg_age, bg_race) %>%
  # right_join(bg_disability) %>%
  right_join(bg_hisp) %>%
  right_join(bg_income) %>%
  right_join(bg_transportation) %>%
  right_join(bg_lang) %>%
  right_join(bg_forbuffer)


## -----------------------------------------------------------------------------------------------------------------------------------------------------

block_group <- tigris::block_groups(
  state = "MN",
  class = "sf"
) %>%
  select(GEOID) %>%
  left_join(bg_merge, by = c("GEOID" = "geoid2")) %>%
  mutate(county = substr(GEOID, start = 3, stop = 5)) %>%
  filter(county %in% metarea$county)


usethis::use_data(block_group, overwrite = TRUE)
# save(block_group, file = "./regionalparks.acs/data/block_group.rda")
