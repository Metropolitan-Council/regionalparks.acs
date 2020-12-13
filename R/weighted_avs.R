## code to prepare `buffer_distances`, `long_buffer_data`, `agency_avg` datasets goes here

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
library("stringr")
library("cowplot")


## population, small area estimates -----------------------------------------------------------------------
temp <- tempfile()
download.file("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/society_small_area_estimates/xlsx_society_small_area_estimates.zip",
              destfile = temp
)

bg_pop_2019 <- readxl::read_xlsx(unzip(temp, "SmallAreaEstimatesBlockGroup.xlsx")) %>%
  janitor::clean_names() %>%
  filter(
    # state == 27, #contains ONLY metcouncil area
    est_year == 2019
  ) %>%
  select(pop_est, hh_est, bg10) %>%
  rename(GEOID = bg10,
         pop2019 = pop_est,
         hh2019 = hh_est)



agency_filter <- tibble(agency = c("Anoka County", # Parks and Recreation",
                                   "Bloomington", # Parks and Recreation",
                                   "Carver County", # Parks and Recreation" , 
                                   "Dakota County", # Parks",
                                   "MPRB",
                                   "Ramsey County", # Parks and Recreation" ,
                                   "Scott County", # Parks",
                                   "St. Paul", # Parks and Recreation",
                                   "Three Rivers",
                                   "Washington County"), # Parks"),
                        num = c(1:10))

park_trail_geog_temp <- bind_rows(park_trail_geog, .id = "status") %>%
  full_join(agency_filter) %>%
  mutate(name = paste(name, num, sep = "_"),
         type = if_else(status == "park" |
                          status == "park_planned" |
                          status == "park_search", "Park", "Trail"),
         status = case_when(status == "park" | status == "trail" ~ "Existing",
                            status == "park_planned" | status == "trail_planned" ~ "Planned", 
                            status == "park_search" | status == "trail_search" ~ "Search")) %>%
  st_transform(3857) #https://epsg.io/3857\

metarea <- tibble(county = c("003", "019", "037", "053", "123", "139", "163"))

acs_temp <- block_group %>%
  mutate(county = substr(GEOID, start = 3, stop = 5)) %>%
  # filter(county %in% metarea$county) %>% #want to get collar counties too
  st_transform(3857) %>% #https://epsg.io/3857\
  mutate(bg_area = st_area(.)) 

agency_boundary <- read_sf("/Volumes/shared/CommDev/Research/Public/GIS/Parks/Park_Operating_Agencies.shp") %>% #("./agencyboundaries/Park_Operating_Agencies.shp") %>%
  mutate(COMCD_DESC = recode(COMCD_DESC, "Minneapolis" = "MPRB")) %>%
  rename(agency = COMCD_DESC) %>%
  select(agency)



## get coverage of block groups falling w/in buffer zones for all
coverage_agency <- acs_temp %>% #this has geography in it
  select(GEOID, bg_area) %>%
  st_intersection(agency_boundary %>% st_transform(3857)) %>%
  mutate(intersect_area = st_area(.)) %>%   # create new column with shape area
  select(GEOID, agency, intersect_area) %>%   # only select columns needed to merge
  st_drop_geometry() %>% #drop geometry as we don't need it
  left_join(acs_temp %>% #merge back in with all block groups
              select(GEOID, bg_area)) %>%
  mutate(coverage = as.numeric(intersect_area / bg_area)) %>% #calculate fraction of block group within each agency/park buffer
  as_tibble() %>%
  select(GEOID, agency, coverage) 

## agency average ------------------
agency_avg <- coverage_agency %>% 
  left_join(acs_temp, by = c("GEOID")) %>% 
  left_join(bg_pop_2019, by = c("GEOID"))

agency_avg %>% 
  transmute(
    agency = agency, 
    coverage = coverage, 
    GEOID = GEOID, 
    adj_2019_pop = coverage * pop2019, #adjust #s by coverage %
    adj_ageunder15 = coverage * ageunder15, #youth
    adj_age15_24 = coverage * age15_24, #young adult
    adj_age25_64 = coverage * age25_64, #middle age?
    adj_age65up_pop = age65up_percent * adj_2019_pop) %>%

    
    
  
  mutate(adj_poptotal = coverage * poptotal, #create weighted/adjusted totals for each blockgroup
         adj_ageunder15 = coverage * ageunder15, #youth
         adj_age15_24 = coverage * age15_24, #young adult
         adj_age25_64 = coverage * age25_64, #middle age?
         adj_age65up = coverage * age65up,
         adj_whitenh = coverage * whitenh,
         adj_blacknh = coverage * blacknh,
         adj_asiannh = coverage * asiannh,
         adj_amindnh = coverage * amindnh,
         adj_othermultinh = coverage * (pacificnh + othernh + multracenh),
         # adj_othernh = coverage * othernh,
         # adj_multracenh = coverage * multracenh,
         # adj_anydis = coverage * anydis_percent, #this is NA at block group levels
         adj_hisppop = coverage * hisppop,
         adj_nothisppop = coverage * nothisppop,
         adj_hhtotal = coverage * hhtotal,
         adj_totalhhi = meanhhinc * adj_hhtotal,
         adj_novehicle = coverage * hh_noveh,
         adj_lep = coverage * lep,
         adj_span = coverage * lep_span) %>%
  group_by(agency) %>%
  summarise(across(adj_poptotal:adj_span, sum, na.rm = T)) %>% #need to get weighted/adjusted totals for each park/agency parcel
  mutate(adj_poptotal = round(adj_poptotal, 0),
         adj_ageunder15_per = round(adj_ageunder15 / adj_poptotal * 100, 1),
         adj_age15_24_per = round(adj_age15_24 / adj_poptotal * 100, 1),
         adj_age25_64_per = round(adj_age25_64 / adj_poptotal * 100, 1),
         adj_age65up_per = round(adj_age65up / adj_poptotal * 100,1),
         adj_whitenh_per = round(adj_whitenh / adj_poptotal * 100, 1),
         adj_blacknh_per = round(adj_blacknh / adj_poptotal * 100, 1),
         adj_asiannh_per = round(adj_asiannh / adj_poptotal * 100, 1),
         adj_amindnh_per = round(adj_amindnh / adj_poptotal * 100, 1),
         adj_othermultinh_per = round(adj_othermultinh / adj_poptotal * 100, 1),
         adj_hisppop_per = round(adj_hisppop / adj_poptotal * 100, 1),
         adj_nothisppop_per = round(adj_nothisppop / adj_poptotal * 100, 1),
         adj_meanhhi = round(adj_totalhhi / adj_hhtotal, 1),
         adj_novehicle_per = round(adj_novehicle / adj_hhtotal *100, 1),
         adj_lep_per = round(adj_lep / adj_poptotal * 100, 1),
         adj_span_per = round(adj_span / adj_poptotal * 100, 1))  %>%
  as_tibble() %>% 
  gather(key = "ACS", value = "value", 
         -agency)

usethis::use_data(agency_avg, overwrite = TRUE)


## 1.0 mile buffer ----------------------------------------------------------------------

buff_1.0mi <- park_trail_geog_temp %>% 
  st_buffer(dist = 1609.34*1.0) %>% #the 3857 projection uses meters as a distance, so 1.0 mi = 
  group_by(agency, name, type, status) %>%
  summarise(geometry = st_union(geom)) #some parks have multiple units which overlap, so we want to consolidate them

# # confirm that each park/agency combo has own buffer. 
# #And that multiple parcels within parks are consolidated together
# buff_1.0mi %>%
#   filter(name == "Battle Creek Regional Park_6" |
#            name == "Indian Mounds Regional Park_8") %>%
#   ggplot() +
#   geom_sf(aes(fill = name), alpha = .6) +
#   geom_sf(data = filter(park_trail_geog_temp, name == "Battle Creek Regional Park_6" |
#                           name == "Indian Mounds Regional Park_8"),
#           aes(fill = name))

intersect_pct_1.0mi <- acs_temp %>%
  select(GEOID, bg_area) %>%
  st_intersection(buff_1.0mi) %>% #every trail name gets own intersection
  mutate(intersect_area = st_area(.)) %>%   # create new column with shape area
  select(GEOID, agency, name, type, status, intersect_area) %>%   # only select columns needed to merge
  st_drop_geometry() %>% # drop geometry as we don't need it
  left_join(acs_temp %>% #merge back in with all block groups
              select(GEOID, bg_area)) %>%
  mutate(coverage = as.numeric(intersect_area / bg_area)) %>% #calculate fraction of block group within each agency/park buffer
  as_tibble() %>%
  select(GEOID, agency, name, type, status, coverage)

# #check to make sure this geoid has multiple occurrences
# intersect_pct_1.0mi %>%
#   filter(GEOID == "270030501071")

# #confirm that each park/agency combo has created it's own % of block group included
# #note overlapping block groups in this example continued from above
# acs_temp %>%
#   right_join(intersect_pct_1.0mi) %>%
#   filter(name == "Battle Creek Regional Park_6" |
#            name == "Indian Mounds Regional Park_8") %>%
#   ggplot() +
#   geom_sf(aes(fill = coverage), alpha = .6) +
#   facet_wrap(~name) +
#   geom_sf(data = filter(buff_1.0mi, name == "Battle Creek Regional Park_6" |
#                           name == "Indian Mounds Regional Park_8"),
#           aes(col = name),
#           fill = "black") +
#   geom_sf(data = filter(park_trail_geog_temp, name == "Battle Creek Regional Park_6" |
#                           name == "Indian Mounds Regional Park_8"),
#   aes(col = name), fill = 'white', alpha = 1)

buffer_block_group_1.0mi_raw <- acs_temp %>%
  right_join(intersect_pct_1.0mi, by = c("GEOID")) %>% 
  mutate(adj_poptotal = coverage * poptotal, #create weighted/adjusted totals for each blockgroup
         adj_ageunder15 = coverage * ageunder15, #youth
         adj_age15_24 = coverage * age15_24, #young adult
         adj_age25_64 = coverage * age25_64, #middle age?
         # adj_ageunder18 = coverage * ageunder18,
         # adj_age18_39 = coverage * age18_39,
         # adj_age40_64 = coverage * age40_64,
         adj_age65up = coverage * age65up,
         adj_whitenh = coverage * whitenh,
         adj_blacknh = coverage * blacknh,
         adj_asiannh = coverage * asiannh,
         adj_amindnh = coverage * amindnh,
         adj_othermultinh = coverage * (pacificnh + othernh + multracenh),
         # adj_othernh = coverage * othernh,
         # adj_multracenh = coverage * multracenh,
         # adj_anydis = coverage * anydis_percent, #this is NA at block group levels
         adj_hisppop = coverage * hisppop,
         adj_nothisppop = coverage * nothisppop,
         adj_hhtotal = coverage * hhtotal,
         adj_totalhhi = meanhhinc * adj_hhtotal,
         adj_novehicle = coverage * hh_noveh,
         adj_lep = coverage * lep,
         adj_span = coverage * lep_span,
         distance = 1.0) 

buffer_block_group_1.0mi <- buffer_block_group_1.0mi_raw %>%
  group_by(agency, name, type, status) %>%
  summarise(across(adj_poptotal:adj_span, sum, na.rm = T)) %>% #need to get weighted/adjusted totals for each park/agency parcel
  mutate(adj_poptotal = round(adj_poptotal, 0),
         adj_ageunder15_per = round(adj_ageunder15 / adj_poptotal * 100, 1),
         adj_age15_24_per = round(adj_age15_24 / adj_poptotal * 100, 1),
         adj_age25_64_per = round(adj_age25_64 / adj_poptotal * 100, 1),
         adj_age65up_per = round(adj_age65up / adj_poptotal * 100,1),
         adj_whitenh_per = round(adj_whitenh / adj_poptotal * 100, 1),
         adj_blacknh_per = round(adj_blacknh / adj_poptotal * 100, 1),
         adj_asiannh_per = round(adj_asiannh / adj_poptotal * 100, 1),
         adj_amindnh_per = round(adj_amindnh / adj_poptotal * 100, 1),
         adj_othermultinh_per = round(adj_othermultinh / adj_poptotal * 100, 1),
         adj_hisppop_per = round(adj_hisppop / adj_poptotal * 100, 1),
         adj_nothisppop_per = round(adj_nothisppop / adj_poptotal * 100, 1),
         adj_meanhhi = round(adj_totalhhi / adj_hhtotal, 1),
         adj_novehicle_per = round(adj_novehicle / adj_hhtotal *100, 1),
         adj_lep_per = round(adj_lep / adj_poptotal * 100, 1),
         adj_span_per = round(adj_span / adj_poptotal * 100, 1),
         distance = 1.0)  

## 1.5 mile buffer ----------------------------------------------------------------------

buff_1.5mi <- park_trail_geog_temp %>% 
  st_buffer(dist = 1609.34 * 1.5) %>% 
  group_by(agency, name, type, status) %>%
  summarise(geometry = st_union(geom)) 

intersect_pct_1.5mi <- acs_temp %>%
  select(GEOID, bg_area) %>%
  st_intersection(buff_1.5mi) %>% 
  mutate(intersect_area = st_area(.)) %>%
  select(GEOID, agency, name, type, status, intersect_area) %>%  
  st_drop_geometry() %>% 
  left_join(acs_temp %>%
              select(GEOID, bg_area)) %>%
  mutate(coverage = as.numeric(intersect_area / bg_area)) %>% 
  as_tibble() %>%
  select(GEOID, agency, name, type, status, coverage) 

buffer_block_group_1.5mi_raw <- acs_temp %>%
  right_join(intersect_pct_1.5mi) %>%
  mutate(adj_poptotal = coverage * poptotal,
         adj_ageunder15 = coverage * ageunder15, #youth
         adj_age15_24 = coverage * age15_24, #young adult
         adj_age25_64 = coverage * age25_64, #middle age?
         adj_age65up = coverage * age65up,
         adj_whitenh = coverage * whitenh,
         adj_blacknh = coverage * blacknh,
         adj_asiannh = coverage * asiannh,
         adj_amindnh = coverage * amindnh,
         adj_othermultinh = coverage * (pacificnh + othernh + multracenh),
         adj_hisppop = coverage * hisppop,
         adj_nothisppop = coverage * nothisppop,
         adj_hhtotal = coverage * hhtotal,
         adj_totalhhi = meanhhinc * adj_hhtotal,
         adj_novehicle = coverage * hh_noveh,
         adj_lep = coverage * lep,
         adj_span = coverage * lep_span,
         distance = 1.5)

buffer_block_group_1.5mi <- buffer_block_group_1.5mi_raw %>%
  group_by(agency, name, type, status) %>%
  summarise(across(adj_poptotal:adj_span, sum, na.rm = T)) %>% 
  mutate(adj_poptotal = round(adj_poptotal, 0),
         adj_ageunder15_per = round(adj_ageunder15 / adj_poptotal * 100, 1),
         adj_age15_24_per = round(adj_age15_24 / adj_poptotal * 100, 1),
         adj_age25_64_per = round(adj_age25_64 / adj_poptotal * 100, 1),
         adj_age65up_per = round(adj_age65up / adj_poptotal * 100,1),
         adj_whitenh_per = round(adj_whitenh / adj_poptotal * 100, 1),
         adj_blacknh_per = round(adj_blacknh / adj_poptotal * 100, 1),
         adj_asiannh_per = round(adj_asiannh / adj_poptotal * 100, 1),
         adj_amindnh_per = round(adj_amindnh / adj_poptotal * 100, 1),
         adj_othermultinh_per = round(adj_othermultinh / adj_poptotal * 100, 1),
         adj_hisppop_per = round(adj_hisppop / adj_poptotal * 100, 1),
         adj_nothisppop_per = round(adj_nothisppop / adj_poptotal * 100, 1),
         adj_meanhhi = round(adj_totalhhi / adj_hhtotal, 1),
         adj_novehicle_per = round(adj_novehicle / adj_hhtotal *100, 1),
         adj_lep_per = round(adj_lep / adj_poptotal * 100, 1),
         adj_span_per = round(adj_span / adj_poptotal * 100, 1),
         distance = 1.5)  


## 3 mile buffer ----------------------------------------------------------------------

buff_3mi <- park_trail_geog_temp %>% 
  st_buffer(dist = 1609.34 * 3) %>% 
  group_by(agency, name, type, status) %>%
  summarise(geometry = st_union(geom)) 

intersect_pct_3mi <- acs_temp %>%
  select(GEOID, bg_area) %>%
  st_intersection(buff_3mi) %>% 
  mutate(intersect_area = st_area(.)) %>%
  select(GEOID, agency, name, type, status, intersect_area) %>%  
  st_drop_geometry() %>% 
  left_join(acs_temp %>%
              select(GEOID, bg_area)) %>%
  mutate(coverage = as.numeric(intersect_area / bg_area)) %>% 
  as_tibble() %>%
  select(GEOID, agency, name, type, status, coverage) 

buffer_block_group_3mi_raw <- acs_temp %>%
  right_join(intersect_pct_3mi) %>%
  mutate(adj_poptotal = coverage * poptotal, 
         adj_ageunder15 = coverage * ageunder15, #youth
         adj_age15_24 = coverage * age15_24, #young adult
         adj_age25_64 = coverage * age25_64, #middle age?
         adj_age65up = coverage * age65up,
         adj_whitenh = coverage * whitenh,
         adj_blacknh = coverage * blacknh,
         adj_asiannh = coverage * asiannh,
         adj_amindnh = coverage * amindnh,
         adj_othermultinh = coverage * (pacificnh + othernh + multracenh),
         adj_hisppop = coverage * hisppop,
         adj_nothisppop = coverage * nothisppop,
         adj_hhtotal = coverage * hhtotal,
         adj_totalhhi = meanhhinc * adj_hhtotal,
         adj_novehicle = coverage * hh_noveh,
         adj_lep = coverage * lep,
         adj_span = coverage * lep_span,
         distance = 3)

buffer_block_group_3mi <- buffer_block_group_3mi_raw %>%
  group_by(agency, name, type, status) %>%
  summarise(across(adj_poptotal:adj_span, sum, na.rm = T)) %>% 
  mutate(adj_poptotal = round(adj_poptotal, 0),
         adj_ageunder15_per = round(adj_ageunder15 / adj_poptotal * 100, 1),
         adj_age15_24_per = round(adj_age15_24 / adj_poptotal * 100, 1),
         adj_age25_64_per = round(adj_age25_64 / adj_poptotal * 100, 1),
         adj_age65up_per = round(adj_age65up / adj_poptotal * 100,1),
         adj_whitenh_per = round(adj_whitenh / adj_poptotal * 100, 1),
         adj_blacknh_per = round(adj_blacknh / adj_poptotal * 100, 1),
         adj_asiannh_per = round(adj_asiannh / adj_poptotal * 100, 1),
         adj_amindnh_per = round(adj_amindnh / adj_poptotal * 100, 1),
         adj_othermultinh_per = round(adj_othermultinh / adj_poptotal * 100, 1),
         adj_hisppop_per = round(adj_hisppop / adj_poptotal * 100, 1),
         adj_nothisppop_per = round(adj_nothisppop / adj_poptotal * 100, 1),
         adj_meanhhi = round(adj_totalhhi / adj_hhtotal, 1),
         adj_novehicle_per = round(adj_novehicle / adj_hhtotal *100, 1),
         adj_lep_per = round(adj_lep / adj_poptotal * 100, 1),
         adj_span_per = round(adj_span / adj_poptotal * 100, 1),
         distance = 3) 

## Combine ---------------------------------------------------------------------
long_buffer_data <- bind_rows(buffer_block_group_1.0mi,
                              buffer_block_group_1.5mi,
                              buffer_block_group_3mi) %>%
  as_tibble() %>% select(-geometry) %>%
  gather(key = "ACS", value = "value", 
         -agency, -name, -type, -status, -distance)
usethis::use_data(long_buffer_data, overwrite = TRUE)


long_buffer_data_raw <- bind_rows(buffer_block_group_1.0mi_raw,
                                  buffer_block_group_1.5mi_raw,
                                  buffer_block_group_3mi_raw) %>%
  as_tibble() %>% 
  select(agency, name, type, status, distance, 
         GEOID, coverage,
         ageunder15_percent, age15_24_percent, age25_64_percent, age65up_percent,
         amindnh_percent, asiannh_percent, blacknh_percent, othermutltnh_percent, whitenh_percent, 
         hisppop_percent, nothisppop_percent, 
         meanhhinc,
         novehicle_percent, poorenglish_percent, spanish_percent
  ) %>%
  gather(key = "ACS", value = "value", 
         -agency, -name, -type, -status, -distance, -GEOID, -coverage) %>%
  mutate(ACS = recode(ACS, "ageunder15_percent" = "adj_ageunder15_per",
                      "age15_24_percent" = "adj_age15_24_per",
                      "age25_64_percent" = "adj_age25_64_per",
                      "age65up_percent" = "adj_age65up_per",
                      "whitenh_percent" = "adj_whitenh_per",
                      "blacknh_percent" = "adj_blacknh_per",
                      "asiannh_percent" = "adj_asiannh_per",
                      "amindnh_percent" = "adj_amindnh_per",
                      "othermutltnh_percent" = "adj_othermultinh_per",
                      "hisppop_percent" = "adj_hisppop_per",
                      "nothisppop_percent" = "adj_nothisppop_per",
                      "meanhhinc" = "adj_meanhhi",
                      "novehicle_percent" = "adj_novwhicle_per",
                      "poorenglish_percent" = "adj_lep_per",
                      "spanish_percent" = "adj_span_per"))
usethis::use_data(long_buffer_data_raw, overwrite = TRUE)


buffer_distances <- list(buffer_block_group_1.0mi, buffer_block_group_1.5mi, buffer_block_group_3mi) 
names(buffer_distances) <- c(
  "1.0 mi",
  "1.5 mi",
  "3 mi"
)
usethis::use_data(buffer_distances, overwrite = TRUE)

buffer_distances_raw <- list(buffer_block_group_1.0mi_raw, buffer_block_group_1.5mi_raw, buffer_block_group_3mi_raw)
names(buffer_distances_raw) <- c(
  "1.0 mi",
  "1.5 mi",
  "3 mi"
)
usethis::use_data(buffer_distances_raw, overwrite = TRUE)

buffer_geo <- (buff_1.0mi %>% mutate(distance = 1.0)) %>%
  bind_rows(buff_1.5mi %>% mutate(distance = 1.5)) %>%
  bind_rows(buff_3mi %>% mutate(distance = 3))  %>%
  st_transform(4326) %>%
  st_as_sf()
usethis::use_data(buffer_geo, overwrite = TRUE)

usethis::use_git_ignore(".DS_Store")

###############
# GEE files
###############
BUFF <- buff_1.5mi %>% 
  filter(status == "park") %>%  
  st_transform(26915) %>%
  rowid_to_column("num")
PARKS <- park_trail_geog_temp %>% 
  filter(status == "park") %>%
  select(-num) %>%
  st_transform(26915) %>% #https://epsg.io/26915
  st_combine()
# write_sf(BUFF, "/Users/escheh/Local work docs/Parks/park buffer less parks/arcBUFF.shp")
# write_sf(PARKS, "/Users/escheh/Local work docs/Parks/park buffer less parks/arcPARKS.shp")
