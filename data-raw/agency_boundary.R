#code to prepare `agency_boundary`


agency_boundary <- read_sf("/Volumes/shared/CommDev/Research/Public/GIS/Parks/Park_Operating_Agencies.shp") %>%
  mutate(COMCD_DESC = recode(COMCD_DESC, "Minneapolis" = "MPRB")) %>%
  rename(agency = COMCD_DESC) %>%
  select(agency) %>%
  st_transform(4326) #for leaflet


usethis::use_data(agency_boundary, overwrite = TRUE)
