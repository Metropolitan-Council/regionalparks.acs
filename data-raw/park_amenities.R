# park entrances -----------------

entrance <- sf::read_sf("/Volumes/shared/CommDev/Research/Public/GIS/Parks/BaseMaps/Entrances_parks.shp") %>%
  st_transform(4326)

usethis::use_data(entrance, overwrite = TRUE)


# water access -----------------
metc <- tibble(counties = c("Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington"))

temp <- tempfile()
download.file("ftp://ftp.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_dnr/loc_water_access_sites/gpkg_loc_water_access_sites.zip",
  destfile = temp
)

water_access <- sf::read_sf(unzip(temp, "loc_water_access_sites.gpkg")) %>%
  filter(COUNTYNAME %in% metc$counties) %>%
  st_transform(4326)

fs::file_delete("loc_water_access_sites.gpkg")

usethis::use_data(water_access, overwrite = TRUE)

usethis::use_git_ignore(".DS_Store")


# water bodies -----------------

temp <- tempfile()
download.file("ftp://ftp.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/water_lakes_rivers/gpkg_water_lakes_rivers.zip", destfile = temp)

river_lake <- sf::read_sf(unzip(temp, "water_lakes_rivers.gpkg")) %>%
  st_transform(4326)

fs::file_delete("water_lakes_rivers.gpkg")

usethis::use_data(river_lake, overwrite = TRUE)
