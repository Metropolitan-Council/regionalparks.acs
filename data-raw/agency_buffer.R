## code to prepare `park_trail_geog` dataset goes here

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


## SetUpThings -----------------------------------------------------------------------
load(file = "./data/block_group.rda")
load(file = "./data/park_trail_geog.rda")

agency_filter <- tibble(agency = c("Anoka County Parks and Recreation",
                                   "Bloomington Parks and Recreation",
                                   "Carver County Parks and Recreation" , 
                                   "Dakota County Parks",
                                   "Minneapolis Park and Recreation Board",
                                   "Ramsey County Parks and Recreation" ,
                                   "Scott County Parks",
                                   "St. Paul Parks and Recreation",
                                   "Three Rivers Park District",
                                   "Washington County Parks"),
                        num = c(1:10))

park_trail_geog_temp <- bind_rows(park_trail_geog, .id = "status") %>%
  full_join(agency_filter) %>%
  mutate(NAME = paste(name, num, sep = "_")) 

## 1 mile buffer ----------------------------------------------------------------------

buff_1mi <- park_trail_geog_temp %>% 
  st_transform(3857) %>% #https://epsg.io/3857\
  st_buffer(dist = 1609.34) %>% #the 3857 projection uses meters as a distance, so 1 mi = 
  group_by(agency, NAME, status) %>%
  summarise(geometry = st_union(geom))

buffer_block_group_1mi <- block_group %>%
  st_transform(3857) %>% #https://epsg.io/3857\
  st_intersection(buff_1mi) %>%
  group_by(GEOID, agency, NAME, status) %>%
  summarise(geometry = st_union(geometry)) %>%
  mutate(Flag = "include") %>%
  as_tibble() %>%
  select(GEOID, agency, NAME, status, Flag) %>%
  left_join(block_group) %>%
  st_as_sf()

# buffer_block_group_1mi %>% filter(NAME == "Como Regional Park_8") %>%
#   ggplot() +
#   geom_sf(data = buff_1mi, col = "grey60", fill = "grey60") +
#   geom_sf(aes(fill = agency, col = agency)) +
#   geom_sf(data = park_trail_geog_temp, fill = "black", col = "black") +
#   scale_fill_brewer(palette = "Paired") +
#   scale_color_brewer(palette = "Paired") +
#   theme(axis.text = element_text(size = 8), 
#         axis.text.x = element_text(angle = 40, hjust = 1))

## 3 mile buffer ----------------------------------------------------------------------

buff_3mi <- park_trail_geog_temp %>% 
  st_transform(3857) %>% #https://epsg.io/3857\
  st_buffer(dist = 1609.34*3) %>% #the 3857 projection uses meters as a distance, so 1 mi = 
  group_by(agency, NAME, status) %>%
  summarise(geometry = st_union(geom))

buffer_block_group_3mi <- block_group %>%
  st_transform(3857) %>% #https://epsg.io/3857\
  st_intersection(buff_3mi) %>%
  group_by(GEOID, agency, NAME, status) %>%
  summarise(geometry = st_union(geometry)) %>%
  mutate(Flag = "include") %>%
  as_tibble() %>%
  select(GEOID, agency, NAME, status, Flag) %>%
  left_join(block_group) %>%
  st_as_sf()

## 5 mile buffer ----------------------------------------------------------------------

buff_5mi <- park_trail_geog_temp %>% 
  st_transform(3857) %>% #https://epsg.io/3857\
  st_buffer(dist = 1609.34*5) %>% #the 3857 projection uses meters as a distance, so 1 mi = 
  group_by(agency, NAME, status) %>%
  summarise(geometry = st_union(geom))

buffer_block_group_5mi <- block_group %>%
  st_transform(3857) %>% #https://epsg.io/3857\
  st_intersection(buff_5mi) %>%
  group_by(GEOID, agency, NAME, status) %>%
  summarise(geometry = st_union(geometry)) %>%
  mutate(Flag = "include") %>%
  as_tibble() %>%
  select(GEOID, agency, NAME, status, Flag) %>%
  left_join(block_group) %>%
  st_as_sf()

## Combine ---------------------------------------------------------------------
buffer_distances <- list(buffer_block_group_1mi, buffer_block_group_3mi, buffer_block_group_5mi)
names(buffer_distances) <- c(
  "1 mi",
  "3 mi",
  "5 mi"
)

usethis::use_data(buffer_distances, overwrite = TRUE)

usethis::use_git_ignore(".DS_Store")
