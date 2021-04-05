# ### quantile color palette labels example
# library(tidyverse)
# library(leaflet)
#
# a <- census_tract_map %>%
#   select(adj_usborn_per)
#
# qpal <- colorQuantile("Blues", a[[1]], n = 6)
# qpal_colors <- unique(qpal(sort(a[[1]]))) # hex codes
# qpal_labs <- quantile(a[[1]], seq(0, 1, (1/6)), na.rm = T) # depends on n from pal
# qpal_labs <- paste(lag(qpal_labs), qpal_labs, sep = " - ")[-1] # first lag is NA
# qpal_labsPERCENT <- paste0(qpal_labs, " %")
#
# leaflet() %>%
#   setView(
#     lat = 44.963,
#     lng = -93.22,
#     zoom = 9)%>%
#   addPolygons(
#     group = "Population data",
#     data = a,
#     stroke = TRUE,
#     color = councilR::colors$suppGray,
#     opacity = 0.6,
#     weight = 0.25,
#     fillOpacity = 0.6,
#     smoothFactor = 0.2,
#     fillColor = ~ qpal(a[[1]]),
#     options = list(zIndex = 0))  %>%
#   addLegend(title = "% US born population",
#             colors = qpal_colors,
#             labels = qpal_labsPERCENT,
#             opacity = 1,
#     position = "bottomleft")


#
# # -----
#
#
# ### quantile color palette labels example
#
# a <- regionalparks.acs::census_tract_map %>%
#   select(adj_usborn_per)
# pal <- colorNumeric(n = 7, palette = "Blues", domain = a[[1]])
# bpal <- colorBin("Blues", a[[1]], n = 6)
# bpal_colors <- unique(bpal(sort(a[[1]]))) # hex codes
# # bpal_labs <- quantile(a[[1]], seq(0, 1, (1/6)), na.rm = T) # depends on n from pal
# # qpal_labs <- paste(lag(qpal_labs), qpal_labs, sep = " - ")[-1] # first lag is NA
# # qpal_labsPERCENT <- paste0(qpal_labs, " %")
#
# leaflet() %>%
#   setView(
#     lat = 44.963,
#     lng = -93.22,
#     zoom = 9)%>%
#   addPolygons(
#     group = "Population data",
#     data = a,
#     stroke = TRUE,
#     color = councilR::colors$suppGray,
#     opacity = 0.6,
#     weight = 0.25,
#     fillOpacity = 0.6,
#     smoothFactor = 0.2,
#     fillColor = ~ pal,
#     options = list(zIndex = 0))  %>%
#   addLegend(title = "Some title",
#             colors = pal(a[[1]]),
#             # labels = qpal_labsPERCENT,
#             opacity = 1,
#             position = "bottomleft")
