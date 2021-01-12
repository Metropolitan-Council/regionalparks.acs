#' leaflet_base UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_leaflet_base_ui <- function(id) {
  ns <- NS(id)
  tagList()
}

#' leaflet_base Server Function
#'
#' @noRd
mod_leaflet_base_server <- function(input, output, session,
                                    add_all_parks = TRUE) {
  ns <- session$ns

  if (add_all_parks == TRUE) { # create a map with all the parks and trails

    output$ns <- renderLeaflet(quoted = TRUE, { #  map --------
      leaflet() %>%
        setView(
          lat = 44.963,
          lng = -93.22,
          zoom = 9
        ) %>%
        addMapPane(name = "Carto Positron", zIndex = 430) %>%
        addProviderTiles("CartoDB.PositronOnlyLabels",
          options = leafletOptions(pane = "Carto Positron"),
          group = "Carto Positron"
        ) %>%
        addProviderTiles("CartoDB.PositronNoLabels",
          group = "Carto Positron"
        ) %>%
        addProviderTiles(
          provider = providers$Esri.WorldImagery,
          group = "Aerial photography"
        ) %>%
        addMapPane("Agency boundaries", zIndex = 650) %>%
        addPolygons(
          data = agency_boundary,
          group = "Agency boundaries",
          stroke = T,
          color = "black",
          fill = F,
          weight = 2,
          options = pathOptions(pane = "Agency boundaries")
        ) %>%
        addMapPane("parks_geo", zIndex = 420) %>%
        addPolygons(
          data = park_trail_geog_LONG[park_trail_geog_LONG$status == "Park - existing", ],
          group = "Regional Parks - existing",
          stroke = TRUE,
          # weight = 0.5,
          color = e_col,
          fill = TRUE,
          fillColor = e_col,
          fillOpacity = 0.9,
          options = pathOptions(pane = "parks_geo"),
          highlightOptions = highlightOptions(
            stroke = TRUE,
            color = "black",
            weight = 6,
            bringToFront = TRUE,
            opacity = 1
          ),
          popup = ~popup_text,
          popupOptions = popupOptions(
            closeButton = FALSE,
            style = list(
              "font-size" = "18px",
              "font-family" = "Arial"
            )
          )
        ) %>%
        addPolygons(
          data = park_trail_geog_LONG[park_trail_geog_LONG$status == "Park - planned", ],
          group = "Regional Parks - planned",
          stroke = TRUE,
          # weight = 0.5,
          color = p_col,
          fill = TRUE,
          fillColor = p_col,
          fillOpacity = 0.9,
          options = pathOptions(pane = "parks_geo"),
          highlightOptions = highlightOptions(
            stroke = TRUE,
            color = "black", weight = 6,
            bringToFront = TRUE,
            opacity = 1
          ),
          popup = ~popup_text,
          popupOptions = popupOptions(
            closeButton = FALSE,
            style = list(
              "font-size" = "18px",
              "font-family" = "Arial"
            )
          )
        ) %>%
        addCircles(
          data = park_trail_geog_LONG[park_trail_geog_LONG$status == "Park - search", ],
          group = "Regional Parks - search",
          stroke = TRUE,
          radius = 2000,
          color = s_col,
          fill = TRUE,
          fillColor = s_col,
          fillOpacity = 0.9,
          options = pathOptions(pane = "parks_geo"),
          highlightOptions = highlightOptions(
            stroke = TRUE,
            color = "black", weight = 6,
            bringToFront = TRUE,
            opacity = 1
          ),
          popup = ~popup_text,
          popupOptions = popupOptions(
            closeButton = FALSE,
            style = list(
              "font-size" = "18px",
              "font-family" = "Arial"
            )
          )
        ) %>%
        addPolylines(
          data = park_trail_geog_LONG[park_trail_geog_LONG$status == "Trail - existing", ],
          group = "Regional Trails - existing",
          stroke = TRUE,
          weight = 3,
          color = e_col,
          smoothFactor = 0.3,
          opacity = 0.9,
          options = pathOptions(pane = "parks_geo"),
          popup = ~popup_text,
          highlightOptions = highlightOptions(
            stroke = TRUE,
            color = "black",
            weight = 6,
            bringToFront = TRUE
          )
        ) %>%
        addPolylines(
          data = park_trail_geog_LONG[park_trail_geog_LONG$status == "Trail - search", ],
          group = "Regional Trails - search",
          stroke = TRUE,
          weight = 3, # 3,
          color = s_col,
          smoothFactor = 0.3,
          opacity = 0.9, # 0.5,
          options = pathOptions(pane = "parks_geo"),
          popup = ~popup_text,
          highlightOptions = highlightOptions(
            stroke = TRUE,
            color = "black",
            weight = 6,
            bringToFront = TRUE
          )
        ) %>%
        addPolylines(
          data = park_trail_geog_LONG[park_trail_geog_LONG$status == "Trail - planned", ],
          group = "Regional Trails - planned",
          stroke = TRUE,
          weight = 3, # 3,
          color = p_col,
          smoothFactor = 0.3,
          opacity = 0.9, # 0.5,
          options = pathOptions(pane = "parks_geo"),
          popup = ~popup_text,
          highlightOptions = highlightOptions(
            stroke = TRUE,
            color = "black",
            weight = 6,
            bringToFront = TRUE
          )
        ) %>%
        addMapPane("water_access", zIndex = 431) %>%
        addAwesomeMarkers(
          group = "Water Access",
          data = regionalparks.acs::water_access,
          icon = iconwater,
          options = pathOptions(pane = "water_access")
        ) %>%
        groupOptions(
          group = "Water Access",
          zoomLevels = 13:20
        ) %>%
        addMapPane("entrance", zIndex = 432) %>%
        addAwesomeMarkers(
          group = "Park Entrance",
          data = regionalparks.acs::entrance,
          icon = iconentry,
          options = pathOptions(pane = "entrance")
        ) %>%
        groupOptions(
          group = "Park Entrance",
          zoomLevels = 13:20
        ) %>%
        addMapPane("trans", zIndex = 430) %>%
        addCircles( # Markers(
          data = regionalparks.acs::trans_stops,
          group = "Active transit stops",
          radius = 20,
          fill = T,
          stroke = TRUE,
          weight = 2,
          color = councilR::colors$transitRed,
          fillColor = councilR::colors$transitRed,
          options = pathOptions(pane = "trans")
        ) %>%
        groupOptions(
          group = "Active transit stops",
          zoomLevels = 13:20
        ) %>%
        addMapPane("riverlake", zIndex = 429) %>%
        addPolygons(
          data = regionalparks.acs::river_lake,
          group = "Rivers & Lakes",
          stroke = TRUE,
          # weight = 0.5,
          color = "black",
          fill = TRUE,
          fillColor = "black",
          fillOpacity = 0.9,
          options = pathOptions(pane = "riverlake")
        ) %>%
        hideGroup(
          c(
            "Regional Parks - planned",
            "Regional Trails - planned",
            "Regional Parks - search",
            "Regional Trails - search",
            "Active transit stops",
            "Water Access",
            "Park Entrance",
            "Rivers & Lakes"
          )
        ) %>%
        addLayersControl(
          position = "bottomright",
          overlayGroups = c(
            "Regional Parks - existing",
            "Regional Trails - existing",
            "Regional Parks - planned",
            "Regional Trails - planned",
            "Regional Parks - search",
            "Regional Trails - search",
            "Demographic data",
            "Active transit stops",
            "Water Access",
            "Park Entrance",
            "Rivers & Lakes",
            "Agency boundaries"
          ),
          baseGroups = c(
            "Carto Positron",
            "Aerial photography"
          ),
          options = layersControlOptions(collapsed = T)
        ) %>%
        leaflet::addScaleBar(position = c("bottomleft"))
    })
  } else if (add_all_parks == FALSE) {
    output$ns <- renderLeaflet(quoted = TRUE, { #  map --------
      leaflet() %>%
        setView(
          lat = 44.963,
          lng = -93.22,
          zoom = 9
        ) %>%
        addMapPane(name = "Carto Positron", zIndex = 430) %>%
        addProviderTiles("CartoDB.PositronOnlyLabels",
          options = leafletOptions(pane = "Carto Positron"),
          group = "Carto Positron"
        ) %>%
        addProviderTiles("CartoDB.PositronNoLabels",
          group = "Carto Positron"
        ) %>%
        addProviderTiles(
          provider = providers$Esri.WorldImagery,
          group = "Aerial photography"
        ) %>%
        addMapPane("Agency boundaries", zIndex = 650) %>%
        addPolygons(
          data = agency_boundary,
          group = "Agency boundaries",
          stroke = T,
          color = "black",
          fill = F,
          weight = 2,
          options = pathOptions(pane = "Agency boundaries")
        ) %>%
        addMapPane("water_access", zIndex = 431) %>%
        addAwesomeMarkers(
          group = "Water Access",
          data = regionalparks.acs::water_access,
          icon = iconwater,
          options = pathOptions(pane = "water_access")
        ) %>%
        groupOptions(
          group = "Water Access",
          zoomLevels = 13:20
        ) %>%
        addMapPane("entrance", zIndex = 432) %>%
        addAwesomeMarkers(
          group = "Park Entrance",
          data = regionalparks.acs::entrance,
          icon = iconentry,
          options = pathOptions(pane = "entrance")
        ) %>%
        groupOptions(
          group = "Park Entrance",
          zoomLevels = 13:20
        ) %>%
        addMapPane("trans", zIndex = 430) %>%
        addCircles( # Markers(
          data = regionalparks.acs::trans_stops,
          group = "Active transit stops",
          radius = 20,
          fill = T,
          stroke = TRUE,
          weight = 2,
          color = councilR::colors$transitRed,
          fillColor = councilR::colors$transitRed,
          options = pathOptions(pane = "trans")
        ) %>%
        groupOptions(
          group = "Active transit stops",
          zoomLevels = 13:20
        ) %>%
        addMapPane("riverlake", zIndex = 429) %>%
        addPolygons(
          data = regionalparks.acs::river_lake,
          group = "Rivers & Lakes",
          stroke = TRUE,
          # weight = 0.5,
          color = "black",
          fill = TRUE,
          fillColor = "black",
          fillOpacity = 0.9,
          options = pathOptions(pane = "riverlake")
        ) %>%
        hideGroup(
          c(
            "Active transit stops",
            "Water Access",
            "Park Entrance",
            "Rivers & Lakes"
          )
        ) %>%
        addLayersControl(
          position = "bottomright",
          overlayGroups = c(
            "Parks and trails",
            "Buffers",
            "Demographic data",
            "Active transit stops",
            "Water Access",
            "Park Entrance",
            "Rivers & Lakes",
            "Agency boundaries"
          ),
          baseGroups = c(
            "Stamen Toner",
            "Carto Positron",
            "Aerial photography"
          ),
          options = layersControlOptions(collapsed = T)
        ) %>%
        leaflet::addScaleBar(position = c("bottomleft"))
    })
  }
}

## To be copied in the UI
# mod_leaflet_base_ui("leaflet_base_ui_1")

## To be copied in the server
# callModule(mod_leaflet_base_server, "leaflet_base_ui_1")
