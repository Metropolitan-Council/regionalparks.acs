#' map_base UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_map_base_ui <- function(id) {
  ns <- NS(id)
  tagList()
  # use_waiter()
}

#' map_base Server Function
#'
#' @noRd
mod_map_base_server <- function(input, output, session) {
  ns <- session$ns
  # w <- Waiter$new()#, html="Please wait")#, hide_on_render=T)
  

  output$ns <- renderLeaflet(quoted = TRUE, {
    # w$show()
    # waiter_show()
    leaflet() %>%
      setView(
        lat = 44.963,
        lng = -93.22,
        zoom = 10
      ) %>%
      leaflet.extras::addDrawToolbar(
        editOptions=editToolbarOptions(selectedPathOptions=selectedPathOptions()),
        polygonOptions = F,
        circleOptions =F, 
        rectangleOptions = F,
        circleMarkerOptions = F,
        markerOptions = F,
        polylineOptions = drawPolylineOptions(
          shapeOptions = drawShapeOptions(color = "black",
                                          weight = 2), 
          guidelineDistance = 1, 
          metric = F,
          feet = T)) %>% 
      # addMeasure(primaryLengthUnit="miles", secondaryLengthUnit="feet") %>%
      addMapPane("parks_geo", zIndex = 420) %>%
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
          "Buffers",
          "Agency boundaries",
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
          "Population data",
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
    # waiter_hide()
    # w$hide
  })
}

## To be copied in the UI
# mod_map_base_ui("map_base_ui_1")

## To be copied in the server
# callModule(mod_map_base_server, "map_base_ui_1")
