#' leaflet UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import leaflet
#' @import councilR
mod_leaflet_ui <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("map"), width = "100%", height = 775)
  )
}

#' leaflet Server Function
#'
#' @noRd
mod_leaflet_server <- function(input, output, session) {
  ns <- session$ns

  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lat = 44.963, lng = -93.22, zoom = 10) %>%
      addProviderTiles("CartoDB.DarkMatter",
        group = "Carto DarkMatter"
      ) %>%
      addProviderTiles("CartoDB.Positron",
        group = "Carto Positron"
      ) %>%
      addMapPane("parks_geo", zIndex = 420) %>%
      addPolygons(
        data = park_trail_geog$park,
        group = "Regional Parks",
        stroke = TRUE,
        weight = 0.5,
        color = councilR::colors$suppWhite,
        fill = TRUE,
        fillColor = councilR::colors$playGreen,
        fillOpacity = 0.8,
        options = pathOptions(pane = "parks_geo"),
        highlightOptions = highlightOptions(
          stroke = TRUE, color = "white", weight = 2,
          bringToFront = TRUE
        ),
        popup = ~ paste0(park_trail_geog$park$name, "<br>", park_trail_geog$park$agency),
        popupOptions = popupOptions(
          closeButton = FALSE,
          style = list(
            "font-size" = "18px",
            "font-family" = "Arial"
          )
        )
      ) %>%
      addPolylines(
        data = park_trail_geog$trail,
        group = "Regional Trails",
        stroke = TRUE,
        weight = 3,
        color = councilR::colors$suppGray,
        smoothFactor = 0.3,
        opacity = 0.5,
        popup = ~ paste0(park_trail_geog$trail$name, "<br>", park_trail_geog$trail$agency),
        highlightOptions = highlightOptions(
          stroke = TRUE,
          color = councilR::colors$suppGray, weight = 6,
          bringToFront = TRUE
        )
      ) %>%
      hideGroup("Regional Trails") %>%
      addLayersControl(
        position = "bottomright",
        overlayGroups = c(
          "Regional Parks",
          "Regional Trails"
        ),
        baseGroups = c(
          "Carto Positron",
          "Carto DarkMatter"
        ),
        options = layersControlOptions(collapsed = T)
      ) %>%
      leaflet::addScaleBar(position = c("bottomleft"))
  })
}



## To be copied in the UI
# mod_leaflet_ui("leaflet_ui_1")

## To be copied in the server
# callModule(mod_leaflet_server, "leaflet_ui_1")
