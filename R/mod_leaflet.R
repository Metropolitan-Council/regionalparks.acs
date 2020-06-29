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
#' @import leaflet.extras
mod_leaflet_ui <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("map"), width = "100%", height = 900)
  )
}

#' leaflet Server Function
#'
#' @noRd
#' @import leaflet
#' @import councilR
#' @import leaflet.extras
mod_leaflet_server <- function(input, output, session, tract_data = tract_data) {
  ns <- session$ns

  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lat = 44.963, lng = -93.22, zoom = 10) %>%
      addProviderTiles(
        provider = providers$Esri.WorldImagery,
        group = "Esri Imagery"
      ) %>%
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
        popup = ~ paste0(park_trail_geog$park$name, "<br>", "<em>", park_trail_geog$park$agency, "</em>"),
        popupOptions = popupOptions(
          closeButton = FALSE,
          style = list(
            "font-size" = "18px",
            "font-family" = "Arial"
          )
        )
      ) %>%
      addPolygons(
        data = county_outlines,
        group = "County Outlines",
        fill = FALSE,
        stroke = TRUE,
        weight = 0.75,
        color = councilR::colors$suppGray
      ) %>%
      addPolylines(
        data = park_trail_geog$trail,
        group = "Regional Trails",
        stroke = TRUE,
        weight = 3,
        color = councilR::colors$suppGray,
        smoothFactor = 0.3,
        opacity = 0.5,
        options = pathOptions(pane = "parks_geo"),
        popup = ~ paste0(park_trail_geog$trail$name, "<br>", "<em>", park_trail_geog$trail$agency, "</em>"),
        highlightOptions = highlightOptions(
          stroke = TRUE,
          color = councilR::colors$suppGray, weight = 6,
          bringToFront = TRUE
        )
      ) %>%
      leaflet.extras::addDrawToolbar(
        targetGroup = "Drawings",
        polygonOptions = FALSE,
        polylineOptions = FALSE,
        rectangleOptions = FALSE,
        markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        circleOptions = drawCircleOptions(
          showRadius = "mi",
          feet = FALSE,
          metric = FALSE,
          shapeOptions = drawShapeOptions(
            weight = 4,
            # clickable = TRUE,
            color = councilR::colors$suppBlack,
            fill = FALSE
          )
        ),
        editOptions = editToolbarOptions()
      ) %>%
      addStyleEditor() %>%
      hideGroup(
        c(
          "Regional Trails",
          "County Outlines"
        )
      ) %>%
      addLayersControl(
        position = "bottomright",
        overlayGroups = c(
          "Regional Parks",
          "Regional Trails",
          "County Outlines",
          "Census Tracts",
          "Drawings"
        ),
        baseGroups = c(
          "Carto Positron",
          "Carto DarkMatter",
          "Esri Imagery"
        ),
        options = layersControlOptions(collapsed = T)
      ) %>%
      leaflet::addScaleBar(position = c("bottomleft"))
  })

  observeEvent(tract_data$tract_data, {
    pal <- if (tract_data$selected_var == "Income, Median Household Income") {
      colorQuantile(
        palette = tract_data$color_pal,
        n = 5,
        # reverse = TRUE,
        domain = tract_data$tract_data[[1]]
      )
    } else {
      colorNumeric(
        palette = tract_data$color_pal,
        domain = tract_data$tract_data[[1]]
      )
    }
    # browser()
    leafletProxy("map") %>%
      clearGroup("Census Tracts") %>%
      clearControls() %>%
      addPolygons(
        data = tract_data$tract_data,
        group = "Census Tracts",
        stroke = TRUE,
        color = councilR::colors$suppGray,
        opacity = 0.6,
        weight = 0.25,
        fillOpacity = 0.6,
        smoothFactor = 0.2,
        fillColor = ~ pal(tract_data$tract_data[[1]]),

        popup = if (tract_data$selected_var == "Income, Median Household Income") {
          ~ paste0(tags$strong(tract_data$selected_var), " $", format(tract_data$tract_data[[1]], big.mark = ","))
        } else {
          ~ paste0(
            tags$strong(tract_data$selected_var),
            " ",
            tract_data$tract_data[[1]], "%"
          )
        },
        options = list(zIndex = 0),
        popupOptions = popupOptions(closeOnClick = TRUE)
      ) %>%
      addLegend(
        title = if (tract_data$selected_var == "Income, Median Household Income") {
          "Income by Percentile"
        } else {
          "% of pop."
        },
        position = "bottomleft",
        group = "Census Tracts",
        layerId = "Census Tracts",
        pal = pal,
        values = tract_data$tract_data[[1]],
        labFormat = if (tract_data$selected_var == "Income, Median Household Income") {
          labelFormat()
        } else {
          labelFormat(suffix = "%")
        }
      )
  })
}



## To be copied in the UI
# mod_leaflet_ui("leaflet_ui_1")

## To be copied in the server
# callModule(mod_leaflet_server, "leaflet_ui_1")
