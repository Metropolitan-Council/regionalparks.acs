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
#' @import sf
mod_leaflet_ui <- function(id) {
  ns <- NS(id)
  tagList(
    HTML("<p>Select 1 ACS variable. The tracts’ colors correspond to 2014-2018 (5-year) American Community Survey (ACS) demographic metric selected. Each demographic characteristic is shown as a percentage of the total population, with the exception of median household income, which is displayed in dollars. The darker the color, the higher the percentage (or income in dollars).</p>
    </p>Statistics produced for American Community Survey and included in ACS tables are survey-based estimates and are subject to error. The errors derive from research design (including instrument bias, data frame, and sampling), the survey data collection (non-response bias and response errors), and processing by the Census Bureau (data coding, compilation processes, and case weighting), as well as statistical inference error and uncertainty (which are related to sample size and variance within the measured attributes).</p>
         <p> EE comment: add transit stops. planned transit stops also possible, but metadata reveals how flexible these planned stops are (ie blue line light rail) (https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/trans_pland_transitway_station/metadata/metadata.html)"),

    leafletOutput(ns("map"), width = "100%", height = 700)
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
      # addProviderTiles("CartoDB.DarkMatter",
      #   group = "Carto DarkMatter"
      # ) %>%
      addProviderTiles("CartoDB.Positron",
        group = "Carto Positron"
      ) %>%
      addMapPane("parks_geo", zIndex = 420) %>%
      addPolygons(
        data = park_trail_geog$park,
        group = "Regional Parks - existing",
        stroke = TRUE,
        # weight = 0.5,
        color = e_col, # councilR::colors$playGreen, # councilR::colors$suppWhite,
        fill = TRUE,
        fillColor = e_col, # councilR::colors$playGreen,
        fillOpacity = 1, # 0.8,
        options = pathOptions(pane = "parks_geo"),
        highlightOptions = highlightOptions(
          stroke = TRUE,
          color = "black",
          weight = 6,
          bringToFront = TRUE,
          opacity = 1
        ),
        popup = ~ paste0(
          "<b>", park_trail_geog$park$status, "</b>", "<br>",
          park_trail_geog$park$name, "<br>", "<em>",
          park_trail_geog$park$agency, "</em>"
        ),
        popupOptions = popupOptions(
          closeButton = FALSE,
          style = list(
            "font-size" = "18px",
            "font-family" = "Arial"
          )
        )
      ) %>%
      addPolygons(
        data = park_trail_geog$park_planned,
        group = "Regional Parks - planned",
        stroke = TRUE,
        # weight = 0.5,
        color = p_col, # councilR::colors$suppGray,
        fill = TRUE,
        fillColor = p_col, # ouncilR::colors$suppGray,
        fillOpacity = 1, # 0.8,
        options = pathOptions(pane = "parks_geo"),
        highlightOptions = highlightOptions(
          stroke = TRUE,
          color = "black", weight = 6,
          bringToFront = TRUE,
          opacity = 1
        ),
        popup = ~ paste0(
          "<b>", park_trail_geog$park_planned$status, "</b>", "<br>",
          park_trail_geog$park_planned$name, "<br>",
          "<em>", park_trail_geog$park_planned$agency, "</em>"
        ),
        popupOptions = popupOptions(
          closeButton = FALSE,
          style = list(
            "font-size" = "18px",
            "font-family" = "Arial"
          )
        )
      ) %>%
      addCircles(
        data = park_trail_geog$park_search,
        group = "Regional Parks - search",
        stroke = TRUE,
        radius = 2000,
        # weight = 0.5,
        color = s_col, # councilR::colors$suppGray,
        fill = TRUE,
        fillColor = s_col, # councilR::colors$suppGray,
        fillOpacity = 1, # 0.8,
        options = pathOptions(pane = "parks_geo"),
        highlightOptions = highlightOptions(
          stroke = TRUE,
          color = "black", weight = 6,
          bringToFront = TRUE,
          opacity = 1
        ),
        popup = ~ paste0(
          "<b>", park_trail_geog$park_search$status, "</b>", "<br>",
          "<em>", park_trail_geog$park_search$name, "</em>"
        ),
        popupOptions = popupOptions(
          closeButton = FALSE,
          style = list(
            "font-size" = "18px",
            "font-family" = "Arial"
          )
        )
      ) %>%
      addPolygons(
        data = agency_boundary,
        group = "Agency boundaries",
        fill = FALSE,
        stroke = TRUE,
        weight = 2, # 0.75,
        color = councilR::colors$suppGray
      ) %>%
      addPolylines(
        data = park_trail_geog$trail,
        group = "Regional Trails - existing",
        stroke = TRUE,
        weight = 3, # 3,
        color = e_col, # councilR::colors$playGreen,
        smoothFactor = 0.3,
        opacity = 1, # 0.5,
        options = pathOptions(pane = "parks_geo"),
        popup = ~ paste0(
          "<b>", park_trail_geog$trail$status, "</b>", "<br>",
          park_trail_geog$trail$name, "<br>",
          "<em>", park_trail_geog$trail$agency, "</em>"
        ),
        highlightOptions = highlightOptions(
          stroke = TRUE,
          # color = councilR::colors$suppGray,
          color = "black", # "white",
          weight = 6,
          bringToFront = TRUE
        )
      ) %>%
      addPolylines(
        data = park_trail_geog$trail_search,
        group = "Regional Trails - search",
        stroke = TRUE,
        weight = 3, # 3,
        color = s_col, # councilR::colors$suppGray,
        smoothFactor = 0.3,
        opacity = 1, # 0.5,
        options = pathOptions(pane = "parks_geo"),
        popup = ~ paste0(
          "<b>", park_trail_geog$trail_search$status, "</b>", "<br>",
          park_trail_geog$trail_search$name, "<br>",
          "<em>", park_trail_geog$trail_search$agency, "<em>"
        ),
        highlightOptions = highlightOptions(
          stroke = TRUE,
          # color = councilR::colors$suppGray,
          color = "black",
          weight = 6,
          bringToFront = TRUE
        )
      ) %>%
      addPolylines(
        data = park_trail_geog$trail_planned,
        group = "Regional Trails - planned",
        stroke = TRUE,
        weight = 3, # 3,
        color = p_col, # councilR::colors$suppGray,
        smoothFactor = 0.3,
        opacity = 1, # 0.5,
        options = pathOptions(pane = "parks_geo"),
        popup = ~ paste0(
          "<b>", park_trail_geog$trail_planned$status, "</b>", "<br>",
          park_trail_geog$trail_planned$name, "<br>",
          "<em>", park_trail_geog$trail_planned$agency, "</em>"
        ),
        highlightOptions = highlightOptions(
          stroke = TRUE,
          # color = councilR::colors$suppGray,
          color = "black",
          weight = 6,
          bringToFront = TRUE
        )
      ) %>%
      # addPolylines(
      #   data = regionalparks.acs::trans_routes,
      #   group = "Transit",
      #   stroke = TRUE,
      #   weight = 2, # 0.75,
      #   color = councilR::colors$transitRed
      # ) %>%
      addCircles(
        data = regionalparks.acs::trans_stops,
        group = "Transit",
        radius = 3,
        fill = T,
        stroke = TRUE,
        weight = 2, # 0.75,
        color = councilR::colors$transitRed,
        fillColor = councilR::colors$transitRed
      ) %>%
      hideGroup(
        c(
          "Regional Parks - planned",
          "Regional Trails - planned",
          "Regional Parks - search",
          "Regional Trails - search",
          "Transit"
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
          "Agency boundaries",
          "Census Tracts",
          "Transit"
        ),
        baseGroups = c(
          "Carto Positron",
          # "Carto DarkMatter",
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
