#' pop_map UI Function
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
mod_pop_map_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # HTML('</p>The Metropolitian Council publishes current population estimates and future forecasted population estimates. Current populaton estimates are available for Census block groups. Future forecasts are based on 2010 Census data and city comprehensive plans and available at the transportation analysis zone (a coarser spatial resolution). Given the differential methods and geographies used in calcuating current and future populations, we will not perform further analyses on these data. However, the overarching patterns still may be useful in parks planning. More information and raw data can be found on the <a href = "https://metrocouncil.org/Data-and-Maps/Research-and-Data/Thrive-2040-Forecasts.aspx">Metropolitian Council website</a>.</p>'),

    leafletOutput(ns("popmap"), width = "100%", height = 600)
  )
}

#' pop_map Server Function
#'
#' @noRd
mod_pop_map_server <- function(input, output, session,
                               pop_data = pop_data) {
  ns <- session$ns


  output$popmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Stamen.TonerLite",
        group = "Stamen Toner"
      ) %>%
      addProviderTiles("CartoDB.Positron",
        group = "Carto Positron"
      ) %>%
      addProviderTiles(
        provider = providers$Esri.WorldImagery,
        group = "Esri Imagery"
      ) %>%
      addMapPane("parks_geo", zIndex = 420) %>%
      addPolygons(
        data = park_trail_geog_LONG[park_trail_geog_LONG$status == "Park - existing", ],
        group = "Regional Parks - existing",
        stroke = TRUE,
        color = e_col, 
        fill = TRUE,
        fillColor = e_col, 
        fillOpacity = 0.8,
        options = pathOptions(pane = "parks_geo"),
        highlightOptions = highlightOptions(
          stroke = TRUE,
          color = "black",
          weight = 6,
          bringToFront = TRUE,
          opacity = 1
        ),
        popup = ~ paste0(
          "<b>", park_trail_geog_LONG[park_trail_geog_LONG$status == "Park - existing", ]$status, "</b>", "<br>",
          park_trail_geog_LONG[park_trail_geog_LONG$status == "Park - existing", ]$name, "<br>", "<em>",
          park_trail_geog_LONG[park_trail_geog_LONG$status == "Park - existing", ]$agency, "</em>"
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
        data = park_trail_geog_LONG[park_trail_geog_LONG$status == "Park - planned", ],
        group = "Regional Parks - planned",
        stroke = TRUE,
        color = p_col, 
        fill = TRUE,
        fillColor = p_col, 
        fillOpacity = 0.8,
        options = pathOptions(pane = "parks_geo"),
        highlightOptions = highlightOptions(
          stroke = TRUE,
          color = "black", weight = 6,
          bringToFront = TRUE,
          opacity = 1
        ),
        popup = ~ paste0(
          "<b>", park_trail_geog_LONG[park_trail_geog_LONG$status == "Park - planned", ]$status, "</b>", "<br>",
          park_trail_geog_LONG[park_trail_geog_LONG$status == "Park - planned", ]$name, "<br>",
          "<em>", park_trail_geog_LONG[park_trail_geog_LONG$status == "Park - planned", ]$agency, "</em>"
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
        data = park_trail_geog_LONG[park_trail_geog_LONG$status == "Park - search", ],
        group = "Regional Parks - search",
        stroke = TRUE,
        radius = 2000,
        color = s_col, 
        fill = TRUE,
        fillColor = s_col, 
        fillOpacity = 0.8, 
        options = pathOptions(pane = "parks_geo"),
        highlightOptions = highlightOptions(
          stroke = TRUE,
          color = "black", weight = 6,
          bringToFront = TRUE,
          opacity = 1
        ),
        popup = ~ paste0(
          "<b>", park_trail_geog_LONG[park_trail_geog_LONG$status == "Park - search", ]$status, "</b>", "<br>",
          "<em>", park_trail_geog_LONG[park_trail_geog_LONG$status == "Park - search", ]$name, "</em>"
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
        weight = 2, 
        color = councilR::colors$suppGray
      ) %>%
      addPolylines(
        data = park_trail_geog_LONG[park_trail_geog_LONG$status == "Trail - existing", ],
        group = "Regional Trails - existing",
        stroke = TRUE,
        weight = 3, # 3,
        color = e_col, 
        smoothFactor = 0.3,
        opacity = 1, # 0.5,
        options = pathOptions(pane = "parks_geo"),
        popup = ~ paste0(
          "<b>", park_trail_geog_LONG[park_trail_geog_LONG$status == "Trail - existing", ]$status, "</b>", "<br>",
          park_trail_geog_LONG[park_trail_geog_LONG$status == "Trail - existing", ]$name, "<br>",
          "<em>", park_trail_geog_LONG[park_trail_geog_LONG$status == "Trail - existing", ]$agency, "</em>"
        ),
        highlightOptions = highlightOptions(
          stroke = TRUE,
          color = "black", # "white",
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
        opacity = 1, # 0.5,
        options = pathOptions(pane = "parks_geo"),
        popup = ~ paste0(
          "<b>", park_trail_geog_LONG[park_trail_geog_LONG$status == "Trail - search", ]$status, "</b>", "<br>",
          park_trail_geog_LONG[park_trail_geog_LONG$status == "Trail - search", ]$name, "<br>",
          "<em>", park_trail_geog_LONG[park_trail_geog_LONG$status == "Trail - search", ]$agency, "<em>"
        ),
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
        weight = 3, 
        color = p_col, 
        smoothFactor = 0.3,
        opacity = 1, 
        options = pathOptions(pane = "parks_geo"),
        popup = ~ paste0(
          "<b>", park_trail_geog_LONG[park_trail_geog_LONG$status == "Trail - planned", ]$status, "</b>", "<br>",
          park_trail_geog_LONG[park_trail_geog_LONG$status == "Trail - planned", ]$name, "<br>",
          "<em>", park_trail_geog_LONG[park_trail_geog_LONG$status == "Trail - planned", ]$agency, "</em>"
        ),
        highlightOptions = highlightOptions(
          stroke = TRUE,
          color = "black",
          weight = 6,
          bringToFront = TRUE
        )
      ) %>%
      hideGroup(
        c(
          "Regional Parks - planned",
          "Regional Trails - planned",
          "Regional Parks - search",
          "Regional Trails - search"
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
          "Pop. Estimates"
        ),
        baseGroups = c(
          "Carto Positron",
          "Stamen Toner",
          "Esri Imagery"
        ),
        options = layersControlOptions(collapsed = T)
      ) %>%
      leaflet::addScaleBar(position = c("bottomleft"))
  })

  observeEvent(pop_data$pop_data, {
    pal <-
      colorQuantile(
        palette = "Purples",
        n = 7,
        domain = pop_data$pop_data[[1]]
      )

    # browser()
    leafletProxy("popmap") %>%
      clearGroup("Pop. Estimates") %>%
      clearControls() %>%
      addPolygons(
        data = pop_data$pop_data,
        group = "Pop. Estimates",
        stroke = TRUE,
        color = councilR::colors$suppGray,
        opacity = 0.6,
        weight = 0.25,
        fillOpacity = 0.6,
        smoothFactor = 0.2,
        fillColor = ~ colorQuantile(
          palette = "Purples",
          n = 7,
          # reverse = TRUE,
          domain = pop_data$pop_data[[1]]
        )(pop_data$pop_data[[1]]),

        popup =
          ~ paste0(
            tags$strong(pop_data$selected_var),
            ": ",
            format(pop_data$pop_data[[1]], big.mark = ",")
          ),
        # options = list(zIndex = 0),
        popupOptions = popupOptions(closeOnClick = TRUE)
      ) %>%
      addLegend("topright",
        pal = colorQuantile(
          n = 7,
          palette = "Purples",
          domain = pop_data$pop_data[[1]]
        ),
        values = (pop_data$pop_data[[1]]),
        title = paste0((names(pop_data$pop_data)[[1]])), # (names(summary_util$map_bg_data)[[1]]),
        opacity = 1,
        group = "Pop.Estimates"#,
        # labFormat = function(type, cuts, p) {
        #   n <- length(cuts)
        #   paste0(format(round(cuts[-n], 0), big.mark = ","), " &ndash; ", format(round(cuts[-1], 0), big.mark = ","))
        # }
      )
  })
}

## To be copied in the UI
# mod_pop_map_ui("pop_map_ui_1")

## To be copied in the server
# callModule(mod_pop_map_server, "pop_map_ui_1")
