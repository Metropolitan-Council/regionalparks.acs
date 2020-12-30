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
                               summary_poputil,
                               selected_popvars#,
                               # parktrail_util,
                               # selected_parktrail
                               ) {
  ns <- session$ns

  popkey <- tibble::tribble( #------
    ~goodname,
    ~"popvar",
    "2019 pop.",
    "PopEst_2019",
    "2019 pop. density",
    "PopDens_2019",
    "2040 pop.",
    "POP2040",
    "2040 pop. density",
    "popdens_2040_mi",
    "Growth, relative",
    "growth_rel_10_40",
    "Growth, absolute",
    "growth_abs_10_40"
  )

  output$popmap <- renderLeaflet({ # pop map --------
    leaflet() %>%
      # setView(
      #   lat = 44.963,
      #   lng = -93.22,
      #   zoom = 9
      # ) %>%
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
        data = park_trail_geog_LONG[park_trail_geog_LONG$status == "Park - existing", ], # https://cran.r-project.org/web/packages/sf/vignettes/sf4.html
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
        # weight = 0.5,
        color = p_col,
        fill = TRUE,
        fillColor = p_col, # ouncilR::colors$suppGray,
        fillOpacity = 0.9,
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
        # weight = 0.5,
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
      
      addPolylines(
        data = park_trail_geog_LONG[park_trail_geog_LONG$status == "Trail - existing", ],
        group = "Regional Trails - existing",
        stroke = TRUE,
        weight = 3, # 3,
        color = e_col,
        smoothFactor = 0.3,
        opacity = 0.9, # 0.5,
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
        opacity = 0.9, # 0.5,
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
        weight = 3, # 3,
        color = p_col,
        smoothFactor = 0.3,
        opacity = 0.9, # 0.5,
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
      )  %>%
      addMapPane("trans", zIndex = 430) %>%
      
      addCircles(#Markers(
        data = regionalparks.acs::trans_stops,
        group = "Transit",
        radius = 10,
        fill = T,
        stroke = TRUE,
        weight = 2, # 0.75,
        color = councilR::colors$transitRed,
        fillColor = councilR::colors$transitRed,
        options = pathOptions(pane = "trans")
      ) %>%
      groupOptions(
        group = "Transit",
        zoomLevels = 13:20
      )   %>%
      
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
          "Population data",
          "Transit",
          "Agency boundaries"
        ),
        baseGroups = c(
          "Stamen Toner",
          "Carto Positron",
          "Esri Imagery"
        ),
        options = layersControlOptions(collapsed = T)
      ) %>%
      leaflet::addScaleBar(position = c("bottomleft")) 
  }) #----


  outputOptions(output, "popmap", suspendWhenHidden = FALSE)

  observeEvent(list(selected_popvars$input_pop),{
    (leafletProxy("popmap") %>%
      clearGroup("Population data") %>%
      # addMapPane("Population data", zIndex = 0) %>%
      clearControls() %>%
      addPolygons(
        group = "Population data",
        data = summary_poputil$pop_data,
        stroke = TRUE,
        color = councilR::colors$suppGray,
        opacity = 0.6,
        weight = 0.25,
        fillOpacity = 0.6,
        smoothFactor = 0.2,
        fillColor = ~ colorQuantile(
          n=9,
          palette = "Blues",
          domain = summary_poputil$pop_data[[1]]
        )(summary_poputil$pop_data[[1]]),
        popup = case_when(
          selected_popvars$input_pop == "growth_rel_10_40" ~
          paste0(tags$strong(filter(popkey, popvar == selected_popvars$input_pop) %>% select(goodname)), ": ", round(summary_poputil$pop_data[[1]], 2), " x"),
          (selected_popvars$input_pop == "popdens_2040_mi" | selected_popvars$input_pop == "PopDens_2019") ~
          paste0(tags$strong(filter(popkey, popvar == selected_popvars$input_pop) %>% select(goodname)), ": ", format(round(summary_poputil$pop_data[[1]], 1), big.mark = ","), " persons/mile"),
          TRUE ~ paste0(tags$strong(filter(popkey, popvar == selected_popvars$input_pop) %>% select(goodname)), ": ", format(summary_poputil$pop_data[[1]], big.mark = ","), " persons")
        ),
        options = list(zIndex = 0)
      ))
  }
  )
}



## To be copied in the UI
# mod_pop_map_ui("pop_map_ui_1")

## To be copied in the server
# callModule(mod_pop_map_server, "pop_map_ui_1")
