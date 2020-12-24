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
                               selected_popvars,
                               parktrail_util,
                               selected_parktrail) {
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
                           "growth_abs_10_40")
  
  output$popmap <- renderLeaflet({ #pop map --------
      leaflet() %>%
        setView(
          lat = 44.963,
          lng = -93.22,
          zoom = 9
        ) %>%
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
        # addMapPane("Buffers", zIndex = 750) %>%
        
        addPolygons(
          data = agency_boundary,
          group = "Agency boundaries",
          stroke = T,
          color = "black",
          fill = F,
          weight = 2,
          options = pathOptions(pane = "Agency boundaries")
        )  %>%
      
        addLayersControl(
          position = "bottomright",
          overlayGroups = c(
            "Parks and trails",
            "Buffers",
            "Population data",
            "Agency boundaries"
          ),
          baseGroups = c(
            "Carto Positron",
            "Stamen Toner",
            "Esri Imagery"
          ),
          options = layersControlOptions(collapsed = F)
        ) %>%
        leaflet::addScaleBar(position = c("bottomleft"))
    }) #----

  observeEvent( c(selected_popvars$input_pop), {
    leafletProxy("popmap") %>%
      clearGroup("Population data") %>%
      addMapPane("Population data", zIndex = 0) %>%
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
        fillColor = ~ colorNumeric(
          palette = "Blues",
          domain = summary_poputil$pop_data[[1]]
        )(summary_poputil$pop_data[[1]]),
        popup = case_when(selected_popvars$input_pop == "growth_rel_10_40" ~
                            paste0(tags$strong(filter(popkey, popvar == selected_popvars$input_pop) %>% select(goodname)), ": ", round(summary_poputil$pop_data[[1]], 2), " x"),
                          (selected_popvars$input_pop == "popdens_2040_mi" | selected_popvars$input_pop == "PopDens_2019") ~
                            paste0(tags$strong(filter(popkey, popvar == selected_popvars$input_pop) %>% select(goodname)), ": ", format(round(summary_poputil$pop_data[[1]],1), big.mark = ","), " persons/mile"),
                          TRUE ~ paste0(tags$strong(filter(popkey, popvar == selected_popvars$input_pop) %>% select(goodname)), ": ", format(summary_poputil$pop_data[[1]], big.mark = ","), " persons")),
        options = list(zIndex = 0)
      )
  })
  
  observeEvent(
               c(selected_parktrail$input_agency,
                 selected_parktrail$input_type,
                 selected_parktrail$input_status),
               {
                 leafletProxy("popmap") %>%
                   clearGroup("Parks and trails") %>%
                   addMapPane("Parks and trails", zIndex = 700) %>%
                   addPolylines(
                     group = "Parks and trails",
                     data = parktrail_util$parktrail_data %>% filter(Type == "Trail"),
                     color = case_when(
                       parktrail_util$parktrail_data$status2[parktrail_util$parktrail_data$Type == "Trail"] == "Existing" ~ e_col,
                       parktrail_util$parktrail_data$status2[parktrail_util$parktrail_data$Type == "Trail"] == "Planned" ~ p_col,
                       parktrail_util$parktrail_data$status2[parktrail_util$parktrail_data$Type == "Trail"] == "Search" ~ s_col
                     ),
                     weight = 3,
                     stroke = T,
                     opacity = 1,
                     popup = ~ paste0(
                       "<b>",
                       parktrail_util$parktrail_data$status[parktrail_util$parktrail_data$Type == "Trail"],
                       "</b>",
                       "<br>",
                       parktrail_util$parktrail_data$name[parktrail_util$parktrail_data$Type == "Trail"],
                       "<br>",
                       "<em>",
                       parktrail_util$parktrail_data$agency[parktrail_util$parktrail_data$Type == "Trail"],
                       "</em>"
                     ),
                     highlightOptions = highlightOptions(
                       stroke = TRUE,
                       color = "black",
                       weight = 6,
                       bringToFront = TRUE
                     ),
                     options = list(zIndex = 700)
                   ) %>%
                   
                   addPolygons(
                     group = "Parks and trails",
                     data = parktrail_util$parktrail_data %>% filter(Type == "Park"),
                     color = case_when(
                       parktrail_util$parktrail_data$status2[parktrail_util$parktrail_data$Type == "Park"] == "Existing" ~ e_col,
                       parktrail_util$parktrail_data$status2[parktrail_util$parktrail_data$Type == "Park"] == "Planned" ~ p_col,
                       parktrail_util$parktrail_data$status2[parktrail_util$parktrail_data$Type == "Park"] == "Search" ~ s_col
                     ),
                     fillColor = case_when(
                       parktrail_util$parktrail_data$status2[parktrail_util$parktrail_data$Type == "Park"] == "Existing" ~ e_col,
                       parktrail_util$parktrail_data$status2[parktrail_util$parktrail_data$Type == "Park"] == "Planned" ~ p_col,
                       parktrail_util$parktrail_data$status2[parktrail_util$parktrail_data$Type == "Park"] == "Search" ~ s_col
                     ),
                     fillOpacity = 1,
                     weight = 3,
                     stroke = T,
                     opacity = 1,
                     popup = ~ paste0(
                       "<b>",
                       parktrail_util$parktrail_data$status[parktrail_util$parktrail_data$Type == "Park"],
                       "</b>",
                       "<br>",
                       parktrail_util$parktrail_data$name[parktrail_util$parktrail_data$Type == "Park"],
                       "<br>",
                       "<em>",
                       parktrail_util$parktrail_data$agency[parktrail_util$parktrail_data$Type == "Park"],
                       "</em>"
                     ),
                     highlightOptions = highlightOptions(
                       stroke = TRUE,
                       color = "black",
                       weight = 6,
                       bringToFront = TRUE
      )#,
      # options = list(zIndex = 710)
    ) 
               })

# 
#   observeEvent( #add buffers -------
#                 c(selected_popvars$input_distance, selected_popvars$input_agency, selected_popvars$input_type, selected_popvars$input_status), {
#                   leafletProxy("popmap") %>%
#                     clearGroup("Buffers") %>%
#                     # clearControls()
#                     addPolygons(
#                       data = parktrail_util$buffer_data,
#                       group = "Buffers",
#                       stroke = TRUE,
#                       weight = 2,
#                       color = "#616161",
#                       fill = T,
#                       fillColor = "transparent",
#                       opacity = .4,
#                       fillOpacity = .005,
#                       highlightOptions = highlightOptions(
#                         stroke = TRUE,
#                         color = "black",
#                         weight = 6,
#                         bringToFront = TRUE,
#                         sendToBack = TRUE,
#                         opacity = 1
#                       ),
#                       popup = ~ paste0(
#                         "<b>",
#                         "Buffer: ",
#                         parktrail_util$buffer_data$status,
#                         ", ",
#                         parktrail_util$buffer_data$type,
#                         "</b>",
#                         "<br>",
#                         parktrail_util$buffer_data$name,
#                         "<br>",
#                         "<em>",
#                         parktrail_util$buffer_data$agency,
#                         "</em>"
#                       ),
#                       popupOptions = popupOptions(
#                         closeButton = FALSE,
#                         style = list(
#                           "font-size" = "18px",
#                           "font-family" = "Arial"
#                         )
#                       ),
#                       options = list(zIndex = 750),
#                     )
#                 })
  
}
    
    

## To be copied in the UI
# mod_pop_map_ui("pop_map_ui_1")

## To be copied in the server
# callModule(mod_pop_map_server, "pop_map_ui_1")
