#' summary_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_summary_map_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::p("This map visualizes the geospatial location of the buffers around the user-selected parks and trails along with the selected demographic data. For the demographic data, darker colors mean higher values and lighter colors mean lower values. Demographic data can be turned off using the layer controls found at the bottom right of the map."),
    
    leafletOutput(ns("buffermap"), height = 700)
  )
}




#' summary_map Server Function
#'
#' @noRd
mod_summary_map_server <- function(input, output, session,
                                   summary_util,
                                   selected_vars) {
  ns <- session$ns
  
  output$buffermap <- renderLeaflet({ # buf map --------
    leaflet() %>%
      setView(
        lat = 44.963,
        lng = -93.22,
        zoom = 9
      ) %>%
      addMapPane(name = "Stamen Toner", zIndex = 430) %>%
      addProviderTiles("Stamen.TonerLines",
                       group = "Stamen Toner"
      ) %>%
      addProviderTiles("Stamen.TonerLabels", 
                       options = leafletOptions(pane = "Stamen Toner"),
                       group = "Stamen Toner") %>%
      
      addMapPane(name = "Carto Positron", zIndex = 430) %>%
      addProviderTiles("CartoDB.PositronOnlyLabels", 
                       options = leafletOptions(pane = "Carto Positron"),
                       group = "Carto Positron") %>%
      addProviderTiles("CartoDB.PositronNoLabels",
                       group = "Carto Positron"
      ) %>%
      
      addProviderTiles(
        provider = providers$Esri.WorldImagery,
        group = "Esri Imagery"
      ) %>%
      addMapPane("Agency boundaries", zIndex = 650) %>%
      addMapPane("buff", zIndex = 660) %>%
      addMapPane("parktrail", zIndex = 670) %>%
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
      )   %>%
      
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
      )   %>%
      
      
      addMapPane("trans", zIndex = 430) %>%
      addCircles(#Markers(
        data = regionalparks.acs::trans_stops,
        group = "Transit",
        radius = 20,
        fill = T,
        stroke = TRUE,
        weight = 2, 
        color = councilR::colors$transitRed,
        fillColor = councilR::colors$transitRed,
        options = pathOptions(pane = "trans")
      ) %>%
      groupOptions(
        group = "Transit",
        zoomLevels = 13:20
      )  %>%
      
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
      )   %>%
      
          addLayersControl(
        position = "bottomright",
        overlayGroups = c(
          "Parks and trails",
          "Buffers",
          "Demographic data",
          "Transit",
          "Water Access",
          "Park Entrance",
          "Rivers & Lakes",
          "Agency boundaries"
        ),
        hideGroup = c("Rivers & Lakes"),
        
        baseGroups = c(
          "Stamen Toner",
          "Carto Positron",
          "Esri Imagery"
        ),
        options = layersControlOptions(collapsed = F)
      ) %>%
      
      #   htmlwidgets::onRender(
      #     "
      #     function() {
      #         $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:center\">Map layers</label>');
      #     }
      # "
      #   ) %>%
      leaflet::addScaleBar(position = c("bottomleft"))
  }) #----
  
  # outputOptions(output, "buffermap", suspendWhenHidden = FALSE)

  
    toListen_parktrail <- reactive({
    list(selected_vars$input_agency,
         selected_vars$input_type,
         selected_vars$input_status)
  })
  
  observeEvent(
    toListen_parktrail(),
    {
      leafletProxy("buffermap") %>%
        clearGroup("Parks and trails") %>%
        # addMapPane("Parks and trails", zIndex = 700) %>%
        # clearControls() %>%
        addPolylines(
          options = pathOptions(pane = "parktrail"),
          group = "Parks and trails",
          data = summary_util$map_parktrail_data,
          color = case_when(
            summary_util$map_parktrail_data$status2[summary_util$map_parktrail_data$Type == "Trail"] == "Existing" ~ e_col,
            summary_util$map_parktrail_data$status2[summary_util$map_parktrail_data$Type == "Trail"] == "Planned" ~ p_col,
            summary_util$map_parktrail_data$status2[summary_util$map_parktrail_data$Type == "Trail"] == "Search" ~ s_col
          ),
          weight = 3,
          stroke = T,
          opacity = 1,
          popup = ~ paste0(
            "<b>", summary_util$map_parktrail_data$status[summary_util$map_parktrail_data$Type == "Trail"], "</b>", "<br>",
            summary_util$map_parktrail_data$name[summary_util$map_parktrail_data$Type == "Trail"], "<br>",
            "<em>",
            summary_util$map_parktrail_data$agency[summary_util$map_parktrail_data$Type == "Trail"], "</em>"
          ),
          highlightOptions = highlightOptions(
            stroke = TRUE,
            color = "black",
            weight = 6,
            bringToFront = TRUE
          )
        ) %>%
        addPolygons(
          options = pathOptions(pane = "parktrail"),
          group = "Parks and trails",
          data = summary_util$map_parktrail_data %>% filter(agency %in% selected_vars$input_agency, Type == "Park"),
          color = case_when(
            summary_util$map_parktrail_data$status2[summary_util$map_parktrail_data$Type == "Park"] == "Existing" ~ e_col,
            summary_util$map_parktrail_data$status2[summary_util$map_parktrail_data$Type == "Park"] == "Planned" ~ p_col,
            summary_util$map_parktrail_data$status2[summary_util$map_parktrail_data$Type == "Park"] == "Search" ~ s_col
          ),
          fillColor = case_when(
            summary_util$map_parktrail_data$status2[summary_util$map_parktrail_data$Type == "Park"] == "Existing" ~ e_col,
            summary_util$map_parktrail_data$status2[summary_util$map_parktrail_data$Type == "Park"] == "Planned" ~ p_col,
            summary_util$map_parktrail_data$status2[summary_util$map_parktrail_data$Type == "Park"] == "Search" ~ s_col
          ),
          fillOpacity = 1,
          weight = 3,
          stroke = T,
          opacity = 1,
          popup = ~ paste0(
            "<b>", summary_util$map_parktrail_data$status[summary_util$map_parktrail_data$Type == "Park"], "</b>", "<br>",
            summary_util$map_parktrail_data$name[summary_util$map_parktrail_data$Type == "Park"], "<br>",
            "<em>", summary_util$map_parktrail_data$agency[summary_util$map_parktrail_data$Type == "Park"], "</em>"
          ),
          highlightOptions = highlightOptions(
            stroke = TRUE,
            color = "black",
            weight = 6,
            bringToFront = TRUE
          )
        )
    }
  )
  
  observeEvent(c(selected_vars$input_acs), {
    pal <- (colorNumeric(n = 9, palette = "Blues", domain = summary_util$map_bg_data[[1]])) 
    
    leafletProxy("buffermap") %>%
      clearGroup("Demographic data") %>%
      addMapPane("Demographic data", zIndex = 0) %>%
      addPolygons(
        group = "Demographic data",
        data = summary_util$map_bg_data,
        stroke = TRUE,
        color = councilR::colors$suppGray,
        opacity = 0.6,
        weight = 0.25,
        fillOpacity = 0.6,
        smoothFactor = 0.2,
        fillColor = ~ colorNumeric(
          # n = 7,
          palette = "Blues",
          domain = summary_util$map_bg_data[[1]]
        )(summary_util$map_bg_data[[1]]),
        
        popup = if (selected_vars$input_acs == "adj_meanhhi") {
          ~ paste0(tags$strong(filter(renamekey, ACS == selected_vars$input_acs) %>% select(goodname)), ": $", format(summary_util$map_bg_data[[1]], big.mark = ","))
        } else {
          ~ paste0(
            tags$strong(filter(renamekey, ACS == selected_vars$input_acs) %>% select(goodname)),
            ": ",
            summary_util$map_bg_data[[1]], "%"
          )
        }#,
        # options = list(zIndex = 0)
      ) %>%
      addLegend(title = paste0(filter(renamekey, ACS == selected_vars$input_acs) %>% select(goodname)),
                position = "bottomleft",
                group = "Demographic data",
                layerId = "Demographic data",
                pal = pal,
                values = summary_util$map_bg_data[[1]])
  })
  
  toListen_buffer <- reactive({
    list(selected_vars$input_distance, selected_vars$input_agency, selected_vars$input_type, selected_vars$input_status)
  })
  
  observeEvent( # add buffers -------
                toListen_buffer(),
                {
                  leafletProxy("buffermap") %>%
                    clearGroup("Buffers") %>%
                    # clearControls()
                    addPolygons(
                      options = pathOptions(pane = "buff"),

                      data = summary_util$map_buffer_data,
                      group = "Buffers",
                      stroke = TRUE,
                      weight = 2,
                      color = "black",#"#616161",
                      fill = FALSE,
                      # fillColor = "transparent",
                      opacity = .4,
                      fillOpacity = .005,
                      highlightOptions = highlightOptions(
                        stroke = TRUE,
                        color = "black",
                        weight = 6,
                        bringToFront = TRUE,
                        sendToBack = TRUE,
                        opacity = 1
                      ),
                      popup = ~ paste0(
                        "<b>",
                        "Buffer: ",
                        summary_util$map_buffer_data$status,
                        ", ",
                        summary_util$map_buffer_data$type,
                        "</b>",
                        "<br>",
                        summary_util$map_buffer_data$name,
                        "<br>",
                        "<em>",
                        summary_util$map_buffer_data$agency,
                        "</em>"
                      ),
                      popupOptions = popupOptions(
                        closeButton = FALSE,
                        style = list(
                          "font-size" = "18px",
                          "font-family" = "Arial"
                        )
                      )
                    )
                }
  )
}

## To be copied in the UI ------
# mod_summary_map_ui("summary_map_ui_1")

## To be copied in the server
# callModule(mod_summary_map_server, "summary_map_ui_1")
