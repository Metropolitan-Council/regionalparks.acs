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
                                   selected_vars,
                                   current_tab,
                                   current_sub_tab) {
  ns <- session$ns
  
  output$buffermap <- mod_leaflet_base_server(input = input,
                                              output = output,
                                              session = session,
                                              add_all_parks = FALSE) #----
  
  # outputOptions(output, "buffermap", suspendWhenHidden = FALSE)
  
  toListen_parktrail <- reactive({
    list(selected_vars$input_agency,
         selected_vars$input_type,
         selected_vars$input_status)
  })
  
  observeEvent(
    toListen_parktrail(),
    {
      # browser()
      
      leafletProxy("buffermap") %>%
        clearGroup("Parks and trails") %>%
        addMapPane("Parks and trails", zIndex = 600) %>%
        # clearControls() %>%
        addPolylines(
          options = pathOptions(pane = "Parks and trails"),
          group = "Parks and trails",
          data = summary_util$map_parktrail_data %>% filter(Type == "Trail"),
          color = ~geog_color,
          weight = 3,
          stroke = T,
          opacity = 1,
          popup = ~popup_text,
          highlightOptions = highlightOptions(
            stroke = TRUE,
            color = "black",
            weight = 6,
            bringToFront = TRUE
          )
        ) %>%
        addPolygons(
          options = pathOptions(pane = "Parks and trails"),
          group = "Parks and trails",
          data = summary_util$map_parktrail_data %>% filter(Type == "Park"),
          color = ~geog_color,
          fillColor = ~geog_color,
          fillOpacity = 1,
          weight = 3,
          stroke = T,
          opacity = 1,
          popup = ~popup_text,
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
                      popup = ~popup_text,
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
  
  observeEvent(current_sub_tab, ignoreInit = TRUE, once = TRUE, { # on startup -----
    # browser()
    
    # if(current_sub_tab != "Weighted averages"){
    # return()
    # }
    pal <- colorNumeric(n = 9, palette = "Blues", domain = summary_util$map_bg_data[[1]])
    
    
    leafletProxy("buffermap") %>%
      ## demographics----
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
                values = summary_util$map_bg_data[[1]]) %>% 
      
      addMapPane("Parks and trails", zIndex = 600) %>%
      # clearControls() %>%
      addPolylines(
        options = pathOptions(pane = "Parks and trails"),
        group = "Parks and trails",
        data = summary_util$map_parktrail_data %>% filter(Type == "Trail"),
        color = ~geog_color,
        weight = 3,
        stroke = T,
        opacity = 1,
        popup = ~popup_text,
        highlightOptions = highlightOptions(
          stroke = TRUE,
          color = "black",
          weight = 6,
          bringToFront = TRUE
        )
      ) %>%
      addPolygons(
        options = pathOptions(pane = "Parks and trails"),
        group = "Parks and trails",
        data = summary_util$map_parktrail_data %>% filter(Type == "Park"),
        color = ~geog_color,
        fillColor = ~geog_color,
        fillOpacity = 1,
        weight = 3,
        stroke = T,
        opacity = 1,
        popup = ~popup_text,
        highlightOptions = highlightOptions(
          stroke = TRUE,
          color = "black",
          weight = 6,
          bringToFront = TRUE
        )
      ) %>% 
      
      ## buffers----
    addMapPane(name = "buff", zIndex = 650) %>% 
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
        popup = ~popup_text,
        popupOptions = popupOptions(
          closeButton = FALSE,
          style = list(
            "font-size" = "18px",
            "font-family" = "Arial"
          )
        )
      )
  })
}

## To be copied in the UI ------
# mod_summary_map_ui("summary_map_ui_1")

## To be copied in the server
# callModule(mod_summary_map_server, "summary_map_ui_1")
