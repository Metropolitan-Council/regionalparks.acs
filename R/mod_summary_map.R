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
    
    HTML('<p>This map visualizes the geospatial location of the buffers around the user-selected parks and trails. Demographic data can also be shown here.  THIS MAP IS NOT REACTIVE AT THE MOMENT (but will be updated)'),
    
    leafletOutput(ns("buffermap"), height = 700)
    
  )
}

#' summary_map Server Function
#'
#' @noRd
mod_summary_map_server <- function(input, output, session,
                                   summary_util) {
  ns <- session$ns
  
  
  output$buffermap <- renderLeaflet({
    leaflet() %>%
      setView(
        lat = 44.963,
        lng = -93.22,
        zoom = 9
      ) %>%
      addProviderTiles("CartoDB.Positron",
                       group = "Carto Positron"
      ) %>%
      addPolygons(
        data = agency_boundary,
        group = "Agency boundaries",
        stroke = T,
        color = "black",
        fill = F,
        weight = 3
      ) %>%
      addLayersControl(
        position = "bottomright",
        baseGroups = c(
          "Carto Positron"
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
  })
  
  
  observe({
    if (nrow(summary_util$map_parktrail_data) > 0) {
      leafletProxy("buffermap") %>%
    addTiles() %>%
    clearShapes() %>%
    #---- existing
    addPolylines(
      data = summary_util$map_parktrail_data %>% filter(Type == "Trail"),
      color = case_when(summary_util$map_parktrail_data$status2[summary_util$map_parktrail_data$Type == "Trail"] == "Existing" ~ "#2ec799", 
                        summary_util$map_parktrail_data$status2[summary_util$map_parktrail_data$Type == "Trail"] == "Planned" ~ "#f77614",
                        summary_util$map_parktrail_data$status2[summary_util$map_parktrail_data$Type == "Trail"] == "Search" ~ "#9591c9"),
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
          data = summary_util$map_parktrail_data %>% filter(Type == "Park"),
          color = case_when(summary_util$map_parktrail_data$status2[summary_util$map_parktrail_data$Type == "Park"] == "Existing" ~ "#2ec799", 
                            summary_util$map_parktrail_data$status2[summary_util$map_parktrail_data$Type == "Park"] == "Planned" ~ "#f77614",
                            summary_util$map_parktrail_data$status2[summary_util$map_parktrail_data$Type == "Park"] == "Search" ~ "#9591c9"),
          fillColor = case_when(summary_util$map_parktrail_data$status2[summary_util$map_parktrail_data$Type == "Park"] == "Existing" ~ "#2ec799", 
                            summary_util$map_parktrail_data$status2[summary_util$map_parktrail_data$Type == "Park"] == "Planned" ~ "#f77614",
                            summary_util$map_parktrail_data$status2[summary_util$map_parktrail_data$Type == "Park"] == "Search" ~ "#9591c9"),
          fillOpacity = 1,
          weight = 3,
          stroke = T,
          opacity = 1,
          popup = ~ paste0(
            "<b>", summary_util$map_parktrail_data$status[summary_util$map_parktrail_data$Type == "Park"], "</b>", "<br>",
            summary_util$map_parktrail_data$name[summary_util$map_parktrail_data$Type == "Park"], "<br>",
            "<em>", summary_util$map_parktrail_data$agency[summary_util$map_parktrail_data$Type == "Park"], "</em>"),
          highlightOptions = highlightOptions(
            stroke = TRUE,
            color = "black",
            weight = 6,
            bringToFront = TRUE
          )
        ) %>%
        addPolygons(
          data = summary_util$map_buffer_data,
          group = "Buffers",
          stroke = TRUE,
          weight = 1,
          color = "#616161",
          fill = T,
          fillColor = "transparent",
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
            style = list("font-size" = "18px",
                         "font-family" = "Arial")
          )
        ) %>%
        addPolygons(
          data = summary_util$map_bg_data,
          group = "Census block groups",
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
        )
    }
  }
  )
      
  
  
}

## To be copied in the UI
# mod_summary_map_ui("summary_map_ui_1")

## To be copied in the server
# callModule(mod_summary_map_server, "summary_map_ui_1")
