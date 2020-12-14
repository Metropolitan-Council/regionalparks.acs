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
    HTML("<p>This map visualizes the geospatial location of the buffers around the user-selected parks and trails along with the selected demographic data. For the demographic data, darker colors mean higher values and lighter colors mean lower values. Demographic data can be turned off using the layer controls found at the bottom right of the map."),

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

  renamekey <- tribble(
    ~goodname,
    ~"ACS",
    "Total population",
    "adj_poptotal",
    "Age, % under 15",
    "adj_ageunder15_per",
    "Age, % 15-24",
    "adj_age15_24_per",
    "Age, % 25-64",
    "adj_age25_64_per",
    "Age, % 65 and up",
    "adj_age65up_per",
    "Race, % White",
    "adj_whitenh_per",
    "Race, % Black",
    "adj_blacknh_per",
    "Race, % Asian",
    "adj_asiannh_per",
    "Race, % American Indian",
    "adj_amindnh_per",
    "Race, % Other + Multiracial",
    "adj_othermultinh_per",
    "Ethnicity, % Hispanic",
    "adj_hisppop_per",
    "Ethnicity, % not-Hispanic",
    "adj_nothisppop_per",
    "Mean household income",
    "adj_meanhhi",
    "% Housholds without a vehicle",
    "adj_novehicle_per",
    "% speaking English less than very well",
    "adj_lep_per",
    "% Spanish speakers",
    "adj_span_per"
  )

  output$buffermap <- renderLeaflet({
    leaflet() %>%
      setView(
        lat = 44.963,
        lng = -93.22,
        zoom = 9
      ) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      # addPolygons(
      #   data = agency_boundary,
      #   group = "Agency boundaries",
      #   stroke = T,
      #   color = "black",
      #   fill = F,
      #   weight = 3
      # ) %>%
      # addLayersControl(
      #   position = "bottomright",
      #   baseGroups = c(
      #   ),
      #   options = layersControlOptions(collapsed = F)
      # ) %>%
      #   htmlwidgets::onRender(
      #     "
      #     function() {
      #         $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:center\">Map layers</label>');
      #     }
      # "
      #   ) %>%
      leaflet::addScaleBar(position = c("bottomleft"))
  })


  # observeEvent(summary_util$map_bg_data, {
  #   # pal <- if (names(summary_util$map_bg_data["adj_meanhhi"])[[1]] == "adj_meanhhi") { #names(bg_geo["adj_meanhhi"])[[1]]
  #   #   colorQuantile(
  #   #     palette = "viridis",
  #   #     n = 5,
  #   #     # reverse = TRUE,
  #   #     domain = summary_util$map_bg_data[[1]]
  #   #   )
  #   # } else {
  #     pal <- colorNumeric(
  #       palette = "viridis",
  #       domain = summary_util$map_bg_data[[1]]
  #     )
  #   # }
  # }
  # )
  # palette_OkabeIto <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
  # palette_OkabeIto_black <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")

  observe({
    if (nrow(summary_util$map_parktrail_data) > 0) {
      leafletProxy("buffermap") %>%
        addTiles() %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(
          group = "Demographic data",
          data = summary_util$map_bg_data,
          stroke = TRUE,
          color = councilR::colors$suppGray,
          opacity = 0.6,
          weight = 0.25,
          fillOpacity = 0.6,
          smoothFactor = 0.2,
          fillColor = ~ colorQuantile(
            n = 7,
            palette = "Blues",
            domain = summary_util$map_bg_data[[1]]
          )(summary_util$map_bg_data[[1]])
        ) %>%
        addLegend("topright",
          pal = colorQuantile(
            n = 7,
            palette = "Blues",
            domain = summary_util$map_bg_data[[1]]
          ),
          values = (summary_util$map_bg_data[[1]]),
          title = paste0(filter(renamekey, ACS == (names(summary_util$map_bg_data)[[1]])) %>% select(goodname)), # (names(summary_util$map_bg_data)[[1]]),
          opacity = 1,
          group = "Demographic data"
        ) %>%
        # popup = paste0(summary_util$map_bg_data, #how can we get it to show just the value for the blockgroup that is clicked?
        #   " ",
        #   summary_util$map_bg_data[[1]], "%"
        # )) %>%
        addPolygons(
          data = agency_boundary,
          group = "Agency boundaries",
          stroke = T,
          color = "black",
          fill = F,
          weight = 2
        ) %>%
        addPolylines(
          data = summary_util$map_parktrail_data %>% filter(Type == "Trail"),
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
          data = summary_util$map_parktrail_data %>% filter(Type == "Park"),
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
        ) %>%
        addPolygons(
          data = summary_util$map_buffer_data,
          group = "Buffers",
          stroke = TRUE,
          weight = 2,
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
            style = list(
              "font-size" = "18px",
              "font-family" = "Arial"
            )
          )
        ) %>%
        addLayersControl(
          position = "bottomright",
          overlayGroups = c(
            "Demographic data",
            "Agency boundaries"
          ),
          # baseGroups = c(
          #   "Carto Positron",
          #   # "Carto DarkMatter",
          #   "Esri Imagery"
          # ),
          options = layersControlOptions(collapsed = F)
        )
    }
  })
}

## To be copied in the UI
# mod_summary_map_ui("summary_map_ui_1")

## To be copied in the server
# callModule(mod_summary_map_server, "summary_map_ui_1")
