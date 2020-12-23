#' summary_map2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_summary_map2_ui <- function(id){
  ns <- NS(id)
  tagList(
    HTML("<p>This map visualizes the geospatial location of the buffers around the user-selected parks and trails along with the selected demographic data. For the demographic data, darker colors mean higher values and lighter colors mean lower values. Demographic data can be turned off using the layer controls found at the bottom right of the map."),
    
    leafletOutput(ns("buffermap"), height = 700)
  )
  
}
    
#' summary_map2 Server Function
#'
#' @noRd 
mod_summary_map2_server <- function(input, output, session,
                                    summary_util,
                                    selected_vars) {
  ns <- session$ns
  
  renamekey <- tibble::tribble( #------
                                ~goodname,
                                ~"ACS",
                                "Total population",
                                "adj_2019pop",
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
                                "adj_span_per",
                                "Ability, % any disability",
                                "adj_anydis_per",
                                "Origin, % US-born",
                                "adj_usborn_per",
                                "Origin, % foreign-born",
                                "adj_forborn_per"
  )
  #buf map --------
  output$buffermap <- renderLeaflet({ 
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
    
      addLayersControl(
        position = "bottomright",
        overlayGroups = c(
          "Parks and Trails",
          "Buffers",
          "Demographic data",
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
  
  #----
  observe({ 
    # req(nrow(summary_util$map_bg_data)>0)

    leafletProxy("buffermap", data = summary_util$map_bg_data) %>%
      clearGroup("Demographic data") %>%
      # clearControls() %>%
      addPolygons(
        group = "Demographic data",
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
        )(summary_util$map_bg_data[[1]])
      )
    event <- input$buffermap_map_shape_click
  })
  
}

    
## To be copied in the UI
# mod_summary_map2_ui("summary_map2_ui_1")
    
## To be copied in the server
# callModule(mod_summary_map2_server, "summary_map2_ui_1")
 
