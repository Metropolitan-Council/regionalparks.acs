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
    leafletOutput(ns("overviewmap"), width = "100%", height = 700)
  )
}

#' leaflet Server Function
#'
#' @noRd
#' @import leaflet
#' @import councilR
#' @import leaflet.extras
#' @import tibble
#' @import ggplot2
#' @import cowplot
#' @import plotly
#' @import tidyr
#' @import stringr
#' @import forcats
#' @import dplyr


mod_leaflet_server <- function(input, output, session, 
                               util_leaflet, selected_map_vars) {#} tract_data = tract_data) {
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
  
  output$overviewmap <- renderLeaflet({ # overviewmap map --------
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
      addMapPane("trans", zIndex = 430) %>%
      
      addCircles(#Markers(
        data = regionalparks.acs::trans_stops,
        group = "Transit",
        radius = 20,
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
      )  %>%
      addLayersControl(
        position = "bottomright",
        overlayGroups = c(
          "Parks and trails",
          "Buffers",
          "Demographic data",
          "Agency boundaries",
          "Transit"
        ),
        baseGroups = c(
          "Stamen Toner",
          "Carto Positron",
          "Esri Imagery"
        ),
        options = layersControlOptions(collapsed = F)
      ) %>%
      leaflet::addScaleBar(position = c("bottomleft"))
  }) #----
  
  outputOptions(output, "overviewmap", suspendWhenHidden = FALSE)
  
  observeEvent(c(selected_map_vars$input_acs), {
    pal <- (colorNumeric(n = 9, palette = "Blues", domain = util_leaflet$leaflet_data[[1]])) 
    
    leafletProxy("overviewmap") %>%
      clearGroup("Demographic data") %>%
      addMapPane("Demographic data", zIndex = 0) %>%
      addPolygons(
        group = "Demographic data",
        data = util_leaflet$leaflet_data,
        stroke = TRUE,
        color = councilR::colors$suppGray,
        opacity = 0.6,
        weight = 0.25,
        fillOpacity = 0.6,
        smoothFactor = 0.2,
        fillColor = ~ colorNumeric(
          # n = 7,
          palette = "Blues",
          domain = util_leaflet$leaflet_data[[1]]
        )(util_leaflet$leaflet_data[[1]]),
        
        popup = if (selected_map_vars$input_acs == "adj_meanhhi") {
          ~ paste0(tags$strong(filter(renamekey, ACS == selected_map_vars$input_acs) %>% select(goodname)), ": $", format(util_leaflet$leaflet_data[[1]], big.mark = ","))
        } else {
          ~ paste0(
            tags$strong(filter(renamekey, ACS == selected_map_vars$input_acs) %>% select(goodname)),
            ": ",
            util_leaflet$leaflet_data[[1]], "%"
          )
        }#,
        # options = list(zIndex = 0)
      ) %>%
      addLegend(title = paste0(filter(renamekey, ACS == selected_map_vars$input_acs) %>% select(goodname)),
                position = "bottomleft",
                group = "Demographic data",
                layerId = "Demographic data",
                pal = pal,
                values = util_leaflet$leaflet_data[[1]])
  })
  

}
  

#
## To be copied in the UI
# mod_leaflet_ui("leaflet_ui_1")

## To be copied in the server
# callModule(mod_leaflet_server, "leaflet_ui_1")
