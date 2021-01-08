#' leaflet UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList

mod_leaflet_ui <- function(id){
  ns <- NS(id)
  tagList(
    # verbatimTextOutput(ns("test")),
    leafletOutput(ns("overviewmap"), height = 700)
  )
}
    
#' leaflet Server Function
#'
#' @noRd 
mod_leaflet_server <- function(input, output, session,
                               util_leaflet, selected_map_vars,
                               current_tab){
  ns <- session$ns
 # browser()
  # output$test<-renderPrint(selected_map_vars$input_acsmap)
  
  
  output$overviewmap <- mod_leaflet_base_server(input = input,
                                                output = output,
                                                session = session) #----
  
  
  

  observeEvent(selected_map_vars$input_acsmap, {

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
        fillColor = ~util_leaflet$leaflet_pal(util_leaflet$leaflet_data[[1]]),

        popup = if (selected_map_vars$input_acsmap == "adj_meanhhi") {
          ~ paste0(tags$strong(filter(renamekey, ACS == selected_map_vars$input_acsmap) %>% select(goodname)), ": $", format(util_leaflet$leaflet_data[[1]], big.mark = ","))
        } else {
          ~ paste0(
            tags$strong(filter(renamekey, ACS == selected_map_vars$input_acsmap) %>% select(goodname)),
            ": ",
            util_leaflet$leaflet_data[[1]], "%"
          )
        }#,
        # options = list(zIndex = 0)
      ) %>%
      addLegend(title = paste0(filter(renamekey, ACS == selected_map_vars$input_acsmap) %>% select(goodname)),
                position = "bottomleft",
                group = "Demographic data",
                layerId = "Demographic data",
                pal = util_leaflet$leaflet_pal,
                values = util_leaflet$leaflet_data[[1]])
  })

 
  
  observeEvent(current_tab,  ignoreInit = TRUE, once = TRUE, {
    # browser()
    leafletProxy("overviewmap") %>%
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
        fillColor = ~util_leaflet$leaflet_pal(util_leaflet$leaflet_data[[1]]),
        
        popup = if (selected_map_vars$input_acsmap == "adj_meanhhi") {
          ~ paste0(tags$strong(filter(renamekey, ACS == selected_map_vars$input_acsmap) %>% select(goodname)), ": $", format(util_leaflet$leaflet_data[[1]], big.mark = ","))
        } else {
          ~ paste0(
            tags$strong(filter(renamekey, ACS == selected_map_vars$input_acsmap) %>% select(goodname)),
            ": ",
            util_leaflet$leaflet_data[[1]], "%"
          )
        }#,
        # options = list(zIndex = 0)
      ) %>%
      addLegend(title = paste0(filter(renamekey, ACS == selected_map_vars$input_acsmap) %>% select(goodname)),
                position = "bottomleft",
                group = "Demographic data",
                layerId = "Demographic data",
                pal = util_leaflet$leaflet_pal,
                values = util_leaflet$leaflet_data[[1]])
  })
  
}
    
## To be copied in the UI
# mod_leaflet_ui("leaflet_ui_1")
    
## To be copied in the server
# callModule(mod_leaflet_server, "leaflet_ui_1")
 
