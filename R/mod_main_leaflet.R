#' main_leaflet UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_main_leaflet_ui <- function(id){
  ns <- NS(id)
  tagList(
 
    leafletOutput(ns("map"), width = "100%", height = 400)
    
  )
}
    
#' main_leaflet Server Function
#'
#' @noRd 
mod_main_leaflet_server <- function(input, output, session,
                                    main_lft_inputs){
  ns <- session$ns
 
 output$map <- renderLeaflet({
    leaflet() %>%
    setView(
      lat = 44.963,
      lng = -93.22,
      zoom = 9
    ) %>%
    addMapPane(name = "Carto Positron", zIndex = 430) %>%
    addProviderTiles("CartoDB.PositronOnlyLabels",
                     options = leafletOptions(pane = "Carto Positron"),
                     group = "Carto Positron"
    ) %>%
    addProviderTiles("CartoDB.PositronNoLabels",
                     group = "Carto Positron"
    ) %>%
    addProviderTiles(
      provider = providers$Esri.WorldImagery,
      group = "Aerial photography"
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
    ) 
 })
 
 
 toListen_mainleaflet <- reactive({
   list(
     main_lft_inputs$map_bg_data_main,
     main_lft_inputs$source,
     main_lft_inputs$mainacs,
     main_lft_inputs$mainpop
   )
 })
 
 
 
 observeEvent(
   toListen_mainleaflet(), {
   print("Rendering main leaflet map")

   leafletProxy("map") %>%
     # clearGroup("Population data") %>%
     # clearControls() %>%
     addPolygons(
       group = "Population data",
       data = main_lft_inputs$map_bg_data_main,
       stroke = TRUE,
       color = councilR::colors$suppGray,
       opacity = 0.6,
       weight = 0.25,
       fillOpacity = 0.6,
       smoothFactor = 0.2,
       # fillColor = ~ main_lft_inputs$pop_pal(main_lft_inputs$map_bg_data_main[[1]]),
       options = list(zIndex = 0)
     )
 })
  
  
}
    
## To be copied in the UI
# mod_main_leaflet_ui("main_leaflet_ui_1")
    
## To be copied in the server
# callModule(mod_main_leaflet_server, "main_leaflet_ui_1")
 
