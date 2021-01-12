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
 
    leafletOutput(ns("map"), width = "100%", height = 700)
    
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
     ) %>%
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
     ) %>%
     addMapPane("trans", zIndex = 430) %>%
     addCircles( # Markers(
       data = regionalparks.acs::trans_stops,
       group = "Active transit stops",
       radius = 20,
       fill = T,
       stroke = TRUE,
       weight = 2,
       color = councilR::colors$transitRed,
       fillColor = councilR::colors$transitRed,
       options = pathOptions(pane = "trans")
     ) %>%
     groupOptions(
       group = "Active transit stops",
       zoomLevels = 13:20
     ) %>%
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
     ) %>%
     hideGroup(
       c(
         "Active transit stops",
         "Water Access",
         "Park Entrance",
         "Rivers & Lakes"
       )
     ) %>%
     addLayersControl(
       position = "bottomright",
       overlayGroups = c(
         "Parks and trails",
         "Buffers",
         "Population data",
         "Active transit stops",
         "Water Access",
         "Park Entrance",
         "Rivers & Lakes",
         "Agency boundaries"
       ),
       baseGroups = c(
         "Carto Positron",
         "Aerial photography"
       ),
       options = layersControlOptions(collapsed = T)
     ) %>%
     leaflet::addScaleBar(position = c("bottomleft"))
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
     clearGroup("Population data") %>%
     clearControls() %>%
     addPolygons(
       group = "Population data",
       data = main_lft_inputs$map_bg_data_main,
       stroke = TRUE,
       color = councilR::colors$suppGray,
       opacity = 0.6,
       weight = 0.25,
       fillOpacity = 0.6,
       smoothFactor = 0.2,
      fillColor = ~ colorNumeric(
         # n = 7,
         palette = "Blues",
         domain = main_lft_inputs$map_bg_data_main[[1]]
       )(main_lft_inputs$map_bg_data_main[[1]]),
       # fillColor = ~ main_lft_inputs$pop_pal(main_lft_inputs$map_bg_data_main[[1]]),
       options = list(zIndex = 0)
     )
 })
  
  
}
    
## To be copied in the UI
# mod_main_leaflet_ui("main_leaflet_ui_1")
    
## To be copied in the server
# callModule(mod_main_leaflet_server, "main_leaflet_ui_1")
 
