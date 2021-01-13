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
                                    main_lft_inputs,
                                    current_tab){
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
       c("Buffers",
         "Agency boundaries",
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
 
 # adding proxys --------
 
 # add pop polygons -----
 toListen_mainleaflet <- reactive({
   list(current_tab,
     main_lft_inputs$map_bg_data_main,
     main_lft_inputs$source,
     main_lft_inputs$mainacs,
     main_lft_inputs$mainpop
   )
 })
 
 observeEvent(
   toListen_mainleaflet(), {
   print("Rendering main leaflet map - pop polygons")
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
      fillColor = ~ main_lft_inputs$pop_pal(main_lft_inputs$map_bg_data_main[[1]]),
       options = list(zIndex = 0),
      
      popup = if (main_lft_inputs$source == "Population characteristics") {
        if (main_lft_inputs$mainacs == "adj_meanhhi") {
        ~ paste0(tags$strong(filter(renamekey, ACS == main_lft_inputs$mainacs) %>% select(goodname)), ": $", format(main_lft_inputs$map_bg_data_main[[1]], big.mark = ","))
      } else {
        ~ paste0(
          tags$strong(filter(renamekey, ACS == main_lft_inputs$mainacs) %>% select(goodname)),
          ": ",
          main_lft_inputs$map_bg_data_main[[1]], "%"
        )
      }} else { case_when(
        main_lft_inputs$mainpop == "growth_rel_10_40" ~
          paste0(tags$strong(filter(popkey, popvar == main_lft_inputs$mainpop) %>% select(goodname)), ": ", round(main_lft_inputs$map_bg_data_main[[1]], 2), " x"),
        (main_lft_inputs$mainpop == "popdens_2040_mi" | main_lft_inputs$mainpop == "PopDens_2019") ~
          paste0(tags$strong(filter(popkey, popvar == main_lft_inputs$mainpop) %>% select(goodname)), ": ", format(round(main_lft_inputs$map_bg_data_main[[1]], 1), big.mark = ","), " persons/mile"),
        TRUE ~ paste0(tags$strong(filter(popkey, popvar == main_lft_inputs$mainpop) %>% select(goodname)), ": ", format(main_lft_inputs$map_bg_data_main[[1]], big.mark = ","), " persons")
      ) }
     ) %>%
     
     addLegend(
       title = if (main_lft_inputs$source == "Population characteristics") {
         paste0(filter(renamekey, ACS == main_lft_inputs$mainacs) %>% select(goodname))} else {
           paste0(filter(popkey, popvar == main_lft_inputs$mainpop) %>% select(short))
         },
       position = "bottomleft",
       group = "Population data",
       layerId = "Population data",
       pal = main_lft_inputs$pop_pal,
       values = main_lft_inputs$map_bg_data_main[[1]]
     )
 })
 
 # add parktrails
 toListen_mainleaflet_parktrail <- reactive({
   list(current_tab,
     main_lft_inputs$map_parktrail_data_main
   )
 }) 

  observeEvent(
    toListen_mainleaflet_parktrail(), {
     print("Rendering main leaflet map - parks/trails")

      leafletProxy("map") %>%
       clearGroup("Parks and trails") %>%
       clearControls() %>%
       
       addMapPane("parks_geo", zIndex = 420) %>%
       addPolygons(
         data = main_lft_inputs$map_parktrail_data_main[main_lft_inputs$map_parktrail_data_main$Type == "Park" & main_lft_inputs$map_parktrail_data_main$status2 == "Existing", ],
         group = "Parks and trails",
         stroke = TRUE,
         color = e_col,
         fill = TRUE,
         fillColor = e_col,
         fillOpacity = 0.9,
         options = pathOptions(pane = "parks_geo"),
         highlightOptions = highlightOptions(
           stroke = TRUE,
           color = "black",
           weight = 6,
           bringToFront = TRUE,
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
       ) %>%
       addPolygons(
         data = main_lft_inputs$map_parktrail_data_main[main_lft_inputs$map_parktrail_data_main$Type == "Park" & main_lft_inputs$map_parktrail_data_main$status2 == "Planned", ],
         group = "Parks and trails",
         stroke = TRUE,
         color = p_col,
         fill = TRUE,
         fillColor = p_col,
         fillOpacity = 0.9,
         options = pathOptions(pane = "parks_geo"),
         highlightOptions = highlightOptions(
           stroke = TRUE,
           color = "black",
           weight = 6,
           bringToFront = TRUE,
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
       ) %>%
       

       addCircles(
         data = main_lft_inputs$map_parktrail_data_main[main_lft_inputs$map_parktrail_data_main$Type == "Park" & main_lft_inputs$map_parktrail_data_main$status2 == "Search", ],
         group = "Parks and trails",
         stroke = TRUE,
         radius = 2000,
         color = s_col,
         fill = TRUE,
         fillColor = s_col,
         fillOpacity = 0.9,
         options = pathOptions(pane = "parks_geo"),
         highlightOptions = highlightOptions(
           stroke = TRUE,
           color = "black", weight = 6,
           bringToFront = TRUE,
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
       )   %>%
       
       addPolylines(
         data = main_lft_inputs$map_parktrail_data_main[main_lft_inputs$map_parktrail_data_main$Type == "Trail" & main_lft_inputs$map_parktrail_data_main$status2 == "Existing", ],
         group = "Parks and trails",
         stroke = TRUE,
         weight = 3,
         color = e_col,
         smoothFactor = 0.3,
         opacity = 0.9,
         options = pathOptions(pane = "parks_geo"),
         popup = ~popup_text,
         highlightOptions = highlightOptions(
           stroke = TRUE,
           color = "black",
           weight = 6,
           bringToFront = TRUE
         )
       ) %>%
       addPolylines(
         data = main_lft_inputs$map_parktrail_data_main[main_lft_inputs$map_parktrail_data_main$Type == "Trail" & main_lft_inputs$map_parktrail_data_main$status2 == "Search", ],
         group = "Parks and trails",
         stroke = TRUE,
         weight = 3, # 3,
         color = s_col,
         smoothFactor = 0.3,
         opacity = 0.9, # 0.5,
         options = pathOptions(pane = "parks_geo"),
         popup = ~popup_text,
         highlightOptions = highlightOptions(
           stroke = TRUE,
           color = "black",
           weight = 6,
           bringToFront = TRUE
         )
       ) %>%
       addPolylines(
         data = main_lft_inputs$map_parktrail_data_main[main_lft_inputs$map_parktrail_data_main$Type == "Trail" & main_lft_inputs$map_parktrail_data_main$status2 == "Planned", ],
         group = "Parks and trails",
         stroke = TRUE,
         weight = 3, # 3,
         color = p_col,
         smoothFactor = 0.3,
         opacity = 0.9, # 0.5,
         options = pathOptions(pane = "parks_geo"),
         popup = ~popup_text,
         highlightOptions = highlightOptions(
           stroke = TRUE,
           color = "black",
           weight = 6,
           bringToFront = TRUE
         )
       )

   })
  
  
  
  toListen_mainleaflet_buffer <- reactive({
    list(current_tab,
      main_lft_inputs$map_parktrail_data_main,
      main_lft_inputs$input_bufferdist
    )
  })
  
  
  observeEvent(
    toListen_mainleaflet_buffer(), {
  print("Rendering buffer layer")
      # addMapPane(name = "buff", zIndex = 650) %>%
      
  leafletProxy("map") %>%
    clearGroup("Buffers") %>%
    addPolygons(
      # options = pathOptions(pane = "buff"),
      data = main_lft_inputs$map_buffer_data_main,
      group = "Buffers",
      stroke = TRUE,
      weight = 2,
      color = "black", 
      fill = FALSE,
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
    
## To be copied in the UI
# mod_main_leaflet_ui("main_leaflet_ui_1")
    
## To be copied in the server
# callModule(mod_main_leaflet_server, "main_leaflet_ui_1")
 
