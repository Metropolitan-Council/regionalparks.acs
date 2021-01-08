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
                               util_leaflet, selected_map_vars){
  ns <- session$ns
 
  # output$test<-renderPrint(selected_map_vars$input_acsmap)
  
  
  output$overviewmap <-renderLeaflet({ #  map --------
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
      
      addPolygons(
        data = agency_boundary,
        group = "Agency boundaries",
        stroke = T,
        color = "black",
        fill = F,
        weight = 2,
        options = pathOptions(pane = "Agency boundaries")
      ) %>%
      
      addMapPane("parks_geo", zIndex = 420) %>%
      
      addPolygons(
        data = park_trail_geog_LONG[park_trail_geog_LONG$status == "Park - existing", ], 
        group = "Regional Parks - existing",
        stroke = TRUE,
        # weight = 0.5,
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
        popup = ~ paste0(
          "<b>", park_trail_geog_LONG[park_trail_geog_LONG$status == "Park - existing", ]$status, "</b>", "<br>",
          park_trail_geog_LONG[park_trail_geog_LONG$status == "Park - existing", ]$name, "<br>", "<em>",
          park_trail_geog_LONG[park_trail_geog_LONG$status == "Park - existing", ]$agency, "</em>"
        ),
        popupOptions = popupOptions(
          closeButton = FALSE,
          style = list(
            "font-size" = "18px",
            "font-family" = "Arial"
          )
        )
      ) %>%
      addPolygons(
        data = park_trail_geog_LONG[park_trail_geog_LONG$status == "Park - planned", ],
        group = "Regional Parks - planned",
        stroke = TRUE,
        # weight = 0.5,
        color = p_col,
        fill = TRUE,
        fillColor = p_col,
        fillOpacity = 0.9,
        options = pathOptions(pane = "parks_geo"),
        highlightOptions = highlightOptions(
          stroke = TRUE,
          color = "black", weight = 6,
          bringToFront = TRUE,
          opacity = 1
        ),
        popup = ~ paste0(
          "<b>", park_trail_geog_LONG[park_trail_geog_LONG$status == "Park - planned", ]$status, "</b>", "<br>",
          park_trail_geog_LONG[park_trail_geog_LONG$status == "Park - planned", ]$name, "<br>",
          "<em>", park_trail_geog_LONG[park_trail_geog_LONG$status == "Park - planned", ]$agency, "</em>"
        ),
        popupOptions = popupOptions(
          closeButton = FALSE,
          style = list(
            "font-size" = "18px",
            "font-family" = "Arial"
          )
        )
      ) %>%
      addCircles(
        data = park_trail_geog_LONG[park_trail_geog_LONG$status == "Park - search", ],
        group = "Regional Parks - search",
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
        popup = ~ paste0(
          "<b>", park_trail_geog_LONG[park_trail_geog_LONG$status == "Park - search", ]$status, "</b>", "<br>",
          "<em>", park_trail_geog_LONG[park_trail_geog_LONG$status == "Park - search", ]$name, "</em>"
        ),
        popupOptions = popupOptions(
          closeButton = FALSE,
          style = list(
            "font-size" = "18px",
            "font-family" = "Arial"
          )
        )
      ) %>%
      
      addPolylines(
        data = park_trail_geog_LONG[park_trail_geog_LONG$status == "Trail - existing", ],
        group = "Regional Trails - existing",
        stroke = TRUE,
        weight = 3, 
        color = e_col,
        smoothFactor = 0.3,
        opacity = 0.9, 
        options = pathOptions(pane = "parks_geo"),
        popup = ~ paste0(
          "<b>", park_trail_geog_LONG[park_trail_geog_LONG$status == "Trail - existing", ]$status, "</b>", "<br>",
          park_trail_geog_LONG[park_trail_geog_LONG$status == "Trail - existing", ]$name, "<br>",
          "<em>", park_trail_geog_LONG[park_trail_geog_LONG$status == "Trail - existing", ]$agency, "</em>"
        ),
        highlightOptions = highlightOptions(
          stroke = TRUE,
          color = "black", 
          weight = 6,
          bringToFront = TRUE
        )
      ) %>%
      addPolylines(
        data = park_trail_geog_LONG[park_trail_geog_LONG$status == "Trail - search", ],
        group = "Regional Trails - search",
        stroke = TRUE,
        weight = 3, # 3,
        color = s_col,
        smoothFactor = 0.3,
        opacity = 0.9, # 0.5,
        options = pathOptions(pane = "parks_geo"),
        popup = ~ paste0(
          "<b>", park_trail_geog_LONG[park_trail_geog_LONG$status == "Trail - search", ]$status, "</b>", "<br>",
          park_trail_geog_LONG[park_trail_geog_LONG$status == "Trail - search", ]$name, "<br>",
          "<em>", park_trail_geog_LONG[park_trail_geog_LONG$status == "Trail - search", ]$agency, "<em>"
        ),
        highlightOptions = highlightOptions(
          stroke = TRUE,
          color = "black",
          weight = 6,
          bringToFront = TRUE
        )
      ) %>%
      addPolylines(
        data = park_trail_geog_LONG[park_trail_geog_LONG$status == "Trail - planned", ],
        group = "Regional Trails - planned",
        stroke = TRUE,
        weight = 3, # 3,
        color = p_col,
        smoothFactor = 0.3,
        opacity = 0.9, # 0.5,
        options = pathOptions(pane = "parks_geo"),
        popup = ~ paste0(
          "<b>", park_trail_geog_LONG[park_trail_geog_LONG$status == "Trail - planned", ]$status, "</b>", "<br>",
          park_trail_geog_LONG[park_trail_geog_LONG$status == "Trail - planned", ]$name, "<br>",
          "<em>", park_trail_geog_LONG[park_trail_geog_LONG$status == "Trail - planned", ]$agency, "</em>"
        ),
        highlightOptions = highlightOptions(
          stroke = TRUE,
          color = "black",
          weight = 6,
          bringToFront = TRUE
        )
      )  %>%
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
      )  %>%
      
  
      hideGroup(
        c(
          "Regional Parks - planned",
          "Regional Trails - planned",
          "Regional Parks - search",
          "Regional Trails - search",
          "Transit",
          "Water Access",
          "Park Entrance",
          "Rivers & Lakes"
        )
      ) %>%
      
      addLayersControl(
        position = "bottomright",
        overlayGroups = c(
          "Regional Parks - existing",
          "Regional Trails - existing",
          "Regional Parks - planned",
          "Regional Trails - planned",
          "Regional Parks - search",
          "Regional Trails - search",
          "Demographic data",
          "Transit",
          "Water Access",
          "Park Entrance",
          "Rivers & Lakes",
          "Agency boundaries"
        ),
        baseGroups = c(
          "Stamen Toner",
          "Carto Positron",
          "Esri Imagery"
        ),
        options = layersControlOptions(collapsed = T)
      ) %>%
      leaflet::addScaleBar(position = c("bottomleft")) 
  }) #----
  
  
  

  observeEvent(selected_map_vars$input_acsmap, {
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
                pal = pal,
                values = util_leaflet$leaflet_data[[1]])
  })

  
}
    
## To be copied in the UI
# mod_leaflet_ui("leaflet_ui_1")
    
## To be copied in the server
# callModule(mod_leaflet_server, "leaflet_ui_1")
 
