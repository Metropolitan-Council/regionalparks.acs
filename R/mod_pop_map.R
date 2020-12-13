#' pop_map UI Function
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
mod_pop_map_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    HTML("</p>The Metropolitian Council publishes current population estimates and future forecasted population estimates. Current populaton estimates are calculated XXX and available for Census block groups (pub date). Future population forecasts represent shared expectations of population change between cities and the Metropolitian Council (pub date). Forecasts are based off of 2010 Census data and city comprehensive plans and available at the transportation analysis zone (TAZ, a coarser spatial resolution than Census block groups). Given the differential methods and geographies used in calcuating current and future populations, we will not perform further analyses on these data. However, the overarching spatial patterns still may be useful in anticipating areas which may have increased need for park access. See (link: https://metrocouncil.org/Data-and-Maps/Research-and-Data/Thrive-2040-Forecasts.aspx) for more detail Also note that pop forecasts are in the comunity profiles.</p>"),
    
    leafletOutput(ns("popmap"), width = "100%", height = 900)
    
      )
}
    
#' pop_map Server Function
#'
#' @noRd 
mod_pop_map_server <- function(input, output, session,
                               pop_data = pop_data){
  ns <- session$ns
  
  
  output$popmap <- renderLeaflet({
    leaflet() %>%
      setView(lat = 44.963, lng = -93.22, zoom = 10) %>%
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
      addMapPane("parks_geo", zIndex = 420) %>%
      addPolygons(
        data = park_trail_geog$park,
        group = "Regional Parks - existing",
        stroke = TRUE,
        # weight = 0.5,
        color = e_col, #councilR::colors$playGreen, # councilR::colors$suppWhite,
        fill = TRUE,
        fillColor = e_col, #councilR::colors$playGreen,
        fillOpacity = 1, # 0.8,
        options = pathOptions(pane = "parks_geo"),
        highlightOptions = highlightOptions(
          stroke = TRUE,
          color = "black",
          weight = 6,
          bringToFront = TRUE,
          opacity = 1
        ),
        popup = ~ paste0(
          "<b>", park_trail_geog$park$status, "</b>", "<br>",
          park_trail_geog$park$name, "<br>", "<em>",
          park_trail_geog$park$agency, "</em>"
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
        data = park_trail_geog$park_planned,
        group = "Regional Parks - planned",
        stroke = TRUE,
        # weight = 0.5,
        color = p_col, #councilR::colors$suppGray,
        fill = TRUE,
        fillColor = p_col, #councilR::colors$suppGray,
        fillOpacity = 1, # 0.8,
        options = pathOptions(pane = "parks_geo"),
        highlightOptions = highlightOptions(
          stroke = TRUE,
          color = "black", weight = 6,
          bringToFront = TRUE,
          opacity = 1
        ),
        popup = ~ paste0(
          "<b>", park_trail_geog$park_planned$status, "</b>", "<br>",
          park_trail_geog$park_planned$name, "<br>",
          "<em>", park_trail_geog$park_planned$agency, "</em>"
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
        data = park_trail_geog$park_search,
        group = "Regional Parks - search",
        stroke = TRUE,
        radius = 2000,
        # weight = 0.5,
        color = s_col, #councilR::colors$suppGray,
        fill = TRUE,
        fillColor = s_col, #councilR::colors$suppGray,
        fillOpacity = 1, # 0.8,
        options = pathOptions(pane = "parks_geo"),
        highlightOptions = highlightOptions(
          stroke = TRUE,
          color = "black", weight = 6,
          bringToFront = TRUE,
          opacity = 1
        ),
        popup = ~ paste0(
          "<b>", park_trail_geog$park_search$status, "</b>", "<br>",
          "<em>", park_trail_geog$park_search$name, "</em>"
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
        data = agency_boundary,
        group = "Agency boundaries",
        fill = FALSE,
        stroke = TRUE,
        weight = 2, # 0.75,
        color = councilR::colors$suppGray
      ) %>%
      addPolylines(
        data = park_trail_geog$trail,
        group = "Regional Trails - existing",
        stroke = TRUE,
        weight = 3, # 3,
        color = e_col, #councilR::colors$playGreen,
        smoothFactor = 0.3,
        opacity = 1, # 0.5,
        options = pathOptions(pane = "parks_geo"),
        popup = ~ paste0(
          "<b>", park_trail_geog$trail$status, "</b>", "<br>",
          park_trail_geog$trail$name, "<br>",
          "<em>", park_trail_geog$trail$agency, "</em>"
        ),
        highlightOptions = highlightOptions(
          stroke = TRUE,
          # color = councilR::colors$suppGray,
          color = "black", # "white",
          weight = 6,
          bringToFront = TRUE
        )
      ) %>%
      addPolylines(
        data = park_trail_geog$trail_search,
        group = "Regional Trails - search",
        stroke = TRUE,
        weight = 3, # 3,
        color = s_col, #councilR::colors$suppGray,
        smoothFactor = 0.3,
        opacity = 1, # 0.5,
        options = pathOptions(pane = "parks_geo"),
        popup = ~ paste0(
          "<b>", park_trail_geog$trail_search$status, "</b>", "<br>",
          park_trail_geog$trail_search$name, "<br>",
          "<em>", park_trail_geog$trail_search$agency, "<em>"
        ),
        highlightOptions = highlightOptions(
          stroke = TRUE,
          # color = councilR::colors$suppGray,
          color = "black",
          weight = 6,
          bringToFront = TRUE
        )
      ) %>%
      addPolylines(
        data = park_trail_geog$trail_planned,
        group = "Regional Trails - planned",
        stroke = TRUE,
        weight = 3, # 3,
        color = p_col, #councilR::colors$suppGray,
        smoothFactor = 0.3,
        opacity = 1, # 0.5,
        options = pathOptions(pane = "parks_geo"),
        popup = ~ paste0(
          "<b>", park_trail_geog$trail_planned$status, "</b>", "<br>",
          park_trail_geog$trail_planned$name, "<br>",
          "<em>", park_trail_geog$trail_planned$agency, "</em>"
        ),
        highlightOptions = highlightOptions(
          stroke = TRUE,
          # color = councilR::colors$suppGray,
          color = "black",
          weight = 6,
          bringToFront = TRUE
        )
      ) %>%
      addStyleEditor() %>%
      hideGroup(
        c(
          "Regional Parks - planned",
          "Regional Trails - planned",
          "Regional Parks - search",
          "Regional Trails - search"
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
          "Agency boundaries",
          "Pop. Estimates"
        ),
        baseGroups = c(
          "Stamen Toner",
          "Carto Positron",
          "Esri Imagery"
        ),
        options = layersControlOptions(collapsed = T)
      ) %>%
      leaflet::addScaleBar(position = c("bottomleft"))
  })
  
  observeEvent(pop_data$pop_data, {
    pal <- 
      colorQuantile(
        palette = "Purples",
        n = 7,
        # reverse = TRUE,
        domain = pop_data$pop_data[[1]]
      )

    # browser()
    leafletProxy("popmap") %>%
      clearGroup("Pop. Estimates") %>%
      clearControls() %>%
      addPolygons(
        data = pop_data$pop_data,
        group = "Pop. Estimates",
        stroke = TRUE,
        color = councilR::colors$suppGray,
        opacity = 0.6,
        weight = 0.25,
        fillOpacity = 0.6,
        smoothFactor = 0.2,
        fillColor = ~ colorQuantile(
          palette = "Purples",
          n = 7,
          # reverse = TRUE,
          domain = pop_data$pop_data[[1]]
        )(pop_data$pop_data[[1]]),
        
        popup = 
          ~ paste0(
            tags$strong(pop_data$selected_var),
            ": ",
            format(pop_data$pop_data[[1]], big.mark = ",")), 
        # options = list(zIndex = 0),
        popupOptions = popupOptions(closeOnClick = TRUE)
      ) %>%
      
      addLegend("topright",
                pal = colorQuantile(
                  n = 7,
                  palette = "Purples",
                  domain = pop_data$pop_data[[1]]
                ),
                values = (pop_data$pop_data[[1]]),
                title = paste0((names(pop_data$pop_data)[[1]])), #(names(summary_util$map_bg_data)[[1]]),
                opacity = 1, 
                group = "Pop.Estimates",
                labFormat = function(type, cuts, p) {
                  n = length(cuts)
                  paste0(format(round(cuts[-n], 0), big.mark = ","), " &ndash; ", format(round(cuts[-1], 0), big.mark = ","))
                }
                )
  })
 
}
    
## To be copied in the UI
# mod_pop_map_ui("pop_map_ui_1")
    
## To be copied in the server
# callModule(mod_pop_map_server, "pop_map_ui_1")
 
