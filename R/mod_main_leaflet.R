#' main_leaflet UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_main_leaflet_ui <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("map"), width = "100%", height = 700),
  )
}

#' main_leaflet Server Function
#'
#' @noRd
mod_main_leaflet_server <- function(
  input,
  output,
  session,
  main_lft_inputs,
  current_tab
) {
  ns <- session$ns
  w <- Waiter$new(ns("map")) # , html="Please wait")#, hide_on_render=T)

  # output$map ----
  output$map <- mod_map_base_server(
    input = input,
    output = output,
    session = session
  )

  # adding proxys --------

  # add pop polygons -----
  toListen_mainleaflet <- reactive({
    list(
      # current_tab,
      main_lft_inputs$map_bg_data_main,
      main_lft_inputs$source,
      main_lft_inputs$mainacs,
      main_lft_inputs$mainpop,
      main_lft_inputs$pop_pal
    )
  })

  observeEvent(
    toListen_mainleaflet(),
    {
      # browser()
      print("Rendering main leaflet map - pop polygons")
      leafletProxy("map") %>%
        clearGroup("Population data") %>%
        # removeControl("Population data") %>%
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
                main_lft_inputs$map_bg_data_main[[1]],
                "%"
              )
            }
          } else {
            case_when(
              main_lft_inputs$mainpop == "growth_rel_10_40" ~
              paste0(tags$strong(filter(popkey, popvar == main_lft_inputs$mainpop) %>% select(goodname)), ": ", round(main_lft_inputs$map_bg_data_main[[1]], 2), " x"),
              (main_lft_inputs$mainpop == "popdens_2040_mi" | main_lft_inputs$mainpop == "PopDens_2019") ~
              paste0(tags$strong(filter(popkey, popvar == main_lft_inputs$mainpop) %>% select(goodname)), ": ", format(round(main_lft_inputs$map_bg_data_main[[1]], 1), big.mark = ","), " persons/mile"),
              TRUE ~ paste0(tags$strong(filter(popkey, popvar == main_lft_inputs$mainpop) %>% select(goodname)), ": ", format(main_lft_inputs$map_bg_data_main[[1]], big.mark = ","), " persons")
            )
          }
        ) %>%
        addLegend(
          labFormat = labelFormat2(),
          title = if (main_lft_inputs$source == "Population characteristics") {
            paste0(filter(renamekey, ACS == main_lft_inputs$mainacs) %>% select(gn2))
          } else {
            paste0(filter(popkey, popvar == main_lft_inputs$mainpop) %>% select(s2))
          },
          position = "bottomleft",
          group = "Population data",
          layerId = "Population data",
          pal = main_lft_inputs$pop_pal,
          values = main_lft_inputs$map_bg_data_main[[1]]
        )
    }
  )

  # add parktrails
  toListen_mainleaflet_parktrail <- reactive({
    list(
      # current_tab,
      main_lft_inputs$map_parktrail_data_main
    )
  })

  observeEvent(
    toListen_mainleaflet_parktrail(),
    {
      # browser()
      print("Rendering main leaflet map - parks/trails")

      leafletProxy("map") %>%
        clearGroup("Parks and trails") %>%
        clearControls() %>%
        addPolygons(
          data = main_lft_inputs$map_parktrail_data_main[main_lft_inputs$map_parktrail_data_main$status == "Park - existing", ],
          group = "Parks and trails",
          stroke = TRUE,
          color = e_col,
          fill = TRUE,
          fillColor = e_col,
          fillOpacity = 0.9,
          options = pathOptions(pane = "parks_geo"),
          highlightOptions = leaflet_highlight_options,
          popup = ~popup_text,
          popupOptions = leaflet_popup_options
        ) %>%
        addPolygons(
          data = main_lft_inputs$map_parktrail_data_main[main_lft_inputs$map_parktrail_data_main$status == "Park - planned", ],
          group = "Parks and trails",
          stroke = TRUE,
          color = p_col,
          fill = TRUE,
          fillColor = p_col,
          fillOpacity = 0.9,
          options = pathOptions(pane = "parks_geo"),
          highlightOptions = leaflet_highlight_options,
          popup = ~popup_text,
          popupOptions = leaflet_popup_options
        ) %>%
        addCircles(
          data = main_lft_inputs$map_parktrail_data_main[
            main_lft_inputs$map_parktrail_data_main$status == "Park - search", ],
          group = "Parks and trails",
          stroke = TRUE,
          radius = 2000,
          color = s_col,
          fill = TRUE,
          fillColor = s_col,
          fillOpacity = 0.9,
          options = pathOptions(pane = "parks_geo"),
          highlightOptions = leaflet_highlight_options,
          popup = ~popup_text,
          popupOptions = leaflet_popup_options
        ) %>%
        addPolylines(
          data = main_lft_inputs$map_parktrail_data_main[main_lft_inputs$map_parktrail_data_main$status == "Trail - existing", ],
          group = "Parks and trails",
          stroke = TRUE,
          weight = 3,
          color = e_col,
          smoothFactor = 0.3,
          opacity = 0.9,
          options = pathOptions(pane = "parks_geo"),
          popup = ~popup_text,
          highlightOptions = leaflet_highlight_options,
          popupOptions = leaflet_popup_options
        ) %>%
        addPolylines(
          data = main_lft_inputs$map_parktrail_data_main[main_lft_inputs$map_parktrail_data_main$status == "Trail - search", ],
          group = "Parks and trails",
          stroke = TRUE,
          weight = 3,
          color = s_col,
          smoothFactor = 0.3,
          opacity = 0.9,
          options = pathOptions(pane = "parks_geo"),
          popup = ~popup_text,
          highlightOptions = leaflet_highlight_options,
          popupOptions = leaflet_popup_options
        ) %>%
        addPolylines(
          data = main_lft_inputs$map_parktrail_data_main[main_lft_inputs$map_parktrail_data_main$status == "Trail - planned", ],
          group = "Parks and trails",
          stroke = TRUE,
          weight = 3,
          color = p_col,
          smoothFactor = 0.3,
          opacity = 0.9,
          options = pathOptions(pane = "parks_geo"),
          popup = ~popup_text,
          highlightOptions = leaflet_highlight_options,
          popupOptions = leaflet_popup_options
        )
    }
  )



  toListen_mainleaflet_buffer <- reactive({
    list(
      # current_tab,
      main_lft_inputs$map_parktrail_data_main,
      main_lft_inputs$input_bufferdist
    )
  })


  observeEvent(
    toListen_mainleaflet_buffer(),
    {
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
          popup = ~popup_text,
          popupOptions = leaflet_popup_options,
          highlightOptions = leaflet_highlight_options
        )
    }
  )


  # startup ------------------
  observeEvent(
    once = TRUE,
    ignoreInit = TRUE,
    label = "startup",
    current_tab,
    {
      w$show()
      # browser()
      # getDefaultReactiveDomain()
      print("Rendering start-up map")

      leafletProxy("map") %>%
        ## park start-up polygons ---------
        addPolygons(
          data = filter(park_trail_geog_LONG, status == "Park - existing"), 
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
          popupOptions = leaflet_popup_options
        ) %>%
        addPolylines(
          data = filter(park_trail_geog_LONG, status == "Trail - existing"),
          group = "Parks and trails",
          stroke = TRUE,
          weight = 3,
          color = e_col,
          smoothFactor = 0.3,
          opacity = 0.9,
          options = pathOptions(pane = "parks_geo"),
          popup = ~popup_text,
          highlightOptions = leaflet_highlight_options,
        ) %>%
        addPolygons(
          group = "Population data",
          data = select(block_group_map, adj_ageunder15_per), 
          stroke = TRUE,
          color = councilR::colors$suppGray,
          opacity = 0.6,
          weight = 0.25,
          fillOpacity = 0.6,
          smoothFactor = 0.2,
          fillColor = ~ colorBin(bins = 5, palette = "Blues", pretty = F, domain = block_group_map$adj_ageunder15_per)(block_group_map$adj_ageunder15_per),
          options = list(zIndex = 0),
          popup =  ~ paste0(
                tags$strong(filter(renamekey, ACS == "adj_ageunder15_per") %>% select(goodname)),
                ": ",
                select(block_group_map, adj_ageunder15_per)[[1]],
                "%"
              )
        ) %>%
        addLegend(
          labFormat = labelFormat2(),
          title = paste0(filter(renamekey, ACS == "adj_ageunder15_per") %>% select(gn2)),
          position = "bottomleft",
          group = "Population data",
          layerId = "Population data",
          pal = colorBin(bins = 5, palette = "Blues", pretty = F, domain = block_group_map$adj_ageunder15_per), 
          values = (block_group_map$adj_ageunder15_per)
        ) %>%
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
          popup = ~popup_text,
          popupOptions = leaflet_popup_options,
          highlightOptions = leaflet_highlight_options
        ) %>%
        hideGroup(group = "buffers")
      w$hide()
      # waiter_hide_on_render()
      # waiter_hide()
    }
  )
}

## To be copied in the UI
# mod_main_leaflet_ui("main_leaflet_ui_1")

## To be copied in the server
# callModule(mod_main_leaflet_server, "main_leaflet_ui_1")
