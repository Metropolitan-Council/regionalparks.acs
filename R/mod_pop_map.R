#' pop_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_pop_map_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # HTML('</p>The Metropolitian Council publishes current population estimates and future forecasted population estimates. Current populaton estimates are available for Census block groups. Future forecasts are based on 2010 Census data and city comprehensive plans and available at the transportation analysis zone (a coarser spatial resolution). Given the differential methods and geographies used in calcuating current and future populations, we will not perform further analyses on these data. However, the overarching patterns still may be useful in parks planning. More information and raw data can be found on the <a href = "https://metrocouncil.org/Data-and-Maps/Research-and-Data/Thrive-2040-Forecasts.aspx">Metropolitian Council website</a>.</p>'),
    # use_waiter(),
    
    # verbatimTextOutput(ns("test")),
    leafletOutput(ns("popmap"), width = "100%", height = 600)
  )
}

#' pop_map Server Function
#'
#' @noRd
mod_pop_map_server <- function(input, output, session,
                               summary_poputil,
                               selected_popvars#,
                               # parktrail_util,
                               # selected_parktrail
) {
  ns <- session$ns
  
  # w <- Waiter$new(id = ns("popmap"))
  # output$test<-renderPrint(selected_popvars$input_pop)
  
  output$popmap <- mod_leaflet_base_server(input = input,
                                           output = output,
                                           session = session) #----
  
  # pal <- case_when(
  #   selected_popvars$input_pop == "PopDens_2019" ~ 
  #     colorQuantile(n=9, palette = "Blues", domain = summary_poputil$pop_data[[1]]),
  #   TRUE ~ 
  #     colorNumeric(n = 9, palette = "Purples", domain = summary_poputil$pop_data[[1]])
  # )
  
  # test <- tibble::tribble(
  #   ~var, ~pal,
  #   "PopDens_2019", colorQuantile(n=9, palette = "Purples", domain = regionalparks.acs::est_pop$PopDens_2019[[1]]),
  #   "PopEst_2019", colorQuantile(n=9, palette = "Blues", domain = regionalparks.acs::est_pop$PopEst_2019[[1]]),
  #   "growth_rel_10_40", colorQuantile(n = 9, palette = "Greens", domain = regionalparks.acs::taz_growth$growth_rel_10_40[[1]]),
  #   "growth_abs_10_40", colorQuantile(n = 9, palette = "Greens", domain = regionalparks.acs::taz_growth$growth_abs_10_40[[1]]),
  #   "popdens_2040_mi", colorQuantile(n = 9, palette = "Greens", domain = regionalparks.acs::taz_growth$popdens_2040_mi[[1]]),
  #   "POP2040", colorQuantile(n = 9, palette = "Greens", domain = regionalparks.acs::taz_growth$POP2040[[1]]))
  
  # outputOptions(output, "popmap", suspendWhenHidden = FALSE)
  
  observeEvent(selected_popvars$input_pop,{
    # w$show()
    
    pal <- if(
      selected_popvars$input_pop == "PopDens_2019" | selected_popvars$input_pop == "popdens_2040_mi" | selected_popvars$input_pop == "growth_rel_10_40")
      (colorQuantile(n = 9, palette = "Blues", domain = summary_poputil$pop_data[[1]])) else (
        colorNumeric(n = 9, palette = "Blues", domain = summary_poputil$pop_data[[1]])
      )
    (leafletProxy("popmap") %>%
        clearGroup("Population data") %>%
        clearControls() %>%
        addPolygons(
          group = "Population data",
          data = summary_poputil$pop_data,
          stroke = TRUE,
          color = councilR::colors$suppGray,
          opacity = 0.6,
          weight = 0.25,
          fillOpacity = 0.6,
          smoothFactor = 0.2,
          fillColor = ~ pal(summary_poputil$pop_data[[1]]),
          popup = case_when(
            selected_popvars$input_pop == "growth_rel_10_40" ~
              paste0(tags$strong(filter(popkey, popvar == selected_popvars$input_pop) %>% select(goodname)), ": ", round(summary_poputil$pop_data[[1]], 2), " x"),
            (selected_popvars$input_pop == "popdens_2040_mi" | selected_popvars$input_pop == "PopDens_2019") ~
              paste0(tags$strong(filter(popkey, popvar == selected_popvars$input_pop) %>% select(goodname)), ": ", format(round(summary_poputil$pop_data[[1]], 1), big.mark = ","), " persons/mile"),
            TRUE ~ paste0(tags$strong(filter(popkey, popvar == selected_popvars$input_pop) %>% select(goodname)), ": ", format(summary_poputil$pop_data[[1]], big.mark = ","), " persons")
          ),
          options = list(zIndex = 0)
        ) %>%
        addLegend(title = paste0(filter(popkey, popvar == selected_popvars$input_pop) %>% select(short)),
                  position = "bottomleft",
                  group = "Population data",
                  layerId = "Population data",
                  pal = pal,
                  values = summary_poputil$pop_data[[1]]))
    
    # on.exit({w$hide()})
  }
  )
}

## To be copied in the UI
# mod_pop_map_ui("pop_map_ui_1")

## To be copied in the server
# callModule(mod_pop_map_server, "pop_map_ui_1")
