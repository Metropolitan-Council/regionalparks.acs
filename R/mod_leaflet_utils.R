#' leaflet_utils UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_leaflet_utils_ui <- function(id) {
  ns <- NS(id)
  tagList()
}

#' leaflet_utils Server Function
#'
#' @noRd
mod_leaflet_utils_server <- function(input, output, session,
                                     selected_map_vars) {
  ns <- session$ns
  vals <- reactiveValues()


  make_leafletacs_data <- reactive({
    p6 <-
      if (selected_map_vars$input_acsmap %in% tract_vars$ACS) {
        regionalparks.acs::census_tract_map %>%
          select(selected_map_vars$input_acsmap)
      }

      else {
        regionalparks.acs::block_group_map %>%
          select(selected_map_vars$input_acsmap)
      }
    return(p6)
  })


  observe({
    vals$leaflet_data <- make_leafletacs_data()
  })


  generate_leaflet_pal <- reactive({ # generate color palette
    pal <- colorNumeric(n = 9, palette = "Blues", domain = vals$leaflet_data[[1]])
    return(pal)
  })

  observe({
    vals$leaflet_pal <- generate_leaflet_pal()
  })

  return(vals)
}

## To be copied in the UI
# mod_leaflet_utils_ui("leaflet_utils_ui_1")

## To be copied in the server
# callModule(mod_leaflet_utils_server, "leaflet_utils_ui_1")
