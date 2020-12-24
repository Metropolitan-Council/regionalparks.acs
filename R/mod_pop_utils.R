#' pop_utils UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pop_utils_ui <- function(id) {
  ns <- NS(id)
  tagList()
}

#' pop_utils Server Function
#'
#' @noRd
mod_pop_utils_server <- function(input, output, session,
                                 selected_population) {
  ns <- session$ns
  
  make_pop_data <- reactive({
    p6 <- 
      regionalparks.acs::est_pop %>%
           select(selected_population$input_pop) %>%
        filter(!is.na(selected_population$input_pop))
    return(p6)
  })

  vals <- reactiveValues()
  
  observe({
    vals$pop_data <- make_pop_data()
  })

  return(vals)

  }

## To be copied in the UI
# mod_pop_utils_ui("pop_utils_ui_1")

## To be copied in the server
# callModule(mod_pop_utils_server, "pop_utils_ui_1")
