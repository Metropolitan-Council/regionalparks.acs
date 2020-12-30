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

  observed <- tibble(observed = c("PopEst_2019", "PopDens_2019"))

  make_pop_data <- reactive({
    p6 <-
      
      if(selected_population$input_pop %in% observed$observed)
      (regionalparks.acs::est_pop %>%
      select(selected_population$input_pop,
             bg_id))
    else 
      (regionalparks.acs::taz_growth %>%
         select(selected_population$input_pop,
                TAZ2012))
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
