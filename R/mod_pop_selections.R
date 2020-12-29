#' pop_selections UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pop_selections_ui <- function(id) {
  ns <- NS(id)
  tagList(
        radioButtons(
          ns("input_pop"),
          label = h5("Observed population and forecasts"),
          
          choices = c("2019 population" = "PopEst_2019",
                      "2019 pop. density" = "PopDens_2019",
                      "2040 forecast pop." = "POP2040",
                      "2040 forecast pop. dens." = "popdens_2040_mi",
                      "Growth, 2010-2040, absolute" = "growth_abs_10_40",
                      "Growth, 2010-2040, relative" = "growth_rel_10_40"),
          # 
          # choices = list(
          #   `Observed` = list(
          #     "2019 population" = "PopEst_2019",
          #     "2019 pop. density" = "PopDens_2019"
          #   ),
          #   `Forecasts` = list(
          #     "2040 forecast pop." = "POP2040",
          #     "2040 forecast pop. dens." = "popdens_2040_mi",
          #     "Growth, 2010-2040, absolute" = "growth_abs_10_40",
          #     "Growth, 2010-2040, relative" = "growth_rel_10_40"
          #   )
          # ),
          selected = "PopEst_2019"
        
      ) )
}



#' pop_selections Server Function
#'
#' @noRd
mod_pop_selections_server <- function(input, output, session) {
  ns <- session$ns

  input_values <- reactiveValues() # start with an empty reactiveValues object.

  observeEvent(input$input_pop, { # only update when the user changes the ACS input
    input_values$input_pop <- input$input_pop # create/update the ACS input value in our reactiveValues object
  })

  return(input_values)
}

## To be copied in the UI
# mod_pop_selections_ui("pop_selections_ui_1")

## To be copied in the server
# callModule(mod_pop_selections_server, "pop_selections_ui_1")
