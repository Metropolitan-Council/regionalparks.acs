#' selections_population UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_selections_population_ui <- function(id) {
  ns <- NS(id)
  tagList(

    # fluidRow(
    # column(
    # width = 3,
    selectInput(
      ns("input_pop"),
      label = h5("Observed & forecasted pop."),
      choices = list(
        `Observed` = list(
          "2019 population" = "PopEst_2019",
          "2019 pop. density" = "PopDens_2019"
        ),
        `Forecasts` = list(
          "2040 forecast pop." = "POP2040",
          "2040 forecast pop. dens." = "popdens_2040_mi",
          "Growth, 2010-2040, absolute" = "growth_abs_10_40",
          "Growth, 2010-2040, relative" = "growth_rel_10_40"
        )
      ),
      selected = "PopEst_2019"
      # )
      # )
    )
  )
}

#' selections_population Server Function
#'
#' @noRd
mod_selections_population_server <- function(input, output, session) {
  ns <- session$ns

  input_values <- reactiveValues() # start with an empty reactiveValues object.

  observeEvent(input$input_pop, { # only update when the user changes the ACS input
    input_values$input_pop <- input$input_pop # create/update the ACS input value in our reactiveValues object
  })

  return(input_values)
}

## To be copied in the UI
# mod_selections_population_ui("selections_population_ui_1")

## To be copied in the server
# callModule(mod_selections_population_server, "selections_population_ui_1")
