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
h3("Select inputs: "),
fluidRow(
  column(width = 3,
radioButtons(
  ns("inputPop"),
  label = h5("Population estimates and forecasts"),
  choices = c("2019 population" = "PopEst_2019",
             "2019 pop. density" = "PopDens_2019"),
  selected = "PopEst_2019")),

column( width = 3,
selectInput(
  ns("input_agency"),
  label = h5("Agenc(y/ies)"),
  choices = c(
    "Anoka County",
    "Bloomington",
    "Carver County",
    "Dakota County",
    "MPRB",
    "Ramsey County",
    "Scott County",
    "St. Paul",
    "Three Rivers",
    "Washington County"
  ),
  selected = "Anoka County",
  multiple = TRUE, selectize = T
)),
column(width = 2,
radioButtons(
  ns("input_distance"),
  label = h5("Buffer dist. (mi)"),
  choices = c(1.0, 1.5, 3),
  selected = c(1.0)
)),
column(width = 2,
checkboxGroupInput(
  ns("input_type"),
  label = h5("Type"),
  choices = c("Park", "Trail"),
  selected = c("Park", "Trail")
)),
column(width=2,
checkboxGroupInput(
  ns("input_status"),
  label = h5("Status"),
  choices = c("Existing", "Planned", "Search"), # HTML("<strong>Existing</strong>") #something like this doesn't exist, but if these text colors could match point colors that would be great!
  selected = c("Existing", "Planned", "Search")
)))

  )
}

#' pop_selections Server Function
#'
#' @noRd
mod_pop_selections_server <- function(input, output, session) {
  ns <- session$ns


  vals <- reactiveValues()

  observeEvent(input$inputPop, {
    vals$selected_var <- input$inputPop
    vals$pop_data <- est_pop[input$inputPop]
  })

  return(vals)
}

## To be copied in the UI
# mod_pop_selections_ui("pop_selections_ui_1")

## To be copied in the server
# callModule(mod_pop_selections_server, "pop_selections_ui_1")
