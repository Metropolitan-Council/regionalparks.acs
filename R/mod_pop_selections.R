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
      column(
        width = 3,
        selectInput(
          ns("input_pop"),
          label = h5("Observed population and forecasts"),
          choices = list(
            `Observed` = list("2019 population" = "PopEst_2019",
                              "2019 pop. density" = "PopDens_2019"),
            `Forecasts` = list(
              "2040 forecast pop." = "POP2040",
              "2040 forecast pop. dens." = "popdens_2040_mi",
              "Growth, 2010-2040, absolute" = "growth_abs_10_40",
              "Growth, 2010-2040, relative" = "growth_rel_10_40"
            )),
            selected = "PopEst_2019"
          )
        ),
        column(
          width = 3,
          selectInput(#izeInput(
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
  # options = list(
  #   placeholder = 'Please select an option below',
  #   onInitialize = I('function() { this.setValue(""); }')
  # ),
  multiple = TRUE, selectize = T
)),
column(width = 2,
radioButtons(
  ns("input_distance"),
  label = h5("Buffer dist. (mi)"),
  choices = c(1.0, 1.5, 3),
  # selected = character(0),
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

  input_values <- reactiveValues() # start with an empty reactiveValues object.
  
  observeEvent(input$input_pop, { # only update when the user changes the ACS input
    input_values$input_pop <- input$input_pop # create/update the ACS input value in our reactiveValues object
  })
  
  observeEvent(input$input_agency, { 
    input_values$input_agency <- input$input_agency 
  })
  
  observeEvent(input$input_distance, { 
    input_values$input_distance <- input$input_distance 
  })
  
  observeEvent(input$input_type, { 
    input_values$input_type <- input$input_type 
  })
  
  observeEvent(input$input_status, { 
    input_values$input_status <- input$input_status 
  })
  
  return(input_values)
  
}

## To be copied in the UI
# mod_pop_selections_ui("pop_selections_ui_1")

## To be copied in the server
# callModule(mod_pop_selections_server, "pop_selections_ui_1")
