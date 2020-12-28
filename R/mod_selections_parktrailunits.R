#' selections_parktrailunits UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_selections_parktrailunits_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # fluidRow(
    column(
      width = 5,
      selectInput( # izeInput(
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
      )
    ),
    column(
      width = 2,
      radioButtons(
        ns("input_distance"),
        label = h5("Buffer dist. (mi)"),
        choices = c(1.0, 1.5, 3),
        # selected = character(0),
        selected = c(1.0)
      )
    ),
    column(
      width = 2,
      checkboxGroupInput(
        ns("input_type"),
        label = h5("Type"),
        choices = c("Park", "Trail"),
        selected = c("Park", "Trail")
      )
    ),
    column(
      width = 2,
      checkboxGroupInput(
        ns("input_status"),
        label = h5("Status"),
        choices = c("Existing", "Planned", "Search"),
        selected = c("Existing", "Planned", "Search")
      )
    )
  )

  # )
}

#' selections_parktrailunits Server Function
#'
#' @noRd
mod_selections_parktrailunits_server <- function(input, output, session) {
  ns <- session$ns

  input_values <- reactiveValues() # start with an empty reactiveValues object.

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
# mod_selections_parktrailunits_ui("selections_parktrailunits_ui_1")

## To be copied in the server
# callModule(mod_selections_parktrailunits_server, "selections_parktrailunits_ui_1")
