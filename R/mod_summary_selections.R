#' summary_selections UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_summary_selections_ui <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanel(
      # h3("Select inputs"),
      # column(
      # width = 3,
      selectInput(
        # width = '100%',
        ns("input_acs"),
        label = h4("Choose population characteristic"),
        choices = ACSMenu,
        selected = deframe(name_helper[1, 1]),
        selectize = F
        # )
      ),
      # column(
      # width = 3,
      selectInput(
        ns("input_agency"),
        label = h4("Select a park agency or agencies"),
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
        multiple = TRUE,
        selectize = T
        # )
      ),
      # column(
      # width = 2,
      radioButtons(
        ns("input_distance"),
        label = h4("Buffer distance (in miles)"),
        choices = c(1.0, 1.5, 3),
        selected = c(1.0)
        # )
      ) %>%
        shinyhelper::helper(
          type = "markdown",
          content = "BufferHelp"
        ),
      # column(
      # width = 2,
      checkboxGroupInput(
        ns("input_type"),
        label = h4("Unit Type"),
        choices = c("Park", "Trail"),
        selected = c("Park", "Trail")
        # )
      ),
      # column(
      # width = 2,
      checkboxGroupInput(
        ns("input_status"),
        label = h4("Unit Status"),
        choices = c("Existing", "Planned", "Search"),
        # HTML("<strong>Existing</strong>") #something like this doesn't exist, but if these text colors could match point colors that would be great!
        selected = c("Existing", "Planned", "Search")
      ) %>%
        shinyhelper::helper(
          type = "markdown",
          content = "StatusHelp"
        ),
      # )
    )
  )
}

#' summary_selections Server Function
#'
#' @noRd
mod_summary_selections_server <- function(input, output, session) {
  ns <- session$ns

  input_values <- reactiveValues() # start with an empty reactiveValues object.

  observeEvent(input$input_acs, { # only update when the user changes the ACS input
    input_values$input_acs <- input$input_acs # create/update the ACS input value in our reactiveValues object
  })

  observeEvent(input$input_agency, {
    input_values$input_agency <- input$input_agency
  })

  observeEvent(input$input_distance, {
    input_values$input_distance <- input$input_distance # sum_CHOICE
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
# mod_summary_selections_ui("summary_selections_ui_1")

## To be copied in the server
# callModule(mod_summary_selections_server, "summary_selections_ui_1")
