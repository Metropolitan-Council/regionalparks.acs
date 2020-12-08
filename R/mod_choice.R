#' choice UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_choice_ui <- function(id){
  ns <- NS(id)
  tagList(
    sliderInput(ns("choice"), "Choice", 1, 10, 5)
    
  )
}
    
#' choice Server Function
#'
#' @noRd 
choice_server <- function(id, r) {
  moduleServer(
    id,
    function(input, output, session, r) {
      # Whenever the choice changes, the value inside r is set
      observeEvent( input$choice , {
        r$number_from_first_mod <- input$choice
      })
      
    }
  )
}
    
## To be copied in the UI
# mod_choice_ui("choice_ui_1")
    
## To be copied in the server
# callModule(mod_choice_server, "choice_ui_1")
 
