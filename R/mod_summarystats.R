#' summarystats UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_summarystats_ui <- function(id){
  ns <- NS(id)
  tagList(
    verbatimTextOutput(ns("print"))
    
  )
}
    
#' summarystats Server Function
#'
#' @noRd 
mod_summarystats_server <- function(id, r) {
  moduleServer(
    id,
    function(input, output, session) {
      # We evaluate the reactiveValue element modified in the 
      # first module
      output$print <- renderPrint({
        r$number_from_first_mod
      })
    }
  )
}
    
## To be copied in the UI
# mod_summarystats_ui("summarystats_ui_1")
    
## To be copied in the server
# callModule(mod_summarystats_server, "summarystats_ui_1")
 
