#' passcombo UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_passcombo_ui <- function(id){
  ns <- NS(id)
  tagList(
    head("datatable"),
    dataTableOutput(ns("datatable"))
  )
}
    
#' passcombo Server Function
#'
#' @noRd 
mod_passcombo_server <- function(input, output, session, data_test = data_test){
  ns <- session$ns
  output$datatable <- renderDataTable({
    data_test()
  })
}
    
## To be copied in the UI
# mod_passcombo_ui("passcombo_ui_1")
    
## To be copied in the server
# callModule(mod_passcombo_server, "passcombo_ui_1")
 
