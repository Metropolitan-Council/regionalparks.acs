#' accept_gfilter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_accept_gfilter_ui <- function(id){
  ns <- NS(id)
  tagList(
    dataTableOutput(ns("datatable"))
    
  )
}
    
#' accept_gfilter Server Function
#'
#' @noRd 
mod_accept_gfilter_server <- function(input, output, session, filtered_df2 = filtered_df2){
  ns <- session$ns
  output$datatable <- renderDataTable({
    filtered_df2()
  })
}
    
## To be copied in the UI
# mod_accept_gfilter_ui("accept_gfilter_ui_1")
    
## To be copied in the server
# callModule(mod_accept_gfilter_server, "accept_gfilter_ui_1")
 
