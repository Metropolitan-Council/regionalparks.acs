#' pop_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_pop_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    dataTableOutput(outputId = ns("output_datatable"))
  )
}
    
#' pop_table Server Function
#'
#' @noRd 
mod_pop_table_server <- function(input, output, session,
                                 summary_poputil,
                                 selected_popvars){
  ns <- session$ns
 
  output$output_datatable <- DT::renderDataTable(
    DT::datatable(
      data = (summary_poputil$pop_data)))
}
    
## To be copied in the UI
# mod_pop_table_ui("pop_table_ui_1")
    
## To be copied in the server
# callModule(mod_pop_table_server, "pop_table_ui_1")
 
