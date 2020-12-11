#' summary_filtered UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_summary_filtered_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' summary_filtered Server Function
#'
#' @noRd 
mod_summary_filtered_server <- function(input, output, session, selected_vars){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_summary_filtered_ui("summary_filtered_ui_1")
    
## To be copied in the server
# callModule(mod_summary_filtered_server, "summary_filtered_ui_1")
 
