#' summary_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_summary_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' summary_plot Server Function
#'
#' @noRd 
mod_summary_plot_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_summary_plot_ui("summary_plot_ui_1")
    
## To be copied in the server
# callModule(mod_summary_plot_server, "summary_plot_ui_1")
 
