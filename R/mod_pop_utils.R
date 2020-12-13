#' pop_utils UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_pop_utils_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' pop_utils Server Function
#'
#' @noRd 
mod_pop_utils_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_pop_utils_ui("pop_utils_ui_1")
    
## To be copied in the server
# callModule(mod_pop_utils_server, "pop_utils_ui_1")
 
