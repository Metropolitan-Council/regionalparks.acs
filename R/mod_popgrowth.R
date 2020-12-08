#' popgrowth UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_popgrowth_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' popgrowth Server Function
#'
#' @noRd 
mod_popgrowth_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_popgrowth_ui("popgrowth_ui_1")
    
## To be copied in the server
# callModule(mod_popgrowth_server, "popgrowth_ui_1")
 
