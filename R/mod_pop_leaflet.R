#' pop_leaflet UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_pop_leaflet_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' pop_leaflet Server Function
#'
#' @noRd 
mod_pop_leaflet_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_pop_leaflet_ui("pop_leaflet_ui_1")
    
## To be copied in the server
# callModule(mod_pop_leaflet_server, "pop_leaflet_ui_1")
 
