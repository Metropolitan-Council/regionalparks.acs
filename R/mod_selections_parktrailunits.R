#' selections_parktrailunits UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_selections_parktrailunits_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' selections_parktrailunits Server Function
#'
#' @noRd 
mod_selections_parktrailunits_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_selections_parktrailunits_ui("selections_parktrailunits_ui_1")
    
## To be copied in the server
# callModule(mod_selections_parktrailunits_server, "selections_parktrailunits_ui_1")
 
