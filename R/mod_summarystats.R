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
    
  )
}
    
#' summarystats Server Function
#'
#' @noRd 
mod_summarystats_server <- function(input, output, session){
  ns <- session$ns
  
}
    
## To be copied in the UI
# mod_summarystats_ui("summarystats_ui_1")
    
## To be copied in the server
# callModule(mod_summarystats_server, "summarystats_ui_1")
 
