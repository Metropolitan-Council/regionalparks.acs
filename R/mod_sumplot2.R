#' sumplot2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sumplot2_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' sumplot2 Server Function
#'
#' @noRd 
mod_sumplot2_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_sumplot2_ui("sumplot2_ui_1")
    
## To be copied in the server
# callModule(mod_sumplot2_server, "sumplot2_ui_1")
 
