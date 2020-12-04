#' summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_summary_ui <- function(id){
  ns <- NS(id)
  tagList(
    tagList(
      shiny::div(
        id = "summary",
        includeMarkdown(system.file("app/www/reference_doc.md", package = "regionalparks.acs"))
      )
    )
    
 
  )
}
    
#' summary Server Function
#'
#' @noRd 
mod_summary_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_summary_ui("summary_ui_1")
    
## To be copied in the server
# callModule(mod_summary_server, "summary_ui_1")
 
