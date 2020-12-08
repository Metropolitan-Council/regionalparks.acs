#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  
  r <- reactiveValues()
  choice_server("choice_ui_1", r = r)
  
  # List the first level callModules here
  callModule(mod_intro_server, "intro_ui_1")
  
  tract_data <- callModule(mod_input_demos_server, "input_demos_ui_1")
  callModule(mod_leaflet_server, "leaflet_ui_1", tract_data)
  
  callModule(mod_summarystats_server, "summarystats_ui_1", r = r)
  
  callModule(mod_popgrowth_server, "popgrowth_ui_1", r = r)
  
  # callModule(mod_notes_server, "notes_ui_1") #this was never added?
  
}
