#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # List the first level callModules here
  tract_data <- callModule(mod_input_demos_server, "input_demos_ui_1")
  callModule(mod_leaflet_server, "leaflet_ui_1", tract_data)
  
  callModule(mod_intro_server, "intro_ui_1")
  
}
