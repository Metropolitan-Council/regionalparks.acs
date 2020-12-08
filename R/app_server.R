
#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # List the first level callModules here
  
  r <- reactiveValues()
  
  observe({
    r$dataset <- switch(input$dataset,
                        "rock" = rock,
                        "pressure" = pressure,
                        "cars" = cars)
  })
  
  callModule(mod_intro_server, "intro_ui_1")
  
  tract_data <- callModule(mod_input_demos_server, "input_demos_ui_1")
  callModule(mod_leaflet_server, "leaflet_ui_1", tract_data)
  
  callModule(mod_summarystats_server, "summarystats_ui_1", r = r)
  
  callModule(mod_popgrowth_server, "popgrowth_ui_1")#, r = r)
}

