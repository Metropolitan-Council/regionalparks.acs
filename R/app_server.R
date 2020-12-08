
#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # List the first level callModules here
  
  callModule(mod_choice_server, "choice_ui_1")
  
  filterData <- reactiveVal(long_buffer_data)
  
  filtered_df <- reactive({
    res <- filterData() %>% filter(agency %in% input$agency)
    res <- res %>% filter(distance == input$distance)
    res
  })
  
  callModule(mod_intro_server, "intro_ui_1")
  
  tract_data <- callModule(mod_input_demos_server, "input_demos_ui_1")
  callModule(mod_leaflet_server, "leaflet_ui_1", tract_data)
  
  callModule(mod_summarystats_server, "summarystats_ui_1",  filtered_df = filtered_df)
  
  callModule(mod_popgrowth_server, "popgrowth_ui_1")#, r = r)
}

