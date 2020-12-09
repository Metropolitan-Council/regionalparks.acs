
#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # List the first level callModules here
  
  callModule(mod_choice_server, "choice_ui_1")
  
  filterData = reactiveVal(long_buffer_data)
  filtered_df <- reactive({
    res <- filterData() %>% filter(ACS == input$ACS,
                                   agency %in% input$agency,
                                   distance == input$distance,
                                   type %in% input$type,
                                   status %in% input$status)
    res
    
  })
  
  callModule(mod_intro_server, "intro_ui_1")

  callModule(mod_choice2_server, "choice2_ui_1")
  filterData2 = reactiveVal(long_buffer_data)
  filtered_df2 <- reactive({
    res2 <- filterData2() %>% filter(ACS == input$ACS2,
                                   agency %in% input$agency2,
                                   distance == input$distance2,
                                   type %in% input$type2,
                                   status %in% input$status2)
    res2
  })
  
  
  tract_data <- callModule(mod_input_demos_server, "input_demos_ui_1")
  callModule(mod_leaflet_server, "leaflet_ui_1", tract_data)
  
  callModule(mod_summarystats_server, "summarystats_ui_1",  filtered_df = filtered_df)
  
  callModule(mod_sumplot2_server, "sumplot2_ui_1", filtered_df2 = filtered_df2)
  
  callModule(mod_popgrowth_server, "popgrowth_ui_1")#, r = r)
}

