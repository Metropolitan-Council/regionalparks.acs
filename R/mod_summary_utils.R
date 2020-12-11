#' summary_utils UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_summary_utils_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' summary_utils Server Function
#'
#' @noRd 
mod_summary_utils_server <- function(input, output, session,
                                     selected_vars){
  ns <- session$ns
  
  make_table_buffer_data <- reactive({
    # browser()
    p <- regionalparks.acs::long_buffer_data %>%
      dplyr::filter(agency %in% selected_vars$input_agency)
    return(p)
  })
  
  
  vals <- reactiveValues()
  
  observe({
    vals$table_buffer_data <- make_table_buffer_data()
  })
  
  return(vals)
   
}
    
## To be copied in the UI
# mod_summary_utils_ui("summary_utils_ui_1")
    
## To be copied in the server
# callModule(mod_summary_utils_server, "summary_utils_ui_1")
 
