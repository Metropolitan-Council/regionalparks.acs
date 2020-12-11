#' summary_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_summary_table_ui <- function(id){
  ns <- NS(id)
  tagList(
 
    dataTableOutput(ns("datatable")),
    dataTableOutput(ns("datatable2"))
    

  )
}
    
#' summary_table Server Function
#'
#' @noRd 
mod_summary_table_server <- function(input, output, session, 
                                     selected_vars, df1, long_buffer_data){
  ns <- session$ns
  
  browser()
  
  test <- selected_vars

  react_df <- reactive({
    p <- long_buffer_data %>%
      filter(agency == selected_vars$select_agency())
    return(p)
  })
  
  output$datatable2 <- renderDataTable({
    react_df()
  })
  
  output$datatable <- renderDataTable({
    long_buffer_data  %>%
      filter(type == selected_vars$select_type())
  })

  }
    
## To be copied in the UI
# mod_summary_table_ui("summary_table_ui_1")
    
## To be copied in the server
# callModule(mod_summary_table_server, "summary_table_ui_1")
 
