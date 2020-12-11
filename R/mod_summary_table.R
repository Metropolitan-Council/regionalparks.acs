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

    HTML('<p>Cells show the average weighted value for all ACS variables for each park and/or trail selected with the inputs above. These data are available for download.</p>'),
    # downloadButton(ns("downloadData"), "Download tabular data"),
    hr(),
    # uiOutput(ns("test")), # if I run this, for some reason the whole ui from the mod_summary_selections downloads
    # dataTableOutput(ns("datatable")),
    # dataTableOutput(ns("datatable2"))
    

  )
}
    
#' summary_table Server Function
#'
#' @noRd 
mod_summary_table_server <- function(input, output, session, 
                                     selected_vars, 
                                     # df1, 
                                     long_buffer_data){
  ns <- session$ns
  
  # browser()
  
  output$test <- renderUI(selected_vars)

  react_df <- reactive({
    p <- long_buffer_data %>%
      filter(agency == selected_vars$agency())
    return(p)
  })
  
  output$datatable2 <- renderDataTable({
    react_df()
  })
  
  # output$datatable <- renderDataTable({
  #   long_buffer_data  %>%
  #     filter(type == selected_vars$type())
  # })

  output$datatable <- renderDataTable({
    long_buffer_data  %>%
      filter(type == selected_vars$type)
  })
  
  output$downloadData <- downloadHandler()
  
  }
    
## To be copied in the UI
# mod_summary_table_ui("summary_table_ui_1")
    
## To be copied in the server
# callModule(mod_summary_table_server, "summary_table_ui_1")
 
