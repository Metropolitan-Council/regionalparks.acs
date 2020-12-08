#' summarystats UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_summarystats_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("colonnes"),label = "Choose some columns", choices = NULL, multiple = TRUE),
    tableOutput(ns("table"))
  )
}
    
#' summarystats Server Function
#'
#' @noRd 
mod_summarystats_server <- function(input, output, session, r){
  ns <- session$ns
  
  observe({
    colonnes <- names(r$dataset)
    updateSelectInput( session, "colonnes", choices = colonnes)
  })
  
  data <- reactive({
    req(input$colonnes)
    r$dataset[, input$colonnes]
  })
  output$table <- renderTable({
    head(data())
  })
  
}
    
## To be copied in the UI
# mod_summarystats_ui("summarystats_ui_1")
    
## To be copied in the server
# callModule(mod_summarystats_server, "summarystats_ui_1")
 
