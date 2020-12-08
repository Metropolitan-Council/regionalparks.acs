#' choice UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import tibble
#' @import ggplot2
#' @import cowplot
#' @import plotly
#' @import tidyr
#' @import stringr
#' @import forcats
#' @import dplyr
mod_choice_ui <- function(id){
  ns <- NS(id)
  tagList(
 
    fluidPage(
      fluidRow(
        column("",
               width = 10, offset = 1,
                 selectInput(
                   "agency",
                   label = h4("Agenc(y/ies)"),
                   choices = c(
                     "Anoka County",
                     "Bloomington",
                     "Carver County",
                     "Dakota County",
                     "MPRB",
                     "Ramsey County",
                     "Scott County",
                     "St. Paul",
                     "Three Rivers",
                     "Washington County"
                   ),
                   selected = "Anoka County",
                   multiple = TRUE
                 ),
                 radioButtons(
                   "distance",
                   label = h4("Buffer dist. (mi)"),
                   choices = c(1.0, 1.5, 3),
                   selected = c(1.0)
                 ))
      )
    )
    
  )
}
    
#' choice Server Function
#'
#' @noRd 
mod_choice_server <- function(input, output, session){
  ns <- session$ns
 
  filterData = reactiveVal(long_buffer_data)
  
  filtered_df <- reactive({
    res <- filterData() %>% filter(agency %in% input$agency)
    res <- res %>% filter(distance == input$distance)
    res
    
  })
  
}
    
## To be copied in the UI
# mod_choice_ui("choice_ui_1")
    
## To be copied in the server
# callModule(mod_choice_server, "choice_ui_1")
 
