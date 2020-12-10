#' gendata UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_gendata_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      id=ns("testgendata"),
      column(
        width = 3,
        selectInput(
          ns("agency"),
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
          multiple = TRUE, selectize = T
        )),
      column(
        width = 2,
        radioButtons(
          ns("distance"),
          label = h4("Buffer dist. (mi)"),
          choices = c(1.0, 1.5, 3),
          selected = c(1.0)
        )),
      column(width = 2,
             checkboxGroupInput(
               ns("type"),
               label = h4("Type"),
               choices = c("Park", "Trail"),
               selected = c("Park", "Trail")
             )),
      column(width = 2,
             checkboxGroupInput(
               ns("status"),
               label = h4("Status"),
               choices = c("Existing", "Planned", "Search"),
               selected = c("Existing", "Planned", "Search")
             ))
    )
  )
}
    
#' gendata Server Function
#'
#' @noRd 
mod_gendata_server <- function(input, output, session){
  filterData4 = reactiveVal(long_buffer_data)
  filtered_df4<-reactive({
    long_buffer_data() %>% filter(ACS == input$ACS,
                                         agency %in% input$agency,
                                         distance == input$distance,
                                          type %in% input$type,
                                         status %in% input$status)
  })
  return(filtered_df4)
  
  # vals <- reactiveValues()
  # 
  # observeEvent(input$testgendata, {
  #   vals$selected_var <- input$testgendata
  # })
  # return(vals)
  
}
 
  
  # ns <- session$ns
  # 
  # filterData4 = reactiveVal(long_buffer_data)
  # filtered_df4 <- reactive({
  #   res <- filterData4() %>% filter(ACS == input$ACS,
  #                                   agency %in% input$agency,
  #                                   distance == input$distance,
  #                                   type %in% input$type,
  #                                   status %in% input$status)
  #   res
  # })
# }
    


## To be copied in the UI
# mod_gendata_ui("gendata_ui_1")
    
## To be copied in the server
# callModule(mod_gendata_server, "gendata_ui_1")
 
