#' summary_selections UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_summary_selections_ui <- function(id){
  ns <- NS(id)
  tagList(
    hr(),
    # id = ns("sum_CHOICE"),
    h3("Select inputs: "),
    # fluidPage(
      fluidRow(
        column(
          width = 3,
          selectInput(
            ns("ACS"),
            label = h4("ACS variable"),
            choices = list(
              `Age` = list(
                "Age, % under 15" = "adj_ageunder15_per",
                "Age, % 15-24" = "adj_age15_24_per",
                "Age, % 25-64" = "adj_age25_64_per",
                "Age, % 65+" = "adj_age65up_per"
              ),
              `Race` = list(
                "Race, % Am. Indian" = "adj_amindnh_per",
                "Race, % Asian" = "adj_asiannh_per",
                "Race, % Black" = "adj_blacknh_per",
                "Race, % Other + Multi" = "adj_othermultinh_per",
                "Race, % White" = "adj_whitenh_per"
              ),
              `Ethnicity` = list(
                "Ethnicity, % Hispanic" = "adj_hisppop_per",
                "Ethnicity, % not-Hispanic" = "adj_nothisppop_per"
              ),
              `Income` = list("Mean household income ($)" = "adj_meanhhi"),
              `Transportation` = list("% Housholds without a vehicle" = "adj_novehicle_per"),
              `Language` = list("% speaking English less than very well" = "adj_lep_per",
                                "% Spanish speakers" = "adj_span_per")
            ),
            selected = "adj_ageunder15_per", selectize = F
          )),
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
      ),
    hr(),
    h3("View data: ")
  )
}
    
#' summary_selections Server Function
#'
#' @noRd 
mod_summary_selections_server <- function(input, output, session){
  ns <- session$ns
 
  input_values <- reactiveValues() # start with an empty reactiveValues object. 
  
  observeEvent(input$ACS, { # only update when the user changes the ACS input
    input_values$ACS <- input$ACS # create/update the ACS input value in our reactiveValues object
  })
  
  observeEvent(input$agency, {
    input_values$agency <- input$agency
  })
  
  observeEvent(input$distance, {
    input_values$distance <- input$distance
  })
  
  observeEvent(input$type, {
    input_values$type <- input$type
  })
  
  observeEvent(input$status, {
    input_values$status <- input$status
  })
  
  return(input_values) # return the entire reactiveValues object

}
    
## To be copied in the UI
# mod_summary_selections_ui("summary_selections_ui_1")
    
## To be copied in the server
# callModule(mod_summary_selections_server, "summary_selections_ui_1")
 
