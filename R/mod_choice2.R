#' choice2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_choice2_ui <- function(id){
  ns <- NS(id)
  tagList(
    id = ns("sum_CHOICE"),
    HTML('<p>Select 1 ACS variable from the drop-down menu and 1 or more Agency of interest. The buffer distances roughly map onto walking distance (0.5 mi), biking distance (1.5 mi), or some other distance (3 mi). The reactive points show the values from the American Community Survey (either weighted averages or raw data, depending on tab selection) within a given buffer zone around regional parks and trails.</p>'), hr(),
    fluidPage(
      fluidRow(
        column(
          width = 3,
          selectizeInput(
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
            selected = "adj_ageunder15_per"
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
            multiple = TRUE
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
  )
}
    
#' choice2 Server Function
#'
#' @noRd 
mod_choice2_server <- function(input, output, session){
  ns <- session$ns
 
  vals2 <- reactiveValues()
  observeEvent(input$sum_CHOICE, {
    vals2 <- long_buffer_data %>%
        filter(ACS == input$ACS,
               distance == input$distance,
               agency %in% input$agency,
               status %in% input$status,
               type %in% input$type)
    })
    return(vals2)
  }



## To be copied in the UI
# mod_choice2_ui("choice2_ui_1")
    
## To be copied in the server
# callModule(mod_choice2_server, "choice2_ui_1")
 
