#' summary_selections UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_summary_selections_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # id = ns("sum_CHOICE"),
    h3("Select inputs "),
    # fluidPage(
    selectInput(
      ns("input_acs"),
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
        `Language` = list(
          "% speaking English less than very well" = "adj_lep_per",
          "% Spanish speakers" = "adj_span_per"
        )
      ),
      selected = "adj_ageunder15_per", selectize = F
    ),

    selectInput(
      ns("input_agency"),
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
    ),
    radioButtons(
      ns("input_distance"),
      label = h4("Buffer dist. (mi)"),
      choices = c(1.0, 1.5, 3),
      selected = c(1.0)
    ),
    #       HTML("<div id='summary_selections_ui_1-sum_CHOICE' class='form-group shiny-input-radiogroup shiny-input-container shiny-bound-input'>
    #               <label class='control-label' for='summary_selections_ui_1-sum_CHOICE'><div></div></label>
    #               <div class='shiny-options-group'>
    #                    <div>
    # <h5>Population</h5>
    # </div>
    # <div class='radio'>
    #                   <label>
    #                     <input type='radio' name='summary_selections_ui_1-sum_CHOICE' value='1' checked='checked'>
    #                     <span>1 mile</span>
    #                   </label>
    #                 </div>
    #  <div class='radio'>
    #                   <label>
    #                     <input type='radio' name='summary_selections_ui_1-sum_CHOICE' value='1.5'>
    #                     <span>1.5 mile</span>
    #                   </label>
    #                 </div>
    #                 <div class='radio'>
    #                   <label>
    #                     <input type='radio' name='summary_selections_ui_1-sum_CHOICE' value='3'>
    #                     <span>3</span>
    #                   </label>
    #                 </div>
    #            </div>
    #            </div>"),
    checkboxGroupInput(
      ns("input_type"),
      label = h4("Type"),
      choices = c("Park", "Trail"),
      selected = c("Park", "Trail")
    ),
    checkboxGroupInput(
      ns("input_status"),
      label = h4("Status"),
      choices = c("Existing", "Planned", "Search"), # HTML("<strong>Existing</strong>") #something like this doesn't exist, but if these text colors could match point colors that would be great!
      selected = c("Existing", "Planned", "Search")
    )
  )
}

#' summary_selections Server Function
#'
#' @noRd
mod_summary_selections_server <- function(input, output, session) {
  ns <- session$ns


  input_values <- reactiveValues() # start with an empty reactiveValues object.

  observeEvent(input$input_acs, { # only update when the user changes the ACS input
    input_values$input_acs <- input$input_acs # create/update the ACS input value in our reactiveValues object
  })

  observeEvent(input$input_agency, {
    input_values$input_agency <- input$input_agency
  })

  observeEvent(input$input_distance, {
    input_values$input_distance <- input$input_distance # sum_CHOICE
  })

  observeEvent(input$input_type, {
    input_values$input_type <- input$input_type
  })

  observeEvent(input$input_status, {
    input_values$input_status <- input$input_status
  })



  return(input_values)
}

## To be copied in the UI
# mod_summary_selections_ui("summary_selections_ui_1")

## To be copied in the server
# callModule(mod_summary_selections_server, "summary_selections_ui_1")
