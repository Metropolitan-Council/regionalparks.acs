#' selections_acs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_selections_acs_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(
      ns("input_acs"),
      label = h5("ACS variable"),
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
        `National origin` = list(
          "Origin, % foreign-born" = "adj_forborn_per",
          "Origin, % US-born" = "adj_usborn_per"
        ),
        `Ability` = list(
          "Ability, % any disability" = "adj_anydis_per"
        ),
        `Income` = list("Mean household income ($)" = "adj_meanhhi"),
        `Transportation` = list("% Housholds without a vehicle" = "adj_novehicle_per"),
        `Language` = list(
          "% speaking English less than very well" = "adj_lep_per",
          "% Spanish speakers" = "adj_span_per"
        )
      ),
      selected = "adj_ageunder15_per", selectize = F
    )
  )
}

#' selections_acs Server Function
#'
#' @noRd
mod_selections_acs_server <- function(input, output, session) {
  ns <- session$ns

  input_values <- reactiveValues() # start with an empty reactiveValues object.

  observeEvent(input$input_acs, { # only update when the user changes the ACS input
    input_values$input_acs <- input$input_acs # create/update the ACS input value in our reactiveValues object
  })

  return(input_values)
}

## To be copied in the UI
# mod_selections_acs_ui("selections_acs_ui_1")

## To be copied in the server
# callModule(mod_selections_acs_server, "selections_acs_ui_1")
