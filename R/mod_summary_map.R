#' summary_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_summary_map_ui <- function(id) {
  ns <- NS(id)
  tagList()
}

#' summary_map Server Function
#'
#' @noRd
mod_summary_map_server <- function(input, output, session) {
  ns <- session$ns
}

## To be copied in the UI
# mod_summary_map_ui("summary_map_ui_1")

## To be copied in the server
# callModule(mod_summary_map_server, "summary_map_ui_1")
