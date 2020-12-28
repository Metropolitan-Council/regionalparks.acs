#' leaflet2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_leaflet2_ui <- function(id) {
  ns <- NS(id)
  tagList()
}

#' leaflet2 Server Function
#'
#' @noRd
mod_leaflet2_server <- function(input, output, session) {
  ns <- session$ns
}

## To be copied in the UI
# mod_leaflet2_ui("leaflet2_ui_1")

## To be copied in the server
# callModule(mod_leaflet2_server, "leaflet2_ui_1")
