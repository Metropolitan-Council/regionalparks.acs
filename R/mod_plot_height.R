#' plot_height UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_height_ui <- function(id) {
  ns <- NS(id)
  tagList()
}

#' plot_height Server Function
#'
#' @noRd
mod_plot_height_server <- function(input, output, session) {
  ns <- session$ns
}

## To be copied in the UI
# mod_plot_height_ui("plot_height_ui_1")

## To be copied in the server
# callModule(mod_plot_height_server, "plot_height_ui_1")
