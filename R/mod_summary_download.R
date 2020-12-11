#' summary_download UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_summary_download_ui <- function(id) {
  ns <- NS(id)
  tagList()
}

#' summary_download Server Function
#'
#' @noRd
mod_summary_download_server <- function(input, output, session,
                                        summary_util) {
  ns <- session$ns
}

## To be copied in the UI
# mod_summary_download_ui("summary_download_ui_1")

## To be copied in the server
# callModule(mod_summary_download_server, "summary_download_ui_1")
