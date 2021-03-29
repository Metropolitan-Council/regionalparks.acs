#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # List the first level callModules here
  tract_data <- callModule(mod_input_demos_server, "input_demos_ui_1")
  callModule(mod_leaflet_server, "leaflet_ui_1", tract_data)

  showModal(modalDialog(
    title = "Major update!",
    "We are delighted to announce that there is a major update to this tool. ",
    tags$br(),
    "Please update your bookmark and go to the new app.",
    tags$br(),
    tags$a(
      href = "https://metrotransitmn.shinyapps.io/regional-parks-equity-tool/",
      "https://metrotransitmn.shinyapps.io/regional-parks-equity-tool/"
    ),
    size = "l",
    footer = NULL
  ))
}
