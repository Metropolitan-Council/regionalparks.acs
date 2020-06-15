#' notes UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_notes_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::wellPanel(
      includeMarkdown(system.file("app/www/reference_doc.md", package = "regionalparks.acs")),
      # p(id = 'update', em("0.0.8. Last updated",
      #                     max(file.info('ui.R')$mtime,
      #                         file.info('server.R')$mtime,
      #                         file.info('global.R')$mtime),
      #                     " by ", tags$a(href = 'mailto:liz.roten@metc.state.mn.us', 'Liz Roten' )))
    )
  )
}

#' notes Server Function
#'
#' @noRd
mod_notes_server <- function(input, output, session) {
  ns <- session$ns
}

## To be copied in the UI
# mod_notes_ui("notes_ui_1")

## To be copied in the server
# callModule(mod_notes_server, "notes_ui_1")
