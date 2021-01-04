#' summary_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_summary_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        shiny::p("Cells show the average weighted value of all ACS variables for the park and/or trail units chosen with the above selections. These data are available for download (but please understand that these data are subject to periodic refinements with model updates and new data releases)."),
        width = 12
      ),

      column(downloadButton(outputID = ns("downloadData"), "Download tabular data"), width = 6),
      HTML("<br>"),
      hr(),

      column(DT::dataTableOutput(outputId = ns("output_datatable")), width = 12)
    )
  )
}

#' summary_table Server Function
#'
#' @noRd
mod_summary_table_server <- function(input, output, session,
                                     selected_vars,
                                     summary_util) {
  ns <- session$ns

  output$output_datatable <- DT::renderDataTable(
    DT::datatable(
      data = (summary_util$table_buffer_data %>%
        left_join(recodeadjtable) %>%
        select(-ACS) %>%
        rename(ACS = nicename) %>%
        filter(!is.na(ACS)) %>%
        mutate(value = round(value, 1)) %>%
        select(agency, name, type, status, distance, ACS, value) %>%
        pivot_wider(names_from = ACS, values_from = value) %>%
        separate(name,
          into = c("name", "delete2"),
          sep = c("_")
        ) %>%
        select(-delete2, -Population) %>%
        mutate(name = str_replace_all(
          name,
          c(
            "Regional Park" = "RP",
            "Regional Trail" = "RT",
            "Park Reserve" = "PR"
          )
        )) %>%
        rename(
          Agency = agency,
          Name = name,
          Type = type,
          Status = status,
          `Buffer Dist.` = distance
        )
      ),
      options = list(scrollX = 500)
    )
  )


  output$downloadData <- downloadHandler(
    filename = paste0("ParksACS_", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(summary_util$table_buffer_data %>%
        left_join(recodeadjtable) %>%
        select(-ACS) %>%
        rename(ACS = nicename) %>%
        filter(!is.na(ACS)) %>%
        mutate(value = round(value, 1)) %>%
        select(agency, name, type, status, distance, ACS, value) %>%
        pivot_wider(names_from = ACS, values_from = value) %>%
        separate(name,
          into = c("name", "delete2"),
          sep = c("_")
        ) %>%
        select(-delete2, -Population) %>%
        mutate(name = str_replace_all(
          name,
          c(
            "Regional Park" = "RP",
            "Regional Trail" = "RT",
            "Park Reserve" = "PR"
          )
        )) %>%
        rename(
          Agency = agency,
          Name = name,
          Type = type,
          Status = status,
          `Buffer Dist.` = distance
        ),
      file,
      row.names = FALSE
      )
    }
  )
}

## To be copied in the UI
# mod_summary_table_ui("summary_table_ui_1")

## To be copied in the server
# callModule(mod_summary_table_server, "summary_table_ui_1")
