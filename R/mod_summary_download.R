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
  tagList(
    downloadButton(outputId = ns("download_button"), label =  "Download tabular data")
  )
}

#' summary_download Server Function
#'
#' @noRd
#' @importFrom utils write.csv
mod_summary_download_server <- function(input, output, session,
                                        selected_vars,
                                        summary_util) {
  ns <- session$ns
  browser()
  
  output$download_button <- downloadHandler(
    filename = paste0("ParksACS_", Sys.Date(), ".csv"),
    contentType = "text/csv",
    content = function(con) {
      utils::write.csv(
        x = 
          summary_util$table_buffer_data %>%
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
          select(-delete2) %>% # , -Population) %>%
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
        file = con,
        row.names = FALSE
      )
    }
  )
}

## To be copied in the UI
# mod_summary_download_ui("summary_download_ui_1")

## To be copied in the server
# callModule(mod_summary_download_server, "summary_download_ui_1")
