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
        HTML("<p>Cells show the average weighted value of all ACS variables for the park and/or trail units chosen with the above selections. These data are available for download (but please understand that these data are subject to periodic refinements with model updates and new data releases).</p>"),
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

  # browser()

  ## recode tibble (consider placing in a utils script)  ----
  recodeadjtable <- tibble::tribble(
    ~ACS,
    ~nicename,
    "adj_2019pop",
    "Population",
    "adj_ageunder15_per",
    "% under age 15",
    "adj_age15_24_per",
    "% age 15-24",
    "adj_age25_64_per",
    "% age 25-64",
    "adj_age65up_per",
    "% age 65+",
    "adj_whitenh_per",
    "% White",
    "adj_blacknh_per",
    "% Black",
    "adj_asiannh_per",
    "% Asian",
    "adj_amindnh_per",
    "% Am. Indian",
    "adj_othermultinh_per",
    "% Other + Multi",
    "adj_hisppop_per",
    "% Hispanic",
    "adj_nothisppop_per",
    "% not-Hispanic",
    "adj_meanhhi",
    "Mean household income",
    "adj_novehicle_per",
    "% Housholds without a vehicle",
    "adj_lep_per",
    "% speaking English less than very well",
    "adj_span_per",
    "% Spanish speakers",
    "adj_anydis_per",
    "Ability, % any disability",
    "adj_usborn_per",
    "Origin, % US-born",
    "adj_forborn_per",
    "Origin, % foreign-born"
  )

  #
  # ee comment - would love to put all the aesthetic improvements into it's own df, and then pass to the render*(), but apparently this needs to be inside a reactive expression? Doens't seem to work to pass thru a reactiveValues() command (at least as I've attempted it)

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
