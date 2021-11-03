#' summary_utils UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_summary_utils_ui <- function(id) {
  ns <- NS(id)
  tagList()
}

#' summary_utils Server Function
#'
#' @noRd
mod_summary_utils_server <- function(input,
                                     output,
                                     session,
                                     selected_vars) {
  ns <- session$ns

  make_table_buffer_data <- reactive({
    p <- name_helper %>%
      left_join(long_buffer_data, by = c("buffercode" = "ACS")) %>%
      # regionalparks.acs::long_buffer_data %>%
      dplyr::filter(
        # agency == "Anoka County"
        agency %in% selected_vars$input_agency
      ) %>%
      separate(
        name,
        into = c("name", "delete2"),
        sep = c("_")
      ) %>%
      mutate(acs_short = stringr::str_remove(popuplab, "% ")) %>%
      mutate(hover_text = stringr::str_wrap(
        paste0(
          "Approx. ",
          "<b>",
          value,
          "%",
          "</b>",
          " of the pop. within ",
          distance,
          " mile of ",
          name,
          " (",
          status,
          ")",
          " falls into the ",
          "<b>",
          acs_short,
          "</b>",
          " category"
        ),
        55
      ))
    return(p)
  })

  make_plot_buffer_data <- reactive({
    make_table_buffer_data() %>%
      dplyr::filter(
        type %in% selected_vars$input_type,
        distance == selected_vars$input_distance,
        status %in% selected_vars$input_status,
        acscode == selected_vars$input_acs
      ) %>%
      # t<- p %>%
      #  filter(
      #    type %in% c("Park" , "Trail"),
      #    distance == 1,
      #    status %in% c("Existing", "Planned",  "Search"))#,
      #    acscode == "ageunder15_percent"
      #  )
      mutate(name = str_replace_all(
        name,
        c(
          "Regional Park" = "RP",
          "Regional Trail" = "RT",
          "Park Reserve" = "PR",
          "Special Recreation Feature" = "SRF"
        )
      )) %>%
      mutate(
        name = forcats::fct_reorder(name, (value))
      )
  })

  make_facet_data <- reactive({
    name_helper %>%
      left_join(agency_avg, by = c("buffercode" = "ACS")) %>%
      # regionalparks.acs::agency_avg %>%
      filter(
        # agency == "Anoka County",
        # acscode == name_helper[[1,1]] #"age15_24_percent"
        agency %in% selected_vars$input_agency,
        acscode == selected_vars$input_acs
      ) %>%
      mutate(
        agency = forcats::fct_reorder(agency, (value))
      ) %>%
      mutate(acs_short = stringr::str_remove(popuplab, "% ")) %>%
      mutate(hover_text = stringr::str_wrap(
        paste0(
          "The average ",
          "<b>",
          acs_short,
          "</b>",
          " within ",
          "<b>",
          agency,
          "</b>",
          "'s jurisdiction is ",
          "<b>",
          value,
          "%",
          "</b>",
          "."
        ),
        50
      )) %>%
      mutate(
        level = "Agency average",
        type = "avg"
      ) %>%
      rename(name = agency) %>%
      bind_rows(make_plot_buffer_data() %>%
        mutate(level = "Unit values")) %>%
      pivot_wider(names_from = acscode, values_from = value) %>%
      rename(value = selected_vars$input_acs) %>%
      # rename(value = name_helper[[1,1]]) %>%
      mutate(hovtext = paste0("Approx. ", .$value, "% of pple within", .$distance, " mi are"))
  })



  vals <- reactiveValues()

  observe({
    vals$table_buffer_data <- make_table_buffer_data()
  })

  observe({
    vals$plot_buffer_data <- make_plot_buffer_data()
  })

  observe({
    vals$facet_data <- make_facet_data()
  })

  return(vals)
}

## To be copied in the UI
# mod_summary_utils_ui("summary_utils_ui_1")

## To be copied in the server
# callModule(mod_summary_utils_server, "summary_utils_ui_1")
