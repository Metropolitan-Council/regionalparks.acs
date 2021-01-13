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
mod_summary_utils_server <- function(input, output, session,
                                     selected_vars) {
  ns <- session$ns

  make_table_buffer_data <- reactive({
    p <- regionalparks.acs::long_buffer_data %>%
      dplyr::filter(
        agency %in% selected_vars$input_agency,
        type %in% selected_vars$input_type,
        distance == selected_vars$input_distance,
        status %in% selected_vars$input_status
      ) %>%
      separate(
        name,
        into = c("name", "delete2"),
        sep = c("_")
      ) %>%
      left_join(renamekey, by = c("ACS" = "ACS")) %>%
      mutate(acs_short = stringr::str_remove(goodname, "% ")) %>%
      mutate(hover_text = stringr::str_wrap(paste0(
        "Approx. ",
        "<b>", value, "%", "</b>", " of the pop. within ",
        distance, " mile of ",
        name, " (", status, ")", " falls into the ",
        "<b>", acs_short, "</b>",
        " category"
      ), 55))
    return(p)
  })


  make_table_buffer_data_display <- reactive({
    make_table_buffer_data() %>%
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
      )
  })


  make_plot_buffer_data <- reactive({
    make_table_buffer_data() %>%
      dplyr::filter(
        ACS == selected_vars$input_acs
      ) %>%
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
    regionalparks.acs::agency_avg %>%
      filter(
        agency %in% selected_vars$input_agency,
        ACS == selected_vars$input_acs
      ) %>%
      mutate(
        agency = forcats::fct_reorder(agency, (value))
      ) %>%
      left_join(renamekey, by = c("ACS" = "ACS")) %>%
      mutate(acs_short = stringr::str_remove(goodname, "% ")) %>%
      mutate(hover_text = stringr::str_wrap(paste0(
        "The average ",
        "<b>", acs_short, "</b>",
        " within ",
        "<b>", agency, "</b>",
        "'s jurisdiction is ",
        "<b>", value, "%", "</b>",
        "."
      ), 50)) %>%
      mutate(
        level = "Agency avg.",
        type = "avg"
      ) %>%
      rename(name = agency) %>%
      bind_rows(make_plot_buffer_data() %>%
        mutate(level = "Unit values")) %>%
      pivot_wider(names_from = ACS, values_from = value) %>%
      rename(value = starts_with("adj")) %>%
      mutate(hovtext = paste0("Approx. ", .$value, "% of pple within", .$distance, " mi are"))
  })

  make_map_parktrail_data <- reactive({
    p4 <- regionalparks.acs::park_trail_geog_LONG %>%
      dplyr::filter(
        agency %in% selected_vars$input_agency,
        Type %in% selected_vars$input_type,
        status2 %in% selected_vars$input_status
      )
    return(p4)
  })


  make_map_buffer_data <- reactive({
    p5 <- regionalparks.acs::buffer_geo %>%
      dplyr::filter(
        agency %in% selected_vars$input_agency,
        type %in% selected_vars$input_type,
        status %in% selected_vars$input_status,
        distance == selected_vars$input_distance
      )
    return(p5)
  })


  make_map_bg_data <- reactive({
    p6 <- if (selected_vars$input_acs %in% tract_vars$ACS) {
      regionalparks.acs::census_tract_map %>%
        select(selected_vars$input_acs)
    } else {
      regionalparks.acs::block_group_map %>%
        select(selected_vars$input_acs)
    }
    return(p6)
  })
  
  
  make_PlotHeight <- reactive( # plot height ------
                          # #if want to set a minimum height
                          return(
                            if ((nrow(make_plot_buffer_data()[!duplicated(make_plot_buffer_data()[, c("name")]), ]) * 30) > 200) {
                              (nrow(make_plot_buffer_data()[!duplicated(make_plot_buffer_data()[, c("name")]), ]) * 30)
                            } else {
                              200
                            }
                          )
  )


  vals <- reactiveValues()

  observe({
    vals$table_buffer_data <- make_table_buffer_data()
  })

  observe({
    vals$plot_buffer_data <- make_plot_buffer_data()
  })

  observe({
    vals$map_parktrail_data <- make_map_parktrail_data()
  })

  observe({
    vals$map_buffer_data <- make_map_buffer_data()
  })

  observe({
    vals$map_bg_data <- make_map_bg_data()
  })

  observe({
    vals$facet_data <- make_facet_data()
  })

  # observe({
  #   vals$table_buffer_data_display <- make_table_buffer_data_display()
  # })

  
  observe({
    vals$sum_plotheight <- make_PlotHeight()
  })
  return(vals)
}

## To be copied in the UI
# mod_summary_utils_ui("summary_utils_ui_1")

## To be copied in the server
# callModule(mod_summary_utils_server, "summary_utils_ui_1")
