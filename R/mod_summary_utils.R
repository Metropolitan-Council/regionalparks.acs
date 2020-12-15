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
    # browser()
    p <- regionalparks.acs::long_buffer_data %>%
      dplyr::filter(
        agency %in% selected_vars$input_agency,
        type %in% selected_vars$input_type,
        distance == selected_vars$input_distance,
        status %in% selected_vars$input_status
      )
    return(p)
  })

  # ee note, when trying to pass make_table_buffer_data to a "make_plot_buffer_data" value, error = no applicable method for 'filter_' applied to an object of class "c('reactiveExpr', 'reactive', 'function'). Which makes sense, so is there a way to pass a reactive df thru a secondary filter?
  make_plot_buffer_data <- reactive({
    p2 <- regionalparks.acs::long_buffer_data %>%
      dplyr::filter(
        agency %in% selected_vars$input_agency,
        type %in% selected_vars$input_type,
        distance == selected_vars$input_distance,
        status %in% selected_vars$input_status,
        ACS == selected_vars$input_acs
      )
    return(p2)
  })


  make_plotly_buffer_data <- reactive({
    make_plot_buffer_data() %>%
      separate(
        name,
        into = c("name", "delete2"),
        sep = c("_")
      ) %>%
      mutate(name = str_replace_all(
        name,
        c(
          "Regional Park" = "RP",
          "Regional Trail" = "RT",
          "Park Reserve" = "PR"
        )
      )) %>%
      mutate(
        name = forcats::fct_reorder(name, desc(value)),
        concat = paste(type, status, sep = "_")
      ) %>%
      left_join(renamekey, by = c("ACS" = "ACS variable")) %>%
      mutate(acs_short = stringr::str_remove(goodname, "% ")) %>%
      mutate(hover_text = stringr::str_wrap(paste0(
        "Approximatley ",
        "<b>", value, "%", "</b>", " of the population within ",
        "<b>", distance, " mile", "</b>",
        " falls into the ",
        "<b>", acs_short, "</b>",
        " category"
      ), 55))
  })

  make_plotly_agency_data <- reactive({
    agency_planned_existing_avgs %>%
      filter(
        # agency %in% selected_vars$input_agency,
        # type %in% selected_vars$input_type,
        distance == selected_vars$input_distance,
        # status %in% selected_vars$input_status,
        ACS == selected_vars$input_acs
      ) %>%
      left_join(renamekey, by = c("ACS" = "ACS variable")) %>%
      mutate(acs_short = stringr::str_remove(goodname, "% ")) %>%
      mutate(hover_text = stringr::str_wrap(paste0(
        "The average ",
        "<b>", acs_short, "</b>",
        " within ",
        "<b>", distance, " mile ", "</b>",
        "of all existing and planned units in ",
        "<b>", agency, " </b>",
        " is ",
        "<b>", avg, "%", "</b>",
        "."
      ), 50))
  })


  make_agencyavg_data <- reactive({
    p3 <- regionalparks.acs::agency_avg %>%
      dplyr::filter(
        agency %in% selected_vars$input_agency,
        ACS == selected_vars$input_acs
      )
    return(p3)
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

  # make_plot_rawbuffer_data <- reactive({
  #   p7 <- regionalparks.acs::long_buffer_data_raw %>%
  #     dplyr::filter(
  #       agency %in% selected_vars$input_agency,
  #       type %in% selected_vars$input_type,
  #       distance == selected_vars$input_distance,
  #       status %in% selected_vars$input_status,
  #       ACS == selected_vars$input_acs
  #     )
  #   return(p7)
  # })


  recode_bg_names <- tribble( # recode bg names ----
    ~block_group_name, ~acs_selection,
    "ageunder15_percent", "adj_ageunder15_per",
    "age15_24_percent", "adj_age15_24_per",
    "age25_64_percent", "adj_age25_64_per",
    "age65up_percent", "adj_age65up_per",
    "whitenh_percent", "adj_whitenh_per",
    "blacknh_percent", "adj_blacknh_per",
    "asiannh_percent", "adj_asiannh_per",
    "amindnh_percent", "adj_amindnh_per",
    "othermutltnh_percent", "adj_othermultinh",
    "hisppop_percent", "adj_hisppop_per",
    "nothisppop_percent", "adj_nothisppop_per",
    "meanhhinc", "adj_meanhhi",
    "novehicle_percent", "adj_novehicle_per",
    "poorenglish_percent", "adj_lep_per",
    "spanish_percent", "adj_span_per"
  )

  make_map_bg_data <- reactive({
    # p6 <- regionalparks.acs::bg_geo[selected_vars$input_acs]
    p6 <- regionalparks.acs::block_group %>%
      rename(
        "adj_ageunder15_per" = "ageunder15_percent",
        "adj_age15_24_per" = "age15_24_percent",
        "adj_age25_64_per" = "age25_64_percent",
        "adj_age65up_per" = "age65up_percent",
        "adj_whitenh_per" = "whitenh_percent",
        "adj_blacknh_per" = "blacknh_percent",
        "adj_asiannh_per" = "asiannh_percent",
        "adj_amindnh_per" = "amindnh_percent",
        "adj_othermultinh" = "othermutltnh_percent",
        "adj_hisppop_per" = "hisppop_percent",
        "adj_nothisppop_per" = "nothisppop_percent",
        "adj_meanhhi" = "meanhhinc",
        "adj_novehicle_per" = "novehicle_percent",
        "adj_lep_per" = "poorenglish_percent",
        "adj_span_per" = "spanish_percent"
      ) %>%
      select(selected_vars$input_acs)
    return(p6)
  })


  vals <- reactiveValues()

  observe({
    vals$table_buffer_data <- make_table_buffer_data()
  })

  observe({
    vals$plot_buffer_data <- make_plot_buffer_data()
  })

  observe({
    vals$agencyavg_data <- make_agencyavg_data()
  })

  observe({
    vals$map_parktrail_data <- make_map_parktrail_data()
  })

  observe({
    vals$map_buffer_data <- make_map_buffer_data()
  })

  observe({
    vals$plotly_buffer_data <- make_plotly_buffer_data()
  })

  observe({
    vals$plotly_agency_data <- make_plotly_agency_data()
  })

  # observe({
  #   vals$plot_rawbuffer_data <- make_plot_rawbuffer_data()
  # })

  observe({
    vals$map_bg_data <- make_map_bg_data()
  })

  return(vals)
}

## To be copied in the UI
# mod_summary_utils_ui("summary_utils_ui_1")

## To be copied in the server
# callModule(mod_summary_utils_server, "summary_utils_ui_1")
