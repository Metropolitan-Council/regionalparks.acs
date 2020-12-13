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
  
  make_plot_rawbuffer_data <- reactive({
    p7 <- regionalparks.acs::long_buffer_data_raw %>%
      dplyr::filter(
        agency %in% selected_vars$input_agency,
        type %in% selected_vars$input_type,
        distance == selected_vars$input_distance,
        status %in% selected_vars$input_status,
        ACS == selected_vars$input_acs
      )
    return(p7)
  })
  
  
  make_map_bg_data <- reactive({
    p6 <- regionalparks.acs::bg_geo[selected_vars$input_acs]
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
