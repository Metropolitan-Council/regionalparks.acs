
#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # List the first level callModules here
  # note: color assignment for parks/trails by status (existing, search, planned) is within golem_utils_server.R file

  # observe({
  #   print(input$nav)
  #   print(input$summary_sub_tab)
  # })

  observe_helpers(help_dir = "inst/app/www")

  # # Introduction/Notes tab -----------------------------------------------------------
  callModule(mod_intro_server, "intro_ui_1")

  callModule(mod_notes_server, "notes_ui_1")


  # ACS Map tab ----------------------------------------------------------------
  main_lft_inputs <- callModule(mod_leaflet_sidebar_server, "leaflet_sidebar_ui_1")

  callModule(
    mod_main_leaflet_server,
    "main_leaflet_ui_1",
    main_lft_inputs,
    current_tab = input$nav
  )

  # ACS Summary tab ------------------------------------------------------------
  ## get input values

  selected_input_vars <- callModule(mod_summary_selections_server, "summary_selections_ui_1")

  ## run reactive calculations with input values
  summary_util_vars <- callModule(
    mod_summary_utils_server,
    "summary_utils_ui_1",
    selected_vars = selected_input_vars
  )
  
  print("input selections")
  observe({print(selected_input_vars$input_type)})
  observe({print(selected_input_vars$input_status)})
  observe({print(selected_input_vars$input_acs)})
  observe({print(selected_input_vars$input_distance)})

  print("table_buffer_data")
  observe({print(summary_util_vars$table_buffer_data)})
 
  # observe({print(summary_util_vars$plot_buffer_data)}) 
  
  
  # print("facet data")
  # observe({print(summary_util_vars$facet_data)})

  callModule(
    mod_summary_download_server,
    "summary_download_ui_1",
    selected_vars = selected_input_vars,
    summary_util = summary_util_vars
  )

  callModule(
    mod_summary_ggplot_server,
    "mod_summary_ggplot_ui_1",
    selected_vars = selected_input_vars,
    summary_util = summary_util_vars
  )

}
