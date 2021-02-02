
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


  # # Introduction/Notes tab -----------------------------------------------------------
  callModule(mod_intro_server, "intro_ui_1")

  callModule(mod_notes_server, "notes_ui_1")


  # ACS Map tab ----------------------------------------------------------------
  main_lft_inputs <- callModule(mod_leaflet_sidebar_server, "leaflet_sidebar_ui_1")
  observe({
    print(main_lft_inputs$source)
  })

  callModule(mod_main_leaflet_server, "main_leaflet_ui_1",
    main_lft_inputs,
    current_tab = input$nav
  )

  # ACS Summary tab ------------------------------------------------------------
  ## get input values
  selected_input_vars <- callModule(mod_summary_selections_server, "summary_selections_ui_1")

  ## run reactive calculations with input values
  summary_util_vars <- callModule(mod_summary_utils_server, "summary_utils_ui_1",
    selected_vars = selected_input_vars
  )

  # callModule(mod_summary_table_server, "summary_table_ui_1",
  #   selected_vars = selected_input_vars,
  #   summary_util = summary_util_vars
  # )

  callModule(mod_summary_download_server, "summary_download_ui_1",
    selected_vars = selected_input_vars,
    summary_util = summary_util_vars
  )

  callModule(mod_summary_ggplot_server, "mod_summary_ggplot_ui_1",
    selected_vars = selected_input_vars,
    summary_util = summary_util_vars
  )

  # callModule(mod_summary_map_server, "summary_map_ui_1",
  #   selected_vars = selected_input_vars,
  #   summary_util = summary_util_vars,
  #   current_tab = input$nav,
  #   current_sub_tab = input$summary_sub_tab
  # )

  # # Population growth tab ------------------------------------------------------
  #
  # # get input values
  # selected_population_vars <- callModule(mod_pop_selections_server, "pop_selections_ui_1") # callModule(mod_selections_population_server, "selections_population_ui_1")
  #
  # summary_util_popvars <- callModule(mod_pop_utils_server, "pop_utils_ui_1",
  #   selected_population = selected_population_vars
  # )
  #
  # callModule(mod_pop_map_server, "pop_map_ui_1",
  #   selected_popvars = selected_population_vars,
  #   summary_poputil = summary_util_popvars,
  #   current_tab = input$nav
  # )
  #
  # callModule(mod_pop_demoshifts_server, "pop_demoshifts_ui_1")
}
