
#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # List the first level callModules here

  # ACS Summary tab ------------------------------------------------------------

  ## get input values
  selected_input_vars <- callModule(mod_summary_selections_server, "summary_selections_ui_1")

  ## run reactive calculations with input values
  summary_util_vars <- callModule(mod_summary_utils_server, "summary_utils_ui_1",
    selected_vars = selected_input_vars
  )


  callModule(mod_summary_table_server, "summary_table_ui_1",
    selected_vars = selected_input_vars,
    summary_util = summary_util_vars
  )

  callModule(mod_summary_plot_server, "summary_plot_ui_1",
             selected_vars = selected_input_vars,
             summary_util = summary_util_vars)

  callModule(mod_summary_map_server, "summary_map_ui_1",
             summary_util = summary_util_vars)

  # ended up not using this for the time being, all iwthin the table_ui
  # callModule(mod_summary_download_server, "summary_download_ui_1",
  #            summary_util = summary_util_vars)
  
  callModule(mod_summary_raw_server, "summary_raw_ui_1")


  # ACS Map tab ----------------------------------------------------------------
  tract_data <- callModule(mod_input_demos_server, "input_demos_ui_1")
  callModule(mod_leaflet_server, "leaflet_ui_1", tract_data)

  # Introduction tab -----------------------------------------------------------
  callModule(mod_intro_server, "intro_ui_1")

  # Population growth tab ------------------------------------------------------

  # callModule(mod_popgrowth_server, "popgrowth_ui_1")#, r = r)

  # data_test <- callModule(mod_gendata_server, "gendata_ui_1")
  # callModule(mod_passcombo_server, "passcombo_ui_1", data_test)

  callModule(mod_combo_server, "combo_ui_1")

  # callModule(mod_accept_gfilter_server, "accept_gfilter_ui_1", filtered_df2)
}
