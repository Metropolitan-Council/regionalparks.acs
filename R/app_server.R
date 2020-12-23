
#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # List the first level callModules here

  # note: color assignment for parks/trails by status (existing, search, planned) is within golem_utils_server.R file

  observe({
    print(input$nav)
  })


  
  # observe({
  #   if(input$nav == "Summary") {print("yes")} else {print("no")}
  # })
  # 
  # observe({
  #   if(input$nav == "Summary" & input$summarytabs == "lflt") {print("yeslflt")} else {print("nolflt")}
  # })
  # 
  # # Introduction tab -----------------------------------------------------------
  callModule(mod_intro_server, "intro_ui_1")

  # ACS Map tab ----------------------------------------------------------------
  tract_data <- callModule(mod_input_demos_server, "input_demos_ui_1")
  callModule(mod_leaflet_server, "leaflet_ui_1", tract_data)


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
  )# callModule(mod_summary_ggplotly_server, "summary_ggplotly_ui_1")


  callModule(mod_summary_plot_server, "summary_plot_ui_1",
    selected_vars = selected_input_vars,
    summary_util = summary_util_vars
  )

  callModule(mod_mod_summary_ggplot_server, "mod_summary_ggplot_ui_1",
             selected_vars = selected_input_vars,
             summary_util = summary_util_vars
  )
  
  callModule(mod_summary_ggplotly_server, "summary_ggplotly_ui_1",
             selected_vars = selected_input_vars,
             summary_util = summary_util_vars)
  

  callModule(mod_summary_map_server, "summary_map_ui_1",
    selected_vars = selected_input_vars,
    summary_util = summary_util_vars
  )
# 
#   callModule(mod_summary_map2_server, "summary_map2_ui_1",
#              selected_vars = selected_input_vars,
#              summary_util = summary_util_vars)
  
  # ended up not using this for the time being, all within the table_ui
  # callModule(mod_summary_download_server, "summary_download_ui_1",
  #            summary_util = summary_util_vars)

  # callModule(mod_summary_raw_server, "summary_raw_ui_1",
  #            # selected_vars = selected_input_vars,
  #            summary_util = summary_util_vars)

  # Population growth tab ------------------------------------------------------

  # get input values
  selected_input_popvars <- callModule(mod_pop_selections_server, "pop_selections_ui_1")

  ## run reactive calculations with input values
  summary_util_popvars <- callModule(mod_pop_utils_server, "pop_utils_ui_1",
                                  selected_popvars = selected_input_popvars
  )

  callModule(mod_pop_map_server, "pop_map_ui_1",
             selected_popvars = selected_input_popvars,
             summary_poputil = summary_util_popvars)


  callModule(mod_pop_demoshifts_server, "pop_demoshifts_ui_1")

}
