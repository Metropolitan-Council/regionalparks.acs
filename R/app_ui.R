##### Make sure files to make .rda are present (i've skipped a lot here)
#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    # List the first level UI elements here
    navbarPage("Regional Parks and Trails Equity Tool",
      id = "nav",


      # intro tab -----
      tabPanel(
        "Introduction",
        mod_intro_ui("intro_ui_1")
      ),

      ## map tab -----
      tabPanel(
        "Map",
        shiny::wellPanel(
          HTML("<p>Select 1 ACS variable. The tracts’ colors correspond to 2014-2018 (5-year) American Community Survey (ACS) demographic metric selected. Each demographic characteristic is shown as a percentage of the total population, with the exception of median household income, which is displayed in dollars. The darker the color, the higher the percentage (or income in dollars).</p>")
        ),

        sidebarPanel(
          mod_input_demos_ui(id = "input_demos_ui_1"),
        ),
        mainPanel(
          mod_leaflet_ui(id = "leaflet_ui_1")
        )
      ),

      # acs summary tab -----
      tabPanel(
        "Summary",
        sidebarPanel(mod_summary_selections_ui("summary_selections_ui_1")),
        mainPanel(
          HTML("<p>Data are summarized in several ways. The <em>Weighted averages</em> tab distills complex spatial demographic patterns into a single summary statistic, and will be appropriate for most uses. The <em>Buffer map</em> tab shows the spatial demographic patterns with the buffer analysis zones overlayed. The <em>Download tabular data</em> tab displays summary statistics for all ACS variables in tabular form.</p>"), # The <em>Methods and raw data</em> tab is intended for advanced users and illustrates the steps taken to aggregate demographic data into weighted averages
          tabsetPanel(
            selected = "Weighted averages",
            tabPanel(
              "Weighted averages",
              mod_summary_plot_ui("summary_plot_ui_1")
            ),
            tabPanel(
              "Buffer map",
              mod_summary_map_ui("summary_map_ui_1")
            ),
            tabPanel(
              "Download tabular data",
              mod_summary_download_ui("summary_download_ui_1"),
              mod_summary_table_ui("summary_table_ui_1")
            ) # ,
            # tabPanel("Methods and raw data",
            #          mod_summary_raw_ui("summary_raw_ui_1"))
          )
        )
      ),

      # Pop growth tab -----
      tabPanel(
        "Population Growth",
        tabsetPanel(
          selected = "Population map",
          tabPanel(
            "Population map",
            sidebarPanel(mod_pop_selections_ui("pop_selections_ui_1")),
            mainPanel(
            # mod_pop_selections_ui("pop_selections_ui_1"),
            mod_pop_map_ui("pop_map_ui_1"))
          ),
          tabPanel(
            "Demographic shifts",
            mod_pop_demoshifts_ui("pop_demoshifts_ui_1")
          )
        )
      ),


      ## Notes tab -----
      tabPanel(
        "Notes",
        mod_notes_ui("notes_ui_1")
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )

  tags$head(
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Regional Parks and the American Community Survey"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
