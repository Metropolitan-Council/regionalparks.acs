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
    navbarPage(
      collapsible = TRUE,
      title = div(img(src = "www/main-logo.png", height = "60px")),
      id = "nav",


      # intro tab -----
      tabPanel(
        "Introduction",
        mod_intro_ui("intro_ui_1"),
        sidebarPanel(
          mod_leaflet_sidebar_ui(id = "leaflet_sidebar_ui_1")
        ),
        mainPanel(
          
        )
      ),

      ## map tab -----
      tabPanel(
        "System Map",
        tags$p(HTML("This map provides a general overview of population characteristics across the 7-county Twin Cities region and adjacent areas. Data is from the 5-year American Community Survey (2015-2019) and shows the highest spatial resolution possible by variable (either Census tract or block group). Each demographic characteristic is shown as a percentage of the total population, with the exception of median household income, which is displayed in dollars. The darker the color, the higher the percentage (or income in dollars).")),

        sidebarPanel(
          mod_input_demos_ui(id = "input_demos_ui_1"),
        ),
        mainPanel(
          mod_leaflet_ui(id = "leaflet_ui_1")
        )
      ),

      # acs summary tab -----
      tabPanel(
        title = "Unit Summary",
        id = "sumtabs",
        # HTML("<p>Population characteristics within a given buffer zone around park and trail units are summarized in several ways. The <em>Weighted averages</em> tab distills complex spatial demographic patterns into a single summary statistic and will be appropriate for most uses. The <em>Buffer map</em> tab shows the spatial demographic patterns with the buffer analysis zones overlayed. The <em>Download tabular data</em> tab displays summary statistics for all ACS variables in tabular form.</p>"),
        
        # hr()

        sidebarPanel(width = 3, mod_summary_selections_ui("summary_selections_ui_1"),
                      style = "position:fixed;width:inherit;"),
        mainPanel(width = 9,
                  fluidRow(mod_summary_ggplot_ui("mod_summary_ggplot_ui_1"),
                           style = "height:1000px"),
                           # height = "mod_summary_utils$sum_plotheight"),
                           # style = "height:mod_summary_utils$sum_plotheight"),
                  hr(),
                  fluidRow(mod_summary_map_ui("summary_map_ui_1")),
                  hr(),
                  fluidRow(mod_summary_download_ui("summary_download_ui_1"),
                           mod_summary_table_ui("summary_table_ui_1")))
        
      ),

      # Pop growth tab -----
      tabPanel(
        "Population Growth",
        shiny::p(
          "The Metropolitan Council publishes current population estimates and future forecasted population estimates. Current population estimates are available for Census block groups. Future forecasts are based on 2010 Census data and city comprehensive plans and available at the transportation analysis zone (a coarser spatial resolution than Census block groups). Forecasts of shifting population demographics (race/ethnicity and age) are only available at the regional level, and are summarised in a ", 
          a(href = "https://metrocouncil.org/Data-and-Maps/Publications-And-Resources/MetroStats/Land-Use-and-Development/Steady-Growth-and-Big-Changes-Ahead-Regional-Forec.aspx", 
          "MetroStats publication.",
          .noWS = "outside",
          target="_blank"
          ), 
          br(), br(), " Given the differential methods and geographies used in calculating current and future populations, we will not perform further analyses on these data. However, the overarching patterns still may be useful in parks planning. More information and raw data can be found on the ",
          a(
            href = "https://metrocouncil.org/Data-and-Maps/Research-and-Data/Thrive-2040-Forecasts.aspx",
            "Metropolitan Council website.",
            .noWS = "outside",
            target="_blank"
          )
        ),
        # tabsetPanel(
          # id = "tab_being_displayed",
          # selected = "Population map",
          # tabPanel(
            # "Population map",
            sidebarPanel(mod_pop_selections_ui("pop_selections_ui_1")),
            mainPanel(mod_pop_map_ui("pop_map_ui_1"))
          # ),
          # tabPanel(
            # "Demographic shifts",
            # mod_pop_demoshifts_ui("pop_demoshifts_ui_1")
          # )
        # )
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
