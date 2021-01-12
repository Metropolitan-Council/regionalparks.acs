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
        "Home",
        mod_intro_ui("intro_ui_1")
      ),

      ## map tab -----
      tabPanel(
        "System Map",
        shiny::p("This map provides a general overview of the population across the 7-county Twin Cities region and adjacent areas."),
        shiny::p("Population characteristic data is from the ",
        a(href = "https://www.census.gov/programs-surveys/acs", 
        "5-year American Community Survey (2015-2019)",
        .noWS = "outside",
        target = "_blank"
        ), " and shows the highest spatial resolution possible by variable (either Census tract or block group). Each demographic characteristic is shown as a percentage of the total population, with the exception of median household income, which is displayed in dollars. The darker the color, the higher the percentage (or income in dollars)."), 
        shiny::p(
          "Population growth data is from the Metropolitan Council's published estimates. Current population estimates are available for Census block groups. Future forecasts are based on 2010 Census data and city comprehensive plans and available at the transportation analysis zone (a coarser spatial resolution than Census block groups). Forecasts of shifting population demographics (race/ethnicity and age) are only available at the regional level, and are summarised in a ", 
          a(href = "https://metrocouncil.org/Data-and-Maps/Publications-And-Resources/MetroStats/Land-Use-and-Development/Steady-Growth-and-Big-Changes-Ahead-Regional-Forec.aspx", 
            "MetroStats publication.",
            .noWS = "outside",
            target="_blank"
          ), " More information and raw data can be found on the ",
          a(
            href = "https://metrocouncil.org/Data-and-Maps/Research-and-Data/Thrive-2040-Forecasts.aspx",
            "Metropolitan Council website.",
            .noWS = "outside",
            target="_blank"
          )
        ),

        sidebarPanel(
          mod_leaflet_sidebar_ui(id = "leaflet_sidebar_ui_1")
        ),
        mainPanel(
          mod_main_leaflet_ui("main_leaflet_ui_1")
        )
      ),

      # acs summary tab -----
      tabPanel(
        title = "Unit Summary",
        id = "sumtabs",
        shiny::p("This plot provides summarized population characteristics (",
        a(href = "",
        "ACS 2014-2019", 
        .noWS = "outside",
        target="_blank"
        ), ") for all the regional parks and trails. Point location along the x-axis indicates the demographic value which can be compared across and within units or agencies. Point color indicates unit status (green = existing, orange = planned, yellow = search). Point shape indicates unit type (circle = park, square = trail). Subplots indicate either average values within agency boundaries or unit-level values."), 
        shiny::p("Right-click on image to copy or download. Click on any point to create a text-based interpretation. Tabular data filtered by agency may also be downloaded."),
        mod_summary_download_ui("summary_download_ui_1"), 
        hr(),
        sidebarPanel(width = 3, mod_summary_selections_ui("summary_selections_ui_1"),
                      style = "position:fixed;width:inherit;"),
        mainPanel(width = 9,
                  fluidRow(mod_summary_ggplot_ui("mod_summary_ggplot_ui_1"),
                           style = "height:1000px")#,
                           # height = "mod_summary_utils$sum_plotheight"),
                           # style = "height:mod_summary_utils$sum_plotheight"),
                  # hr(),
                  # fluidRow(mod_summary_download_ui("summary_download_ui_1"),
                  #          mod_summary_table_ui("summary_table_ui_1"))
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
