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
        "About this tool",
        mod_intro_ui("intro_ui_1")
      ),

      ## map tab -----
      tabPanel(
        id = "map_tab",
        "System Map",
        shiny::p("This map provides a general overview of the population across the 7-county Twin Cities region and adjacent areas.", strong("Population characteristics")," available for mapping include: age, disability, ethnicity, income national origin, language, race, and transportation. ", strong("Forecasted population variables"), " available for mapping include: annual population estimates, long-range population estimates, and population growth."),
        hr(),
        sidebarPanel(style="max-width:300px", 
          mod_leaflet_sidebar_ui(id = "leaflet_sidebar_ui_1")
        ),
        mainPanel(style="max-width:1200px",
          mod_main_leaflet_ui("main_leaflet_ui_1")
        ), br(),hr(),
      shiny::p(
        "Population characteristic data is from the U.S. Census Bureau ",
        a(
          href = "https://www.census.gov/programs-surveys/acs",
          "(American Community Survey, 2015-2019)",
          .noWS = "outside",
          target = "_blank"
        ), " and shows the highest spatial resolution possible by variable (either Census tract or block group, see 'Notes'). Each demographic characteristic is shown as a percentage of the total population, with the exception of median household income, which is displayed in dollars. The darker the color, the higher the percentage (or income in dollars).", br(), br(), "Population estimates are published by the Metropolitan Council. Current population estimates are available for Census block groups. Long-range forecasts are based on 2010 Census data and city comprehensive plans and available at the transportation analysis zone (a coarser spatial resolution than Census block groups). Forecasts of shifting population demographics (race/ethnicity and age) are only available at the regional level, and are summarised in a ",
        a(
          href = "https://metrocouncil.org/Data-and-Maps/Publications-And-Resources/MetroStats/Land-Use-and-Development/Steady-Growth-and-Big-Changes-Ahead-Regional-Forec.aspx",
          "MetroStats publication.",
          .noWS = "outside",
          target = "_blank"
        ), " More information and raw data can be found on the ",
        a(
          href = "https://metrocouncil.org/Data-and-Maps/Research-and-Data/Thrive-2040-Forecasts.aspx",
          "Metropolitan Council website.",
          .noWS = "outside",
          target = "_blank"
        )
      )),

      # acs summary tab -----
      tabPanel(
        title = "Unit Level Data",
        id = "sumtabs",
        shiny::p(
          "This plot provides summarized population characteristics (",
          a(
            href = "",
            "American Community Survey, 2014-2019",
            .noWS = "outside",
            target = "_blank"
          ), ") for all the regional parks and trails. Point location along the x-axis indicates the demographic value which can be compared across and within units or agencies. Subplots indicate either average values within agency boundaries or unit-level values."
        ),br(),
        shiny::p("Right-click on image to copy or download. Click on any point to create a text-based interpretation. Tabular data filtered by agency may also be downloaded.",br(), br(),
        mod_summary_download_ui("summary_download_ui_1")),
        hr(),
        sidebarPanel(style="max-width:300px", #this does work, but it stops the wrapping
                     mod_summary_selections_ui("summary_selections_ui_1"),
          # style = "position:fixed;"
        ),
        mainPanel(
          fluidRow(style="min-width:700px; max-width:1200px", mod_summary_ggplot_ui("mod_summary_ggplot_ui_1")) # ,
          # style = "height:1000px")#,
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
