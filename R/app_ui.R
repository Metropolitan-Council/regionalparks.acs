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
        "ACS Map",
        mod_leaflet_ui(id = "leaflet_ui_1"),
        mod_input_demos_ui(id = "input_demos_ui_1")
      ),
      
      ## ACS summary stats tab ----
      tabPanel(
        "ACS Summary",
        selectInput("dataset", "Choose a dataset:",
                    choices = c("rock", "pressure", "cars")),
        selectInput(
          "agency",
          label = h4("Agenc(y/ies)"),
          choices = c(
            "Anoka County",
            "Bloomington",
            "Carver County",
            "Dakota County",
            "MPRB",
            "Ramsey County",
            "Scott County",
            "St. Paul",
            "Three Rivers",
            "Washington County"
          ),
          selected = "Anoka County",
          multiple = TRUE
        ),
        
        mod_summarystats_ui("summarystats_ui_1")
      ),
      
      ## Pop growth tab -----
      tabPanel(
        "Population Growth",
        mod_popgrowth_ui("popgrowth_ui_1")
      ),
      

      ## Notes tab -----
      tabPanel(
        "Notes",
        mod_notes_ui("notes_ui_1")
      )

      # footer = tags$a(
      #   href = "https://metrocouncil.org", target = "_blank",
      #   img(src = "www/main-logo.png", align = "right", style = "padding: 1%")
      # )
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
