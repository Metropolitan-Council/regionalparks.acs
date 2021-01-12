#' leaflet_sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_leaflet_sidebar_ui <- function(id){
  ns <- NS(id)
  tagList(
    # wellPanel(
    #   h4("About this app"),
    #   tags$p("This mapping tool visualizes population characteristics and sizes alongside regional parks and trails across the Twin Cities region.")
    # ),
    
    wellPanel(
      id = "controls",
      selectInput(ns("source"), h4("Choose your data source"),
                  choices = c(
                    "Population characteristics",
                    "Population size"
                  ), selectize = FALSE,
                  selected = "CoStar"
      ),
      
      conditionalPanel(
        ns = ns,
        condition = "input.source == 'Population characteristics'",
        selectInput(ns("zdate"),
                    h4("Choose a variable to map:"),
                      choices = list(
                        `Age` = list(
                          "Age, % under 15" = "adj_ageunder15_per",
                          "Age, % 15-24" = "adj_age15_24_per",
                          "Age, % 25-64" = "adj_age25_64_per",
                          "Age, % 65+" = "adj_age65up_per"
                        ),
                        `Ethnicity & Race` = list(
                          "Ethnicity, % Hispanic" = "adj_hisppop_per",
                          "Ethnicity, % not-Hispanic" = "adj_nothisppop_per",
                          "Race, % Am. Indian" = "adj_amindnh_per",
                          "Race, % Asian" = "adj_asiannh_per",
                          "Race, % Black" = "adj_blacknh_per",
                          "Race, % Other + Multi" = "adj_othermultinh_per",
                          "Race, % White" = "adj_whitenh_per"
                        ),
                        
                        `National origin` = list(
                          "Origin, % foreign-born" = "adj_forborn_per",
                          "Origin, % US-born" = "adj_usborn_per"
                        ),
                        `Ability` = list(
                          "Ability, % any disability" = "adj_anydis_per"
                        ),
                        `Income` = list("Mean household income ($)" = "adj_meanhhi"),
                        `Transportation` = list("% Housholds without a vehicle" = "adj_novehicle_per"),
                        `Language` = list(
                          "% limited English proficiency" = "adj_lep_per",
                          "% Spanish speakers" = "adj_span_per"
                        )
                      ),
                    selectize = FALSE,
                    selected = "adj_ageunder15_per"
        )
      ),
      
      conditionalPanel(
        ns = ns,
        condition = "input.source == 'Population size'",
        
        selectInput(ns("cdate"),
                    h4("Choose a variable to map:"),
                    choices = list(
                      `Observed pop.` = list(
                        "2019 population" = "adj_ageunder15_per",
                        "2019 population density" = "adj_age15_24_per"
                      ),
                      `Forecasted pop.` = list(
                        "2040 forecast pop." = "adj_hisppop_per",
                        "2040 forecast pop. dens." = "adj_nothisppop_per"
                      ),
                      `Growth` = list(
                        "2010-2040, absolute growth" = "adj_forborn_per",
                        "2010-2040, relative growth" = "adj_usborn_per"
                      )
                    ),
                    selectize = FALSE
        )
      )
    ), 
    
    wellPanel(
      # h4("Choose park/trail units:"),
      id = "maintype",
      checkboxGroupInput(
        ns("input_type"),
        label = h4("Choose park/trail units:"),
        choices = c("Park - Existing", 
                    "Trail - Existing",
                    "Park - Planned", 
                    "Trail - Planned", 
                    "Park - Search", 
                    "Trail - Search"),
        selected = c("Park - Existing", "Trail - Existing")
      )
    ),
    
    wellPanel(
      id = "mainbufs",
      checkboxGroupInput(
        ns("input_type"),
        label = h4("Choose buffer distances:"),
        choices = c("1 mile buffer", 
                    "1.5 mile buffer", 
                    "3 mile buffer")
      )
    )
  )
}
    
#' leaflet_sidebar Server Function
#'
#' @noRd 
mod_leaflet_sidebar_server <- function(input, output, session){
  ns <- session$ns
 
  vals <- reactiveValues()
  
  observe({
    vals$acs_variable <- input$acs_variable
  })
  
  observe({
    vals$pop_variable <- input$pop_variable
  })
  
  return(vals)
  
  
}
    
## To be copied in the UI
# mod_leaflet_sidebar_ui("leaflet_sidebar_ui_1")
    
## To be copied in the server
# callModule(mod_leaflet_sidebar_server, "leaflet_sidebar_ui_1")
 
