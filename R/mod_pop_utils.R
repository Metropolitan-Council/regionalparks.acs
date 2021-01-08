#' pop_utils UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pop_utils_ui <- function(id) {
  ns <- NS(id)
  tagList()
}

#' pop_utils Server Function
#'
#' @noRd
mod_pop_utils_server <- function(input, output, session,
                                 selected_population) {
  ns <- session$ns
  
  observed <- tibble(observed = c("PopEst_2019", "PopDens_2019"))
  
  make_pop_data <- reactive({
    p6 <-
      
      if(selected_population$input_pop %in% observed$observed)
        (regionalparks.acs::est_pop %>%
           select(selected_population$input_pop,
                  bg_id))
    else 
      (regionalparks.acs::taz_growth %>%
         select(selected_population$input_pop,
                TAZ2012))
    return(p6)
  })
  
  vals <- reactiveValues()
  
  observe({
    vals$pop_data <- make_pop_data()
  })
  
  
  generate_pop_pal <- reactive({
    pal <- 
      if (selected_population$input_pop == "PopDens_2019" | selected_population$input_pop == "popdens_2040_mi" | selected_population$input_pop == "growth_rel_10_40"){
      colorQuantile(n = 9, palette = "Blues", domain = summary_poputil$pop_data[[1]])
    } else {
      colorNumeric(n = 9, palette = "Blues", domain = summary_poputil$pop_data[[1]])
    }
    return(pal)
  })
      
  
  observe({
    vals$pop_pal <- generate_pop_pal()
  })
  
      return(vals)
}

## To be copied in the UI
# mod_pop_utils_ui("pop_utils_ui_1")

## To be copied in the server
# callModule(mod_pop_utils_server, "pop_utils_ui_1")
