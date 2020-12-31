#' leaflet_utils UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_leaflet_utils_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' leaflet_utils Server Function
#'
#' @noRd 
#' @importFrom shiny NS tagList
#' @import tibble
#' @import ggplot2
#' @import cowplot
#' @import plotly
#' @import tidyr
#' @import stringr
#' @import forcats
#' @import dplyr
#' @import leaflet
mod_leaflet_utils_server <- function(input, output, session, selected_map_vars){
  ns <- session$ns
 
  
  tractdata <- tibble(ACS = c("adj_anydis_per", "adj_forborn_per", "adj_usborn_per"))
  
  make_leafletacs_data <- reactive({
    # p6 <- regionalparks.acs::bg_geo[selected_map_vars$input_acs]
    p6 <- 
    #   if (selected_map_vars$input_acs %in% tractdata$ACS) (
    #   regionalparks.acs::census_tract %>%
    #     mutate(
    #       disab_percent = `Disability, any disability` * 100,
    #       usborn_percent = `Origin, US-born` * 100,
    #       forborn_percent = `Origin, foreign-born` * 100
    #     ) %>%
    #     rename(
    #       "adj_anydis_per" = "disab_percent",
    #       "adj_usborn_per" = "usborn_percent",
    #       "adj_forborn_per" = "forborn_percent"
    #     ) %>%
    #     select(selected_map_vars$input_acs)
    # )
    # 
    # else (
      regionalparks.acs::block_group %>%
        mutate(
          ageunder15_percent = ageunder15_percent * 100,
          age15_24_percent = age15_24_percent * 100,
          age25_64_percent = age25_64_percent * 100,
          age65up_percent = age65up_percent * 100,
          whitenh_percent = whitenh_percent * 100,
          blacknh_percent = blacknh_percent * 100,
          asiannh_percent = asiannh_percent * 100,
          amindnh_percent = amindnh_percent * 100,
          othermultinh_percent = othermultinh_percent * 100,
          hisppop_percent = hisppop_percent * 100,
          nothisppop_percent = nothisppop_percent * 100,
          novehicle_percent = novehicle_percent * 100,
          poorenglish_percent = poorenglish_percent * 100,
          spanish_percent = spanish_percent * 100
        ) %>%
        rename(
          "adj_ageunder15_per" = "ageunder15_percent",
          "adj_age15_24_per" = "age15_24_percent",
          "adj_age25_64_per" = "age25_64_percent",
          "adj_age65up_per" = "age65up_percent",
          "adj_whitenh_per" = "whitenh_percent",
          "adj_blacknh_per" = "blacknh_percent",
          "adj_asiannh_per" = "asiannh_percent",
          "adj_amindnh_per" = "amindnh_percent",
          "adj_othermultinh_per" = "othermultinh_percent",
          "adj_hisppop_per" = "hisppop_percent",
          "adj_nothisppop_per" = "nothisppop_percent",
          "adj_meanhhi" = "meanhhinc",
          "adj_novehicle_per" = "novehicle_percent",
          "adj_lep_per" = "poorenglish_percent",
          "adj_span_per" = "spanish_percent"
        ) %>%
        select(selected_map_vars$input_acs)
    # )
  
    return(p6)
  })

  vals <- reactiveValues()
  
  
  observe({
    vals$leaflet_data <- make_leafletacs_data()
  })
  
  # observe({
  #     vals$selected_var <- input$inputCensusTracts
  #     vals$color_pal <- "Blues"#dplyr::filter(renamekey, ACS == input$inputCensusTracts)[[3]]
  #   vals$leafletacs_data <- make_leafletacs_data()
  # })
  
  # observeEvent(input$inputCensusTracts, {
  #   vals$selected_var <- input$inputCensusTracts
  #   vals$color_pal <- dplyr::filter(table_ct, category == input$inputCensusTracts)[[3]]
  #   vals$tract_data <- census_tract[input$inputCensusTracts]
  # })
  
  
  return(vals)
}

    
## To be copied in the UI
# mod_leaflet_utils_ui("leaflet_utils_ui_1")
    
## To be copied in the server
# callModule(mod_leaflet_utils_server, "leaflet_utils_ui_1")
 
