#' parktrail_utils UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_parktrail_utils_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' parktrail_utils Server Function
#'
#' @noRd 
mod_parktrail_utils_server <- function(input, output, session,
                                       selected_parktrail){
  ns <- session$ns

  renamekey <- tibble::tribble(
    ~goodname,
    ~"ACS",
    "Total population",
    "adj_2019pop",
    "Age, % under 15",
    "adj_ageunder15_per",
    "Age, % 15-24",
    "adj_age15_24_per",
    "Age, % 25-64",
    "adj_age25_64_per",
    "Age, % 65 and up",
    "adj_age65up_per",
    "Race, % White",
    "adj_whitenh_per",
    "Race, % Black",
    "adj_blacknh_per",
    "Race, % Asian",
    "adj_asiannh_per",
    "Race, % American Indian",
    "adj_amindnh_per",
    "Race, % Other + Multiracial",
    "adj_othermultinh_per",
    "Ethnicity, % Hispanic",
    "adj_hisppop_per",
    "Ethnicity, % not-Hispanic",
    "adj_nothisppop_per",
    "Mean household income",
    "adj_meanhhi",
    "% Housholds without a vehicle",
    "adj_novehicle_per",
    "% speaking English less than very well",
    "adj_lep_per",
    "% Spanish speakers",
    "adj_span_per",
    "Ability, % any disability",
    "adj_anydis_per",
    "Origin, % US-born",
    "adj_usborn_per",
    "Origin, % foreign-born",
    "adj_forborn_per"
  )
  
  make_parktrail_data <- reactive({
    p4 <- regionalparks.acs::park_trail_geog_LONG %>%
      dplyr::filter(
        agency %in% selected_parktrail$input_agency,
        Type %in% selected_parktrail$input_type,
        status2 %in% selected_parktrail$input_status
      )
    return(p4)
  })
  
  make_buffer_data <- reactive({
    p5 <- regionalparks.acs::buffer_geo %>%
      dplyr::filter(
        agency %in% selected_parktrail$input_agency,
        type %in% selected_parktrail$input_type,
        status %in% selected_parktrail$input_status,
        distance == selected_parktrail$input_distance
      ) %>% 
      separate(name, into = c("name", "delete"), sep= "_")
    return(p5)
  })
  
  make_table_buffer_data <- reactive({
    p <- regionalparks.acs::long_buffer_data %>%
      dplyr::filter(
        agency %in% selected_parktrail$input_agency,
        type %in% selected_parktrail$input_type,
        distance == selected_parktrail$input_distance,
        status %in% selected_parktrail$input_status
      ) %>%
      separate(
        name,
        into = c("name", "delete2"),
        sep = c("_")
      )  %>%
      left_join(renamekey, by = c("ACS" = "ACS")) %>%
      mutate(acs_short = stringr::str_remove(goodname, "% ")) %>%
      mutate(hover_text = stringr::str_wrap(paste0(
        "Approx. ",
        "<b>", value, "%", "</b>", " of the pop. within ",
        distance, " mile of ",
        name, " (", status, ")", " falls into the ",
        "<b>", acs_short, "</b>",
        " category"
      ), 55))
    return(p)
  })
  
  vals <- reactiveValues()
  
  observe({
    vals$parktrail_data <- make_parktrail_data()
  })
  
  observe({
    vals$buffer_data <- make_buffer_data()
  })
  
  observe({
    vals$table_data <- make_table_buffer_data()
  })
  
  return(vals)
  
}
    
## To be copied in the UI
# mod_parktrail_utils_ui("parktrail_utils_ui_1")
    
## To be copied in the server
# callModule(mod_parktrail_utils_server, "parktrail_utils_ui_1")
 
