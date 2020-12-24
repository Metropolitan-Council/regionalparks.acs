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

  
  make_parktrail_data <- reactive({
    p4 <- regionalparks.acs::park_trail_geog_LONG %>%
      dplyr::filter(
        agency %in% selected_parktrail$input_agency,
        Type %in% selected_parktrail$input_type,
        status2 %in% selected_parktrail$input_status
      )
    
    # test <-regionalparks.acs::park_trail_geog_LONG %>%
    #   dplyr::filter(
    #     agency =="Anoka County",
    #     Type =="Park",
    #     status2 =="Existing"
    #   )
    
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
  
  vals <- reactiveValues()
  
  observe({
    vals$parktrail_data <- make_parktrail_data()
  })
  
  observe({
    vals$buffer_data <- make_buffer_data()
  })
  
  return(vals)
  
}
    
## To be copied in the UI
# mod_parktrail_utils_ui("parktrail_utils_ui_1")
    
## To be copied in the server
# callModule(mod_parktrail_utils_server, "parktrail_utils_ui_1")
 
