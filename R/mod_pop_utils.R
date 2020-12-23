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
                                 selected_popvars) {
  ns <- session$ns
  
  
  
  make_pop_data <- reactive({
    p6 <- 
      regionalparks.acs::est_pop %>%
           select(selected_popvars$input_pop) %>%
        filter(!is.na(selected_popvars$input_pop))
    return(p6)
  })
    # 
    # regionalparks.acs::est_pop %>%
    #   select(POP2040) %>%# selected_popvars$input_pop) %>%
    #   filter(!is.na(POP2040)) #selected_popvars$input_pop))

  
  make_pop_parktrail_data <- reactive({
    p4 <- regionalparks.acs::park_trail_geog_LONG %>%
      dplyr::filter(
        agency %in% selected_popvars$input_agency,
        Type %in% selected_popvars$input_type,
        status2 %in% selected_popvars$input_status
      )
    return(p4)
  })
  
  make_pop_buffer_data <- reactive({
    p5 <- regionalparks.acs::buffer_geo %>%
      dplyr::filter(
        agency %in% selected_popvars$input_agency,
        type %in% selected_popvars$input_type,
        status %in% selected_popvars$input_status,
        distance == selected_popvars$input_distance
      ) %>% 
      separate(name, into = c("name", "delete"), sep= "_")
    return(p5)
  })

  vals <- reactiveValues()
  
  observe({
    vals$pop_data <- make_pop_data()
  })

  observe({
    vals$pop_parktrail_data <- make_pop_parktrail_data()
  })
  
  observe({
    vals$pop_buffer_data <- make_pop_buffer_data()
  })
  
  return(vals)

  }

## To be copied in the UI
# mod_pop_utils_ui("pop_utils_ui_1")

## To be copied in the server
# callModule(mod_pop_utils_server, "pop_utils_ui_1")
