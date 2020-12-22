#' summary_utils UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_summary_utils_ui <- function(id) {
  ns <- NS(id)
  tagList()
}

#' summary_utils Server Function
#'
#' @noRd
mod_summary_utils_server <- function(input, output, session,
                                     selected_vars) {
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
  
  make_table_buffer_data <- reactive({
    # browser()
    p <- regionalparks.acs::long_buffer_data %>%
      dplyr::filter(
        agency %in% selected_vars$input_agency,
        type %in% selected_vars$input_type,
        distance == selected_vars$input_distance,
        status %in% selected_vars$input_status
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

  make_plot_buffer_data <- reactive({
    make_table_buffer_data() %>%
      dplyr::filter(
        ACS == selected_vars$input_acs
      ) %>%
      mutate(name = str_replace_all(
        name,
        c(
          "Regional Park" = "RP",
          "Regional Trail" = "RT",
          "Park Reserve" = "PR",
          "Special Recreation Feature" = "SRF"
        )
      )) %>% 
      mutate(
        name = forcats::fct_reorder(name, desc(value)),
        concat = paste(type, status, sep = "_")
      )
  })


  # make_plotly_agency_data <- reactive({
  #   agency_planned_existing_avgs %>%
  #     filter(
  #       agency %in% selected_vars$input_agency,
  #       # type %in% selected_vars$input_type,
  #       distance == selected_vars$input_distance,
  #       # status %in% selected_vars$input_status,
  #       ACS == selected_vars$input_acs
  #     ) %>%
  #     left_join(renamekey, by = c("ACS" = "ACS variable")) %>%
  #     mutate(acs_short = stringr::str_remove(goodname, "% ")) %>%
  #     mutate(hover_text = stringr::str_wrap(paste0(
  #       "The average ",
  #       "<b>", acs_short, "</b>",
  #       " within ",
  #       "<b>", agency, "</b>",
  #       "'s jurisdiction is ",
  #       "<b>", avg, "%", "</b>",
  #       "."
  #     ), 50))
  # })
  
  make_plotly_agency_data <- reactive({
    regionalparks.acs::agency_avg %>%
      filter(
        agency %in% selected_vars$input_agency,
        ACS ==  selected_vars$input_acs
      ) %>%
      mutate(
        agency = forcats::fct_reorder(agency, desc(value))
      ) %>%
      left_join(renamekey, by = c("ACS" = "ACS")) %>%
      mutate(acs_short = stringr::str_remove(goodname, "% ")) %>%
      mutate(hover_text = stringr::str_wrap(paste0(
        "The average ",
        "<b>", acs_short, "</b>",
        " within ",
        "<b>", agency, "</b>",
        "'s jurisdiction is ",
        "<b>", value, "%", "</b>",
        "."
      ), 50))
  })


  make_agencyavg_data <- reactive({
    p3 <- regionalparks.acs::agency_avg %>%
      dplyr::filter(
        agency %in% selected_vars$input_agency,
        ACS == selected_vars$input_acs
      )
    return(p3)
  })


  make_map_parktrail_data <- reactive({
    p4 <- regionalparks.acs::park_trail_geog_LONG %>%
      dplyr::filter(
        agency %in% selected_vars$input_agency,
        Type %in% selected_vars$input_type,
        status2 %in% selected_vars$input_status
      )
    return(p4)
  })


  make_map_buffer_data <- reactive({
    p5 <- regionalparks.acs::buffer_geo %>%
      dplyr::filter(
        agency %in% selected_vars$input_agency,
        type %in% selected_vars$input_type,
        status %in% selected_vars$input_status,
        distance == selected_vars$input_distance
      )
    return(p5)
  })

  # make_plot_rawbuffer_data <- reactive({
  #   p7 <- regionalparks.acs::long_buffer_data_raw %>%
  #     dplyr::filter(
  #       agency %in% selected_vars$input_agency,
  #       type %in% selected_vars$input_type,
  #       distance == selected_vars$input_distance,
  #       status %in% selected_vars$input_status,
  #       ACS == selected_vars$input_acs
  #     )
  #   return(p7)
  # })


  # recode_bg_names <- tibble::tribble( # recode bg names ----
  #   ~block_group_name, ~acs_selection,
  #   "ageunder15_percent", "adj_ageunder15_per",
  #   "age15_24_percent", "adj_age15_24_per",
  #   "age25_64_percent", "adj_age25_64_per",
  #   "age65up_percent", "adj_age65up_per",
  #   "whitenh_percent", "adj_whitenh_per",
  #   "blacknh_percent", "adj_blacknh_per",
  #   "asiannh_percent", "adj_asiannh_per",
  #   "amindnh_percent", "adj_amindnh_per",
  #   "othermutltnh_percent", "adj_othermultinh",
  #   "hisppop_percent", "adj_hisppop_per",
  #   "nothisppop_percent", "adj_nothisppop_per",
  #   "meanhhinc", "adj_meanhhi",
  #   "novehicle_percent", "adj_novehicle_per",
  #   "poorenglish_percent", "adj_lep_per",
  #   "spanish_percent", "adj_span_per"
  # )

tractdata <- tibble(ACS = c("adj_anydis_per", "adj_forborn_per", "adj_usborn_per"))
  
  make_map_bg_data <- reactive({
    # p6 <- regionalparks.acs::bg_geo[selected_vars$input_acs]
    p6 <- if (selected_vars$input_acs %in% tractdata$ACS) {
      regionalparks.acs::census_tract %>%
        mutate(disab_percent = `Disability, any disability`*100,
               usborn_percent = `Origin, US-born` * 100,
               forborn_percent = `Origin, foreign-born` * 100) %>%
        rename(
          "adj_anydis_per" = "disab_percent",
          "adj_usborn_per" = "usborn_percent",
          "adj_forborn_per" = "forborn_percent"
        )%>% 
        select(selected_vars$input_acs)
    }
      
      else {regionalparks.acs::block_group %>%
      mutate(ageunder15_percent = ageunder15_percent*100,
             age15_24_percent = age15_24_percent * 100,
             age25_64_percent = age25_64_percent * 100,
             age65up_percent = age65up_percent * 100,
             whitenh_percent = whitenh_percent * 100,
             blacknh_percent = blacknh_percent * 100,
             asiannh_percent = asiannh_percent*100,
             amindnh_percent = amindnh_percent * 100,
             othermultinh_percent = othermultinh_percent *100, 
             hisppop_percent = hisppop_percent * 100,
             nothisppop_percent = nothisppop_percent * 100,
             novehicle_percent = novehicle_percent * 100,
             poorenglish_percent = poorenglish_percent * 100,
             spanish_percent = spanish_percent * 100) %>%
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
      select(selected_vars$input_acs)} 
    return(p6)
  })


  vals <- reactiveValues()

  observe({
    vals$table_buffer_data <- make_table_buffer_data()
  })

  observe({
    vals$plot_buffer_data <- make_plot_buffer_data()
  })

  observe({
    vals$agencyavg_data <- make_agencyavg_data()
  })

  observe({
    vals$map_parktrail_data <- make_map_parktrail_data()
  })

  observe({
    vals$map_buffer_data <- make_map_buffer_data()
  })

  # observe({
  #   vals$plotly_buffer_data <- make_plotly_buffer_data()
  # })
  
  observe({
    vals$plotly_height <- nrow(make_plot_buffer_data())
  })

  observe({
    vals$plotly_agency_data <- make_plotly_agency_data()
  })

  # observe({
  #   vals$plot_rawbuffer_data <- make_plot_rawbuffer_data()
  # })

  observe({
    vals$map_bg_data <- make_map_bg_data()
  })

  return(vals)
}

## To be copied in the UI
# mod_summary_utils_ui("summary_utils_ui_1")

## To be copied in the server
# callModule(mod_summary_utils_server, "summary_utils_ui_1")
