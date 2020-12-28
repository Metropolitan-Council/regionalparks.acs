#' acs_utils UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_acs_utils_ui <- function(id) {
  ns <- NS(id)
  tagList()
}

#' acs_utils Server Function
#'
#' @noRd
mod_acs_utils_server <- function(input, output, session,
                                 selected_acs,
                                 selected_parktrail) {
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

  make_plot_buffer_data <- reactive({
    make_table_buffer_data() %>%
      dplyr::filter(
        ACS == selected_acs$input_acs
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
        name = forcats::fct_reorder(name, (value))
      )
  })

  make_plotly_agency_data <- reactive({
    regionalparks.acs::agency_avg %>%
      filter(
        agency %in% selected_parktrail$input_agency,
        ACS == selected_acs$input_acs
      ) %>%
      mutate(
        agency = forcats::fct_reorder(agency, (value))
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
        agency %in% selected_parktrail$input_agency,
        ACS == selected_acs$input_acs
      )
    return(p3)
  })


  make_facet_data <- reactive({
    make_plotly_agency_data() %>%
      mutate(
        level = "Agency avg.",
        type = "avg"
      ) %>%
      rename(name = agency) %>%
      bind_rows(make_plot_buffer_data() %>%
        mutate(level = "Unit values")) %>%
      pivot_wider(names_from = ACS, values_from = value) %>%
      rename(value = starts_with("adj")) %>%
      mutate(hovtext = paste0("Approx. ", .$value, "% of pple within", .$distance, " mi are"))
  })


  tractdata <- tibble(ACS = c("adj_anydis_per", "adj_forborn_per", "adj_usborn_per"))

  make_map_bg_data <- reactive({
    p6 <- if (selected_acs$input_acs %in% tractdata$ACS) {
      regionalparks.acs::census_tract %>%
        mutate(
          disab_percent = `Disability, any disability` * 100,
          usborn_percent = `Origin, US-born` * 100,
          forborn_percent = `Origin, foreign-born` * 100
        ) %>%
        rename(
          "adj_anydis_per" = "disab_percent",
          "adj_usborn_per" = "usborn_percent",
          "adj_forborn_per" = "forborn_percent"
        ) %>%
        select(selected_acs$input_acs)
    }

    else {
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
        select(selected_acs$input_acs)
    }
    return(p6)
  })


  vals <- reactiveValues()

  observe({
    vals$plot_buffer_data <- make_plot_buffer_data()
  })

  observe({
    vals$plotly_agency_data <- make_plotly_agency_data()
  })

  observe({
    vals$agencyavg_data <- make_agencyavg_data()
  })

  observe({
    vals$facet_data <- make_facet_data()
  })

  observe({
    vals$map_bg_data <- make_map_bg_data()
  })

  return(vals)
}

## To be copied in the UI
# mod_acs_utils_ui("acs_utils_ui_1")

## To be copied in the server
# callModule(mod_acs_utils_server, "acs_utils_ui_1")
