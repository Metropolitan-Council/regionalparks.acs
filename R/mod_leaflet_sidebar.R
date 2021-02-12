#' leaflet_sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_leaflet_sidebar_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # wellPanel(
    #   h4("About this app"),
    #   tags$p("This mapping tool visualizes population characteristics and sizes alongside regional parks and trails across the Twin Cities region.")
    # ),

    wellPanel(
      id = "controls",
      selectInput(ns("source"), h4("Choose your data source:"),
        choices = c(
          "Population characteristics",
          "Population estimates"
        ), selectize = FALSE,
        selected = "Population characteristics"
      ),

      conditionalPanel(
        ns = ns,
        condition = "input.source == 'Population characteristics'",
        selectInput(ns("mainacs"),
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
              "Ability, % ambulatory disability" = "adj_ambdis_per",
              "Ability, % any other disability" = "adj_anydis_per"
            ),
            `Income` = list(
              "Mean household income ($)" = "adj_meanhhi",
              "Income, % below 185% poverty line" = "adj_185pov_per"
            ),
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
        condition = "input.source == 'Population estimates'",

        selectInput(ns("mainpop"),
          h4("Choose a variable to map:"),
          choices = list(
            `Annual population estimates` = list(
              "2019 population" = "PopEst_2019",
              "2019 population density" = "PopDens_2019"
            ),
            `Long-range population forecast` = list(
              "2040 population" = "POP2040",
              "2040 population density" = "popdens_2040_mi"
            ),
            `Growth` = list(
              "2010-2040, absolute growth" = "growth_abs_10_40",
              "2010-2040, relative growth" = "growth_rel_10_40"
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
        ns("input_parktype"),
        label = h4("Choose park/trail units:"),
        choices = c(
          "Park - existing",
          "Trail - existing",
          "Park - planned",
          "Trail - planned",
          "Park - search",
          "Trail - search"
        ),
        selected = c(
          "Park - existing",
          "Trail - existing"
        )
      )
    ),

    wellPanel(
      id = "mainbufs",
      radioButtons(
        ns("input_bufferdist"),
        label = h4("Choose buffer distances:"),
        choices = c(1, 1.5, 3), selected = c(1)
      )
    )
  )
}

#' leaflet_sidebar Server Function
#'
#' @noRd
mod_leaflet_sidebar_server <- function(input, output, session) {
  ns <- session$ns

  observed <- tibble(observed = c("PopEst_2019", "PopDens_2019"))


  make_map_bg_data_main <- reactive({
    req(input$source)

    p6 <- if (input$source == "Population characteristics") {
      if (input$mainacs %in% tract_vars$ACS) {
        regionalparks.acs::census_tract_map %>%
          select(input$mainacs)
      } else {
        regionalparks.acs::block_group_map %>%
          select(input$mainacs)
      }
    } else if (input$source == "Population estimates") {
      if (input$mainpop %in% observed$observed) {
        regionalparks.acs::est_pop %>%
          select(
            input$mainpop,
            bg_id
          )
      } else {
        regionalparks.acs::taz_growth %>%
          select(
            input$mainpop,
            TAZ2012
          )
      }
    }
    return(p6)
  })


  make_map_parktrail_data_main <- reactive({
    p4 <- regionalparks.acs::park_trail_geog_LONG %>%
      dplyr::filter(
        status %in% input$input_parktype
      )
    return(p4)
  })


  make_map_buffer_data_main <- reactive({
    p5 <- regionalparks.acs::buffer_geo %>%
      mutate(status = case_when(
        status == "Existing" ~ "existing",
        status == "Planned" ~ "planned",
        status == "Search" ~ "search"
      )) %>%
      mutate(typestatus = paste0(type, " - ", status)) %>%
      dplyr::filter(
        typestatus %in% input$input_parktype,
        distance == input$input_bufferdist
      )
    return(p5)
  })



  vals <- reactiveValues()

  observe({
    vals$map_bg_data_main <- make_map_bg_data_main()
  })

  observe({
    vals$map_parktrail_data_main <- make_map_parktrail_data_main()
  })


  observe({
    vals$map_buffer_data_main <- make_map_buffer_data_main()
  })

  observeEvent(input$source, {
    vals$source <- input$source
  })

  observeEvent(input$mainpop, {
    vals$mainpop <- input$mainpop
  })

  observeEvent(input$mainacs, {
    vals$mainacs <- input$mainacs
  })

  observeEvent(input$input_parktype, {
    vals$input_parktype <- input$input_parktype
  })

  observeEvent(input$input_bufferdist, {
    vals$input_bufferdist <- input$input_bufferdist
  })


  generate_pop_pal <- reactive({
    pal <-
      if (input$mainpop %in% quantile_vars$mainpop) {
        colorQuantile(n = 8, palette = "Blues", domain = vals$map_bg_data_main[[1]])
      } else {
        colorNumeric(n = 8, palette = "Blues", domain = vals$map_bg_data_main[[1]])
      }
    return(pal)
  })

  observe({
    vals$pop_pal <- generate_pop_pal()
  })
  
  
  
  # generateqpal_colors <- unique(qpal(sort(a[[1]]))) # hex codes
  # qpal_labs <- quantile(a[[1]], seq(0, 1, (1/8)), na.rm = T) # depends on n from pal
  # qpal_labs <- paste(lag(qpal_labs), qpal_labs, sep = " - ")[-1] # first lag is NA
  # qpal_labsPERCENT <- paste0(qpal_labs, " %")

  return(vals)

  #------
}

## To be copied in the UI
# mod_leaflet_sidebar_ui("leaflet_sidebar_ui_1")

## To be copied in the server
# callModule(mod_leaflet_sidebar_server, "leaflet_sidebar_ui_1")
