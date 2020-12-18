#' pop_demoshifts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pop_demoshifts_ui <- function(id) {
  ns <- NS(id)
  tagList(
    HTML('<p>Regional demographic characteristics are expected to shift. These estimates are based on the 2010 Census and forecast to 2040. No information is available on the spatial characteristics of forecasted demographic shifts.</p>'),
    fluidPage(
      fluidRow(
        column(
          width = 12,
          plotOutput(ns("race"), height = 400)
        )
      ),
      fluidRow(column(
        width = 12,
        plotOutput(ns("age"), height = 400)
      ))
    )
  )
}

#' pop_demoshifts Server Function
#'
#' @noRd
#' @importFrom ggplot2 ggplot aes
mod_pop_demoshifts_server <- function(input, output, session) {
  ns <- session$ns

  # ##########
  # # Figs
  # ##########
  #
  # ## race ---------
  #
  # # this is how metrostats presents data: https://metrocouncil.org/Data-and-Maps/Publications-And-Resources/MetroStats/Land-Use-and-Development/Steady-Growth-and-Big-Changes-Ahead-Regional-Forec.aspx
  # raceshift %>%
  #   mutate(Race = factor(Race, levels = c("Latinx",
  #                                         "Asian and Other Races",
  #                                         "Black-NonLatinx",
  #                                         "White-NonLatinx"
  #   ))) %>%
  #   ggplot(aes(x = Year, y = SubTotalPop, fill = Race)) +
  #   geom_bar(stat = "identity",
  #            color = "black") +
  #   council_theme() +
  #   labs(x = "Forecast year",
  #        y = "Forecasted population",
  #        fill = "Race & Ethnicity") +
  #   scale_fill_brewer(palette = "Blues")
  #
  # # but I like this:

  output$race <- renderPlot({
    regionalparks.acs::demo_shifts %>%
      filter(Type == "raceshift") %>%
      mutate(Race = factor(Race, levels = c(
        "Latinx",
        "Asian and Other Races",
        "Black-NonLatinx",
        "White-NonLatinx"
      ))) %>%
      ggplot(aes(x = Year, y = Percent, fill = Race)) +
      geom_bar(
        stat = "identity",
        col = "black"
      ) +
      council_theme() +
      labs(
        x = "Forecast year",
        y = "Forecasted % of total population",
        fill = "Race & Ethnicity",
        title = "Race & Ethnicity forecasts"
      ) +
      scale_fill_brewer(palette = "Blues") +
      geom_text(aes(label = round(Percent, 1)),
        position = position_stack(vjust = 0.5),
        size = 8
      )
  })
  #
  #
  # ## age ---------------
  # # metrostas way: https://metrocouncil.org/Data-and-Maps/Publications-And-Resources/MetroStats/Land-Use-and-Development/Steady-Growth-and-Big-Changes-Ahead-Regional-Forec.aspx
  # # COOL - they use the same age breakdowns as we are using here
  # ageshift %>%
  #   mutate(Ages = factor(Ages, levels = c("Ages 65+",
  #                                               "Ages 25-64",
  #                                               "Ages 15-24",
  #                                               "Ages 0-14"
  #   ))) %>%
  #   ggplot(aes(x = Year, y = SubTotalPop, fill = Agse)) +
  #   geom_bar(stat = "identity",
  #            color = "black") +
  #   council_theme() +
  #   labs(x = "Forecast year",
  #        y = "Forecasted population",
  #        fill = "Age") +
  #   scale_fill_brewer(palette = "Greens")
  #
  # # ee preference
  output$age <- renderPlot({
    regionalparks.acs::demo_shifts %>%
      filter(Type == "ageshift") %>%
      mutate(Age = factor(Ages, levels = c(
        "Ages 65+",
        "Ages 25-64",
        "Ages 15-24",
        "Ages 0-14"
      ))) %>%
      ggplot(aes(x = Year, y = Percent, fill = Ages)) +
      geom_bar(
        stat = "identity",
        color = "black"
      ) +
      council_theme() +
      labs(
        x = "Forecast year",
        y = "Forecasted % of total population",
        fill = "Ages",
        title = "Age forecasts"
      ) +
      scale_fill_brewer(palette = "Greens") +
      geom_text(aes(label = round(Percent, 1)),
        position = position_stack(vjust = 0.5),
        size = 8
      )
  })
}

## To be copied in the UI
# mod_pop_demoshifts_ui("pop_demoshifts_ui_1")

## To be copied in the server
# callModule(mod_pop_demoshifts_server, "pop_demoshifts_ui_1")
