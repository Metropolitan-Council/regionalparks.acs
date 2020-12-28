#' mod_summary_ggplot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_mod_summary_ggplot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::p("This plot provides summarized demographic values for all the regional parks and trails. Point location along the x-axis indicates the demographic value which can be compared across and within units or agencies. Color indicates unit status (green = existing, orange = planned, yellow = search). Shape indicates unit type (circle = park, square = trail). Subplots indicate either average values within agency boundaries or unit-level values. (Right-click on image to copy or download. Click on any point to create a text-based interpretation.)"),


    plotOutput(ns("leg"), height = 100),

    hr(),

    uiOutput(ns("text_info")),

    plotOutput(
      outputId = ns("Rggplots"),
      # hover = hoverOpts(ns("plot_hover"), delay = 100, delayType = "debounce"),
      click = hoverOpts(ns("plot_click"))
    ),

    # uiOutput(ns("hover_info"))
  )
}

#' mod_summary_ggplot Server Function
#'
#' @noRd
mod_mod_summary_ggplot_server <- function(input, output, session,
                                          selected_vars,
                                          summary_util) {
  ns <- session$ns


  type_status_legend <- # status legend -------
    cowplot::get_legend(
      tibble(
        status = rep(c("Existing", "Planned", "Search"), 3),
        type = rep(c(" Park ", " Trail ", "Agency avg."), each = 3),
        location = rep(1, 9)
      ) %>%
        ggplot(aes(
          fill = status,
          pch = type,
          x = location,
          y = status
        )) +
        geom_point() +
        scale_shape_manual(values = c(
          " Park " = 21,
          " Trail " = 22,
          "Agency avg." = 8
        )) +
        scale_fill_manual(
          values = c(
            "Existing" = e_col,
            "Planned" = p_col,
            "Search" = s_col
          )
        ) +
        cowplot::theme_cowplot() +
        guides(
          fill = guide_legend(
            override.aes = list(pch = 23, size = 8),
            label.position = "bottom"
          ),
          shape = guide_legend(
            override.aes = list(size = 8),
            label.position = "bottom"
          )
        ) +
        labs(fill = "        Status:", shape = "Type:") +
        theme(legend.position = "bottom")
    )


  ## legend -----
  output$leg <- renderPlot({
    plot_grid(type_status_legend)
  })

  renamekey <- tibble::tribble( #-----
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

  PlotHeight <- reactive( # plot height ------
    # #if want to set a minimum height
    return(
      if ((nrow(summary_util$plot_buffer_data[!duplicated(summary_util$plot_buffer_data[, c("name")]), ]) * 30) > 200) {
        (nrow(summary_util$plot_buffer_data[!duplicated(summary_util$plot_buffer_data[, c("name")]), ]) * 30)
      } else {
        200
      }

      # return(nrow(summary_util$plot_buffer_data[!duplicated(summary_util$plot_buffer_data[, c('name')]), ])*30 + nrow(summary_util$plotly_agency_data)*30
    )
  )


  output$Rggplots <- renderPlot(height = function() PlotHeight(), {
    summary_util$facet_data %>%
      ggplot(aes(
        y = name,
        x = value, # .[, 8],
        shape = type,
        fill = status
      )) +
      facet_grid(level ~ ., scales = "free_y", space = "free") +
      geom_point(size = 4) +
      scale_shape_manual(values = c("avg" = 8, "Park" = 21, "Trail" = 22)) +
      scale_fill_manual(values = c("Existing" = e_col, "Planned" = p_col, "Search" = s_col)) +
      council_theme() +
      labs(
        y = "",
        x = filter(renamekey, ACS == selected_vars$input_acs) %>% select(goodname),
        title = paste0((filter(renamekey, ACS == selected_vars$input_acs) %>% select(goodname)), " - ", selected_vars$input_distance, " mi buffer")
      ) +
      theme(
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14),
        strip.text.y = element_text(angle = 0),
        plot.title = element_text(size = 14, family = "Helvetica", face = "bold"),
        strip.background = element_rect(fill = "grey90"),
        panel.border = element_rect(color = "black", fill = NA),
        strip.placement = "outside",
        panel.grid.minor.x = element_blank()
      ) +
      # annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
      # annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
      guides(shape = FALSE, fill = FALSE) +
      scale_x_continuous(labels = scales::comma)
    # scale_x_continuous(labels = function(x) if(selected_vars$input_acs == "adj_meanhhi") {
    #   paste0("$", x)} else {paste0(x, "%")})
  })

  # #this is nice, but position is strange
  #   output$hover_info <- renderUI({
  #     hover <- input$plot_hover
  #     point <- nearPoints(summary_util$facet_data, hover, threshold = 10)#, maxpoints = 1, addDist = TRUE)
  #     if (nrow(point) == 0) return(NULL)
  #
  #     # calculate point position INSIDE the image as percent of total dimensions
  #     # from left (horizontal) and from top (vertical)
  #     left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
  #     top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
  #
  #     # calculate distance from left and bottom side of the picture in pixels
  #     left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
  #     top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
  #
  #     # create style property fot tooltip
  #     # background color is set so tooltip is a bit transparent
  #     # z-index is set so we are sure are tooltip will be on top
  #     style <- paste0("position:absolute; z-index:100; background-color: rgba(173, 173, 173, 0.6); ",
  #                     "left:", left_px + 2, "px; top:", top_px + 500, "px;")
  #
  #     # actual tooltip created as wellPanel
  #     wellPanel(
  #       style = style,
  #       p(HTML(paste0("Unit: ", point$name, " (", point$type, "-", point$status, ")", "<br/>",
  #                     (filter(renamekey, ACS == selected_vars$input_acs) %>% select(goodname)),
  #              ": ", point$value, "<br/>"))) #"Approx. ", "<b>", point$value, " </b>", "% of people", "<br/>",
  #                     # "<b> within </b>", point$name, "<br/>", point$hovtext)))
  #     )
  #   })
  #

  color_code <- data.frame(catg = c("Existing", "Planned", "Search"), color = c("#66C5A0", p_col, s_col))


  output$text_info <- renderUI({
    click <- input$plot_click
    point <- nearPoints(summary_util$facet_data, click, threshold = 10) # , maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) {
      return(NULL)
    }
    background_color <- color_code %>%
      filter(catg == point$status) %>%
      .[, 2]

    HTML(if (selected_vars$input_acs != "adj_meanhhi") {
      (
        (paste0(
          "<div style='background-color:", background_color, "'>", "Approx. ", "<b>", point$value, "%", "</b>",
          " of people within ", "<b>",
          (if (point$type == "avg") ("</b>") else (paste0(point$distance, " mi</b> of "))),
          "<b>", point$name,
          (if (point$type == "avg") ("</b>") else (paste0(" (", point$type, " - ", point$status, ") </b>"))),
          " fall into the ", "<b>", (filter(renamekey, ACS == selected_vars$input_acs) %>% select(goodname)), "</b> category.", "</br> </div>"
        ))
      )
    } else {
      (
        (paste0(
          "<div style='background-color:", background_color, "'>", "$", prettyNum(point$value, big.mark = ","), " is the approx. mean household income within ",
          (if (point$type == "avg") ("") else (paste0(point$distance, " mi of "))),
          point$name,
          (if (point$type == "avg") ("</b>") else (paste0(" (", point$type, " - ", point$status, ") </b> </div>"))),
          "."
        ))
      )
    })
  })
}



## To be copied in the UI
# mod_mod_summary_ggplot_ui("mod_summary_ggplot_ui_1")

## To be copied in the server
# callModule(mod_mod_summary_ggplot_server, "mod_summary_ggplot_ui_1")
