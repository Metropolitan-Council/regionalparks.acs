#' mod_summary_ggplot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_summary_ggplot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # shiny::p("This plot provides summarized demographic values for all the regional parks and trails. Point location along the x-axis indicates the demographic value which can be compared across and within units or agencies. Color indicates unit status (green = existing, orange = planned, yellow = search). Shape indicates unit type (circle = park, square = trail). Subplots indicate either average values within agency boundaries or unit-level values. (Right-click on image to copy or download. Click on any point to create a text-based interpretation.)"),


    # plotOutput(ns("leg"), height = 100),

    # hr(),

    uiOutput(ns("text_info")),
    plotOutput(
      outputId = ns("Rggplots"),
      click = hoverOpts(ns("plot_click"))
    ),
  )
}

#' mod_summary_ggplot Server Function
#'
#' @noRd
#' @import ggplot2
mod_summary_ggplot_server <- function(
  input,
  output,
  session,
  selected_vars,
  summary_util
) {
  ns <- session$ns


  ns <- session$ns
  w2 <- Waiter$new(ns("Rggplots")) # , html="Please wait")#, hide_on_render=T)



  ## legend -----
  output$leg <- renderPlot({
    plot_grid(type_status_legend)
  })

  PlotHeight <- reactive( # plot height ------
    # #if want to set a minimum height
    return(
      if ((nrow(summary_util$plot_buffer_data[!duplicated(summary_util$plot_buffer_data[, c("name")]), ]) * 33) > 400) {
        (nrow(summary_util$plot_buffer_data[!duplicated(summary_util$plot_buffer_data[, c("name")]), ]) * 33)
      } else {
        400
      }
    )
  )


  output$Rggplots <- renderPlot(height = function() PlotHeight(), {

    # w2$show()

    summary_util$facet_data %>%
      ggplot2::ggplot(aes(
        y = name,
        x = value,
        # .[, 8],
        shape = type,
        fill = status
      )) +
      facet_grid(level ~ ., scales = "free_y", space = "free") +
      geom_point(size = 8) +
      scale_shape_manual(
        values = c("avg" = 8, "Park" = 21, "Trail" = 22),
        labels = c("avg" = "Agency\naverage", "Park" = "Park", "Trail" = "Trail")
      ) +
      scale_fill_manual(values = c("Existing" = e_col, "Planned" = p_col, "Search" = s_col)) +
      council_theme() +
      labs(
        y = "",
        x = filter(name_helper, acscode == selected_vars$input_acs) %>% select(popuplab),
        title = paste0((filter(name_helper, acscode == selected_vars$input_acs) %>% select(popuplab)), " - ", selected_vars$input_distance, " mi buffer"),
        fill = "Unit status",
        shape = "Unit type" # ,
        # caption = ("\nMetropolitan Council, 12 Jan. 2020")
      ) +
      theme(
        # legend.key.size = unit(1.5, 'lines'),
        legend.text = element_text(margin = margin(t = 6, b = 6)),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14),
        strip.text.y = element_text(angle = 0),
        plot.title = element_text(size = 14, family = "Helvetica", face = "bold"),
        strip.background = element_rect(fill = "grey90"),
        panel.border = element_rect(color = "black", fill = NA),
        strip.placement = "outside",
        panel.grid.minor.x = element_blank(),
        plot.caption = element_text(size = 10) # ,
        # legend.position = "bottom", legend.box = "vertical"
      ) +
      guides(fill = guide_legend(override.aes = list(shape = 21))) +
      # guides(shape = FALSE, fill = FALSE) +
      scale_x_continuous(labels = scales::comma) +
      geom_stripes(odd = "#00000000", even = "#cfcfcf33")

    # w2$hide()
  })

  color_code <- data.frame(catg = c("Existing", "Planned", "Search"), color = c(e_col, p_col, s_col))


  output$text_info <- renderUI({
    click <- input$plot_click
    point <- nearPoints(summary_util$facet_data, click, threshold = 10) # , maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) {
      (
        return(HTML(paste0("<div style='font-size:1.8rem;padding:1%;background-color:#FFFFFF'>", "Click an icon ")))
      )
    }
    background_color <- color_code %>%
      filter(catg == point$status) %>%
      .[, 2]

    HTML(if (selected_vars$input_acs != "meanhhinc_per"){# "adj_meanhhi") {
      (
        (
          (paste0(
            "<div style='font-size:1.8rem;padding:1%;background-color:",
            background_color,
            "'>",
            "Approximately ",
            "<b>",
            point$value,
            "%",
            "</b>",
            " of people within ",
            "<b>",
            (if (point$type == "avg") ("</b>") else (paste0(point$distance, " mi</b> of "))),
            "<b>",
            point$name,
            (if (point$type == "avg") ("</b>") else (paste0(" (", point$type, " - ", point$status, ", ", point$agency, ") </b>"))),
            " fall into the ",
            "<b>",
            (filter(name_helper, acscode == selected_vars$input_acs) %>% select(popuplab)),
            "</b> category.",
            "</br> </div>"
          ))
        )
      )
    } else {
      (
        (
          (paste0(
            "<div style='font-size:1.8rem;padding:1%;background-color:",
            background_color,
            "'>",
            "$",
            prettyNum(point$value, big.mark = ","),
            " is the approximate mean household income within ",
            (if (point$type == "avg") ("") else (paste0(point$distance, " mi of "))),
            point$name,
            (if (point$type == "avg") ("</b>") else (paste0(" (", point$type, " - ", point$status, ", ", point$agency, "). </b> </div>")))
          ))
        )
      )
    })
  })
}



## To be copied in the UI
# mod_summary_ggplot_ui("mod_summary_ggplot_ui_1")

## To be copied in the server
# callModule(mod_summary_ggplot_server, "mod_summary_ggplot_ui_1")
