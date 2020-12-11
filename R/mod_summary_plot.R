#' summary_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import cowplot
#' 




##### ui ----------
mod_summary_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
 
    HTML('<p>This plot is indeted to provide summarized demographic values for all the regional parks and trails. Point location along the x-axis indicates the demographic value which can be compared across and within park/trail status (existing, planned, search) or agencies. Color indicates park/trail status (green = existing, yellow = planned, red = search). Shape indicates park/trail type (circle = park, square = trail). The solid black, vertical line indicates the average demographic value within agency boundaries.</p>'),
    plotOutput(ns("leg"), height = 100),
    plotOutput(ns("leg2")),
    # plotlyOutput("weightedav", height = 700),
    hr(),
    HTML("Average values within agency boundaries: "),
    # plotlyOutput("agencyav", height = 200)
    
  )
}
    
### server -----------
#' summary_plot Server Function
#'
#' @noRd 
mod_summary_plot_server <- function(input, output, session){
  ns <- session$ns
 
  
  type_status_legend <-
    get_legend(
      tibble(
        status = rep(c("Existing", "Planned", "Search"), 2),
        type = rep(c("park", "trail"), each = 3),
        location = rep(1, 6)
      ) %>%
        ggplot(aes(
          fill = status,
          pch = type,
          x = location,
          y = status
        )) +
        geom_point() +
        scale_shape_manual(values = c("park" = 21,
                                      "trail" = 22)) +
        scale_fill_manual(
          values = c(
            "Existing" = "#31a354",
            "Planned" = "#edd066",
            "Search" = "#de2d26"
          )
        ) +
        theme_cowplot() +
        guides(
          fill = guide_legend(
            override.aes = list(pch = 23, size = 8),
            label.position = "bottom"
          ),
          shape = guide_legend(override.aes = list(size = 8),
                               label.position = "bottom")
        ) +
        labs(fill = "        Status:", shape = "Type:") +
        theme(legend.position = "bottom")
    )
  
  output$leg <- renderPlot({
    plot_grid(type_status_legend)
  })
  
  output$leg2 <- renderPlot({
      tibble(
        status = rep(c("Existing", "Planned", "Search"), 2),
        type = rep(c("park", "trail"), each = 3),
        location = rep(1, 6)
      ) %>%
        ggplot(aes(
          fill = status,
          pch = type,
          x = location,
          y = status
        )) +
        geom_point() +
        scale_shape_manual(values = c("park" = 21,
                                      "trail" = 22)) +
        scale_fill_manual(
          values = c(
            "Existing" = "#31a354",
            "Planned" = "#edd066",
            "Search" = "#de2d26"
          )
        ) +
        theme_cowplot() +
        guides(
          fill = guide_legend(
            override.aes = list(pch = 23, size = 8),
            label.position = "bottom"
          ),
          shape = guide_legend(override.aes = list(size = 8),
                               label.position = "bottom")
        ) +
        labs(fill = "        Status:", shape = "Type:") +
        theme(legend.position = "bottom")
  })
  
}
    
## To be copied in the UI
# mod_summary_plot_ui("summary_plot_ui_1")
    
## To be copied in the server
# callModule(mod_summary_plot_server, "summary_plot_ui_1")
 
