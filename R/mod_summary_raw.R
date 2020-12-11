#' summary_raw UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_summary_raw_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    HTML('<p>This plot is inteded for advanced users. Points show the values from <strong>all</strong> Census block groups which intersect the buffer zone around regional parks and trails. Point fill indicates the percent of the block which overlaps the buffer zone (lighter grey = lower percent overlap, darker grey = higher percent overlap). Color indicates park/trail status (green = existing, yellow = planned, red = search). Shape indicates park/trail type (circle = park, square = trail). Park/trail names are shortened ("RP" = regional park, "RT" = regional trail, "PR" = park reserve) as is park/trail status ("EP" = existing park, "PP" = planned park, "SP" = search park, "ET" = existing trail, "PT" = planned trail, "ST" = search trail).</p>'),
 
    plotOutput(ns("rangeleg"), height = 100)
  )
}
    
#' summary_raw Server Function
#'
#' @noRd 
mod_summary_raw_server <- function(input, output, session){
  ns <- session$ns
  
  type_range_legend <-
    get_legend(
      tibble(
        status = rep(c("Existing", "Planned", "Search"), 8),
        type = rep(c("Park", "Trail"), each = 12),
        coverage = rep(c("0-25%", "25-50%", "50-75%", "75-100%"), each = 3, 2),
        location = rep(1, 24)
      ) %>%
        ggplot(aes(
          col = status,
          pch = type,
          fill = coverage,
          x = location,
          y = status
        )) +
        geom_point() +
        scale_fill_brewer(palette = "Greys") +
        scale_shape_manual(values = c("Park" = 21,
                                      "Trail" = 22)) +
        scale_color_manual(
          values = c(
            "Existing" = "#2ec799",
            "Planned" = "#f77614",
            "Search" = "#9591c9"
          )
        ) +
        theme_cowplot() +
        guides(
          fill = guide_legend(override.aes = list(pch = 23, size = 8),
                              label.position = "bottom"),
          pch = guide_legend(override.aes = list(size = 8),
                             label.position = "bottom"),
          color = guide_legend(override.aes = list(pch = 18, size = 8),
                               label.position = "bottom")
        ) +
        labs(fill = "% of\nblock within\nbuffer: ",
             col = "Status: ",
             shape = "Type: ") +
        theme(legend.position = "bottom")
    )
  
  output$rangeleg <- renderPlot({
    plot_grid(type_range_legend)
  })
 
}
    
## To be copied in the UI
# mod_summary_raw_ui("summary_raw_ui_1")
    
## To be copied in the server
# callModule(mod_summary_raw_server, "summary_raw_ui_1")
 
