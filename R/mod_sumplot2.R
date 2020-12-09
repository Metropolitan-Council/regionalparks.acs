#' sumplot2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import plotly
#' @import councilR
#' @import dplyr
#' @import ggplot2
#' @import tibble
#' @import cowplot
#' @import tidyr
#' @import stringr
#' @import forcats

mod_sumplot2_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("SUMPLOT"), height = 700)
    
  )
}
    
#' sumplot2 Server Function
#'
#' @noRd 
mod_sumplot2_server <- function(input, output, session, vals2 = vals2){
  ns <- session$ns
 
  # output$SUMPLOT <-  observeEvent(vals2, {
  #   renderPlotly({
  #     ggplotly(
  #       vals2() %>%
  #       ggplot(
  #         aes(
  #           y = name,
  #           x = value,
  #           pch = type,
  #           fill = status
  #         )
  #       ) +
  #         geom_point(
  #           col = "black",
  #           size = 4,
  #           position = position_dodge(width = 0)
  #         ) +
  #         scale_fill_manual(
  #           values = c(
  #             "Existing" = "#31a354",
  #             "Planned" = "#edd066",
  #             "Search" = "#de2d26"
  #           )
  #         ) +
  #         scale_shape_manual(values = c(
  #           "Park" = 21,
  #           "Trail" = 22
  #         )) +
  #         council_theme() +
  #         scale_x_continuous(
  #           labels = function(x)
  #             format(x, big.mark = ",",
  #                    scientific = FALSE)
  #         ) +
  #         guides(shape = F, fill = F) +
  #         theme(axis.text.y = element_text(size = 9)),
  #       tooltip = c("text")
  #     ) %>%
  #       hide_legend()
  #   })
  # })


  output$SUMPLOT <- renderPlotly({
    ggplotly(
      vals2() %>%
        # # filter(
        # #   # distance == input$distance,
        # #   agency %in% input$agency#,
        # #   # ACS == input$ACS,
        # #   # status %in% input$status,
        # #   # type %in% input$type
        # # ) %>%
        # separate(
        #   name,
        #   into = c("name", 'delete2'),
        #   sep = c("_")
      # ) %>%
      # mutate(name = str_replace_all(
      #   name,
      #   c(
      #     "Regional Park" = "RP",
      #     "Regional Trail" = "RT",
      #     "Park Reserve" = "PR"
      #   )
      # )) %>%
      # mutate(
      #   name = forcats::fct_reorder(name, desc(value)),
      #   concat = paste(type, status, sep = "_")
      # ) %>%
      ggplot(
        aes(
          y = name,
          x = value,
          pch = type,
          fill = status
        )
      ) +

        geom_point(
          col = "black",
          size = 4,
          position = position_dodge(width = 0)
        ) +
        scale_fill_manual(
          values = c(
            "Existing" = "#31a354",
            "Planned" = "#edd066",
            "Search" = "#de2d26"
          )
        ) +
        scale_shape_manual(values = c(
          "Park" = 21,
          "Trail" = 22
        )) +
        council_theme() +
        scale_x_continuous(
          labels = function(x)
            format(x, big.mark = ",",
                   scientific = FALSE)
        ) +
        guides(shape = F, fill = F) +
        theme(axis.text.y = element_text(size = 9)),
      tooltip = c("text")
    ) %>%
      hide_legend()
  })
  
}
    
## To be copied in the UI
# mod_sumplot2_ui("sumplot2_ui_1")
    
## To be copied in the server
# callModule(mod_sumplot2_server, "sumplot2_ui_1")
 
