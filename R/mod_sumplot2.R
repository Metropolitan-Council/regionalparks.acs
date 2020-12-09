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
mod_sumplot2_server <- function(input, output, session, filtered_df2){ #vals2 = vals2){
  ns <- session$ns
 
  output$SUMPLOT <- renderPlotly({
    ggplotly(
      filtered_df2() %>%
        separate(
          name,
          into = c("name", 'delete2'),
          sep = c("_")
        ) %>%
        mutate(name = str_replace_all(
          name,
          c(
            "Regional Park" = "RP",
            "Regional Trail" = "RT",
            "Park Reserve" = "PR"
          )
        )) %>%
        mutate(
          name = forcats::fct_reorder(name, desc(value)),
          concat = paste(type, status, sep = "_")
        ) %>%
        ggplot(
          aes(
            y = name,
            x = value,
            pch = type,
            fill = status,
            text = paste0((filter(renamekey, `ACS variable` %in% filtered_df2()[1,6]) %>% select(goodname)),
                          ":  ", value, "\n", name)
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
        labs(y = "", 
             x = paste0(filter(renamekey, `ACS variable` %in% filtered_df2()[1,6]) %>% select(goodname)))+
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
 
