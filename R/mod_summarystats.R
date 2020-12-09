####### NEED CODE TO CREATE long_buffer_data

#' summarystats UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import tibble
#' @import ggplot2
#' @import cowplot
#' @import plotly
#' @import tidyr
#' @import stringr
#' @import forcats
#' @import dplyr


## set up some legends -----

renamekey <- tribble(
  ~ goodname,
  ~ "ACS variable",
  "Total population",
  "adj_poptotal",
  "Age, % under 15" ,
  "adj_ageunder15_per",
  "Age, % 15-24" ,
  "adj_age15_24_per",
  "Age, % 25-64" ,
  "adj_age25_64_per",
  "Age, % 65 and up" ,
  "adj_age65up_per",
  "Race, % White" ,
  "adj_whitenh_per",
  "Race, % Black" ,
  "adj_blacknh_per",
  "Race, % Asian" ,
  "adj_asiannh_per",
  "Race, % American Indian" ,
  "adj_amindnh_per",
  "Race, % Other + Multiracial" ,
  "adj_othermultinh_per",
  "Ethnicity, % Hispanic" ,
  "adj_hisppop_per",
  "Ethnicity, % not-Hispanic" ,
  "adj_nothisppop_per",
  "Mean household income" ,
  "adj_meanhhi",
  "% Housholds without a vehicle",
  "adj_novehicle_per",
  "% speaking English less than very well",
  "adj_lep_per",
  "% Spanish speakers",
  "adj_span_per"
)


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

#####

mod_summarystats_ui <- function(id){
  ns <- NS(id)
  tagList(
    # selectInput(ns("agency"),label = "Choose some columns", choices = NULL, multiple = TRUE),
    # tableOutput(ns("table")),
    
    # HTML('<p>This plot is indeted to provide summarized demographic values for all the regional parks and trails. Point location along the x-axis indicates the demographic value which can be compared across and within park/trail status (existing, planned, search) or agencies. Color indicates park/trail status (green = existing, yellow = planned, red = search). Shape indicates park/trail type (circle = park, square = trail). The solid black, vertical line indicates the average demographic value within agency boundaries.</p>'),
    
    plotOutput(ns("leg"), height = 100),
    plotlyOutput(ns("weightedav"), height = 700)
    
  )
}
    
#' summarystats Server Function
#'
#' @noRd 
mod_summarystats_server <- function(input, output, session, filtered_df){
  ns <- session$ns
  
  output$leg <- renderPlot({
    plot_grid(type_status_legend)
  })
  
  output$weightedav <- renderPlotly({
    ggplotly(
      filtered_df() %>%
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
            text = paste0((filter(renamekey, `ACS variable` %in% filtered_df()[1,6]) %>% select(goodname)),
                          ":  ", value, "\n", name)
          )
        ) +
        # geom_vline(aes(xintercept = value), 
        #            data = (agency_avg %>%
        #                      filter(agency %in% input$agency,
        #                             ACS == input$ACS))) +
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
             x = paste0(filter(renamekey, `ACS variable` %in% filtered_df()[1,6]) %>% select(goodname)))+
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
# mod_summarystats_ui("summarystats_ui_1")
    
## To be copied in the server
# callModule(mod_summarystats_server, "summarystats_ui_1")
 
