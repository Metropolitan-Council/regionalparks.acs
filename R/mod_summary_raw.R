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
    
    HTML('<p>Something about methods...</p> <br>'),
    hr(),
    HTML('<p>This plot is inteded for advanced users, and shows the methodology behind the weighted average summary values. Points show the values from <strong>all</strong> Census block groups which intersect the buffer zone around regional parks and trails. Point fill indicates the percent of the block which overlaps the buffer zone (lighter grey = lower percent overlap, darker grey = higher percent overlap). Color indicates park/trail status (green = existing, yellow = planned, red = search). Shape indicates park/trail type (circle = park, square = trail). Park/trail names are shortened ("RP" = regional park, "RT" = regional trail, "PR" = park reserve) as is park/trail status ("EP" = existing park, "PP" = planned park, "SP" = search park, "ET" = existing trail, "PT" = planned trail, "ST" = search trail).</p>'),
    
    plotOutput(ns("rangeleg"), height = 100),
    
    plotlyOutput(outputId = ns("plotlyrange"), height = 850) 
  )
}

#' summary_raw Server Function
#'
#' @noRd 
mod_summary_raw_server <- function(input, output, session,
                                   summary_util){
  ns <- session$ns
  
  
  renamekey <- tribble(
    ~ goodname,
    ~ "ACS",
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
            "Existing" = e_col,
            "Planned" = p_col,
            "Search" = s_col
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
        labs(fill = "% of block\ngroup within\nbuffer: ",
             col = "Status: ",
             shape = "Type: ") +
        theme(legend.position = "bottom")
    )
  
  output$rangeleg <- renderPlot({
    plot_grid(type_range_legend)
  })
  
  
  
  output$plotlyrange <- renderPlotly({
    ggplotly(summary_util$plot_rawbuffer_data  %>%
               as_tibble() %>%
               separate(name,
                        into = c("name", 'delete2'),
                        sep = c("_")) %>%
               mutate(
                 name = paste(name, status, sep = "; "),
                 name = paste(name, type, sep = " ")
               ) %>%
               mutate(name = str_replace_all(
                 name,
                 c(
                   "Regional Park" = "RP",
                   "Regional Trail" = "RT",
                   "Park Reserve" = "PR",
                   "Existing Park" = "EP",
                   "Planned Park" = "PP",
                   "Search Park" = "SP",
                   "Existing Trail" = "ET",
                   "Planned Trail" = "PT",
                   "Search Trail" = "ST"
                 )
               )) %>%
               mutate(
                 cat_coverage = case_when(
                   coverage < .25 ~ "0-25%",
                   coverage >= .25 &
                     coverage < .5 ~ "25-50%",
                   coverage >= .5 &
                     coverage < .75 ~ "50-75%",
                   coverage >= .75 &
                     coverage <= 1 ~ "75-100%"
                 )
               ) %>%
               mutate(value = if_else(ACS != "adj_meanhhi", value*100, value)) %>%
               mutate(name = forcats::fct_reorder(name, desc(name))) %>%
               ggplot(aes(
                 y = name,
                 x = value,
                 col = status,
                 fill = cat_coverage,
                 pch = type,
                 text = paste0(
                   ((summary_util$plot_rawbuffer_data)[1,8] %>%
                      left_join(renamekey, by = "ACS") %>%
                      select(goodname)), ":  ",
                   value, "\n Overlap: ",
                   round(coverage*100, 1), "%")
               )
               ) +
               geom_jitter(
                 height = 0.2,
                 width = 0,
                 size = 2,
                 # stroke = .75
               ) +
               council_theme() +
               scale_fill_brewer(palette = "Greys") +
               scale_shape_manual(values = c("Park" = 21,
                                             "Trail" = 22)) +
               scale_color_manual(
                 values = c(
                   "Existing" = e_col,
                   "Planned" = p_col,
                   "Search" = s_col
                 )
               ) +
               guides(
                 fill = guide_legend(override.aes = list(
                   pch = 23, size = 8
                 )),
                 pch = guide_legend(override.aes = list(size = 8)),
                 color = guide_legend(override.aes = list(
                   pch = 18, size = 8
                 ))
               ) +
               labs(
                 y = "",
                 x = paste0((summary_util$plot_rawbuffer_data)[1,8] %>%
                              left_join(renamekey, by = "ACS") %>%
                              select(goodname)),
                 fill = "Percent of\nblock within\nbuffer",
                 col = "Status",
                 shape = "Type"
               ) +
               scale_x_continuous(
                 labels = function(x)
                   format(x, big.mark = ",",
                          scientific = FALSE)
               ) +
               theme(axis.text.y = element_text(size = 9)),
             tooltip = c("text")
    ) %>%
      hide_legend()
  })
  
}


## To be copied in the UI
# mod_summary_raw_ui("summary_raw_ui_1")

## To be copied in the server
# callModule(mod_summary_raw_server, "summary_raw_ui_1")
