#' summary_ggplotly UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_summary_ggplotly_ui <- function(id){
  ns <- NS(id)
  tagList(
    HTML("<p>This plot provides summarized demographic values for all the regional parks and trails. Point location along the x-axis indicates the demographic value which can be compared across and within units or agencies. Color indicates unit status (green = existing, orange = planned, yellow = search). Shape indicates unit type (circle = park, square = trail). The subplot with the blue point indicates the average demographic value within agency boundaries.</p>"),
    
    
    plotOutput(ns("leg"), height = 100),
    
    hr(),
    
    plotlyOutput(outputId = ns("Rggplots"))
  )
}
    
#' summary_ggplotly Server Function
#'
#' @noRd 
mod_summary_ggplotly_server <- function(input, output, session,
                                        selected_vars,
                                        summary_util){
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
  
  PlotHeight = reactive( #plot height ------
                         # #if want to set a minimum height
                         return(if ((nrow(summary_util$plot_buffer_data[!duplicated(summary_util$plot_buffer_data[, c('name')]), ])*30) > 200) {
                           (nrow(summary_util$plot_buffer_data[!duplicated(summary_util$plot_buffer_data[, c('name')]), ])*30)
                         } else {200}
                         
                         # return(nrow(summary_util$plot_buffer_data[!duplicated(summary_util$plot_buffer_data[, c('name')]), ])*30 + nrow(summary_util$plotly_agency_data)*30
                         )
  )
  
  
  output$Rggplots <- renderPlotly( { #height = function() PlotHeight(),
    p <- summary_util$facet_data %>% 
      ggplot(aes(y = name, 
                 x = value, #.[, 8], 
                 shape = type, 
                 fill = status)) +
      facet_grid(level ~., scales = "free_y", space = "free") + 
      # ggforce::facet_col(~ level, scales = "free_y", space = "free") + 
      geom_point(size = 4) +
      scale_shape_manual(values = c("avg" = 8, "Park" = 21, "Trail" = 22))+
      scale_fill_manual(values = c("Existing" = e_col, "Planned" = p_col, "Search" = s_col))+
      council_theme() +
      labs(y = "",
           x = filter(renamekey, ACS == selected_vars$input_acs) %>% select(goodname),
           title = paste0((filter(renamekey, ACS == selected_vars$input_acs) %>% select(goodname)), " - ", selected_vars$input_distance, " mi buffer")) +
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 14),
            strip.text = element_text(size = 14),
            strip.text.y = element_text(angle = 0),
            plot.title = element_text(size = 14, family = "Helvetica", face = "bold"),
            strip.background = element_rect(fill = "grey90"),
            panel.border = element_rect(color = "black", fill = NA),
            strip.placement = "outside",
            panel.grid.minor.x = element_blank()) +
      # annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
      # annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
      guides(shape = FALSE, fill = FALSE) +
      scale_x_continuous(labels = scales::comma)
    # scale_x_continuous(labels = function(x) if(selected_vars$input_acs == "adj_meanhhi") {
    #   paste0("$", x)} else {paste0(x, "%")})
    ggplotly(p)
  })
  
}
    
## To be copied in the UI
# mod_summary_ggplotly_ui("summary_ggplotly_ui_1")
    
## To be copied in the server
# callModule(mod_summary_ggplotly_server, "summary_ggplotly_ui_1")
 
