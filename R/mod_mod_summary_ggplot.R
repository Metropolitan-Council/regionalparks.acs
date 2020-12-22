#' mod_summary_ggplot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_mod_summary_ggplot_ui <- function(id){
  ns <- NS(id)
  tagList(
    HTML("<p>This plot provides summarized demographic values for all the regional parks and trails. Point location along the x-axis indicates the demographic value which can be compared across and within units or agencies. Color indicates unit status (green = existing, orange = planned, yellow = search). Shape indicates unit type (circle = park, square = trail). The subplot with the blue point indicates the average demographic value within agency boundaries.</p>"),
    
  
    plotOutput(ns("leg"), height = 100),
    
    hr(),

    plotlyOutput(outputId = ns("ggplots"))
    
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
      scale_shape_manual(values = c(
        "park" = 21,
        "trail" = 22
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
  
  
  output$subplotlys <- renderPlotly({ # plotlys using subplots ----
    subplot(
      plot_ly(height = nrow(summary_util$plotly_agency_data)*25) %>%
        plotly::add_markers(
          data = summary_util$plotly_agency_data,
          x = ~value, #avg,
          y = ~agency,
          hoverinfo = "text",
          text = ~hover_text,
          marker = list(
            size = 10,
            opacity = 0.8
          )
        ) %>%
        layout(
          # title = "Agency average",
          showlegend = FALSE,
          margin = list(l = 10, r = 45, b = 10, t = 10), # l = left; r = right; t = top; b = bottom
          hovermode = "closest",
          hoverdistance = "10",
          hoverlabel = hoverlabel_list,
          xaxis = list(
            title = paste0(summary_util$plotly_agency_data$goodname),#(unique(summary_util$plotly_buffer_data$goodname)),
            font = x_axis_font_list,
            tickfont = tickfont_list,
            zeroline = FALSE,
            showline = FALSE,
            showgrid = TRUE,
            ticksuffix = if (selected_vars$input_acs != "adj_meanhhi") {"%"} else{"$"},
            range = if(selected_vars$input_acs != "adj_meanhhi") {c(min(summary_util$plotly_buffer_data %>%.$value)-1, max(summary_util$plotly_buffer_data %>% .$value)+1)}
            else {c(min(summary_util$plotly_buffer_data %>%.$value)-1000, max(summary_util$plotly_buffer_data %>% .$value)+1000)}
          ),
          yaxis = list(
            title = "",
            font = y_axis_font_list,
            tickfont = tickfont_list,
            zeroline = FALSE,
            showline = FALSE,
            showgrid = TRUE,
            autorange = "reversed"
          )
        ),
      
      plot_ly(height = nrow(summary_util$plotly_buffer_data)*25) %>%
        plotly::add_markers(
          data = summary_util$plotly_buffer_data,
          x = ~value,
          y = ~name,
          symbol = ~type,
          color = ~status,
          symbols = c("circle", "square"),
          colors = c(e_col, p_col, s_col),
          hoverinfo = "text",
          text = ~hover_text,
          marker = list(
            size = 10,
            opacity = 0.8,
            line = list(color = "black", width = 1)
          )
        ) %>%
        layout(
          showlegend = FALSE,
          margin = list(l = 10, r = 45, b = 10, t = 10), # l = left; r = right; t = top; b = bottom
          hovermode = "closest",
          hoverdistance = "10",
          hoverlabel = hoverlabel_list,
          xaxis = list(
            title = unique(summary_util$plotly_buffer_data$goodname),
            font = x_axis_font_list,
            
            tickfont = tickfont_list,
            zeroline = FALSE,
            showline = FALSE,
            showgrid = TRUE,
            ticksuffix = if (selected_vars$input_acs != "adj_meanhhi") {"%"} else{"$"},
            range  = if(selected_vars$input_acs != "adj_meanhhi") {c(min(summary_util$plotly_buffer_data %>%.$value)-1, max(summary_util$plotly_buffer_data %>% .$value)+1)}
            else {c(min(summary_util$plotly_buffer_data %>%.$value)-1000, max(summary_util$plotly_buffer_data %>% .$value)+1000)}
          ),
          yaxis = list(
            title = "",
            tickfont = tickfont_list,
            zeroline = FALSE,
            showline = FALSE,
            showgrid = TRUE,
            autorange = "reversed"
          )
        ),
      
      nrows = 2,
      
      heights=c(0.07, 0.93),
      shareX = F,
      shareY = F, 
      titleX = T, 
      titleY = T)#, ,shareX=F,shareY=F,titleX=T,titleY=T
    # height = nrow(summary_util$plotly_height))
  })
  
  
  ## legend -----
  output$leg <- renderPlot({
    plot_grid(type_status_legend)
  })
  
  
  # dynamic height----
  output$TEST <- renderText(
    nrow(summary_util$plot_buffer_data)
  )
  
  output$height <- renderText(
    summary_util$plotly_height[[1]]
  )
  
  renamekey <- tibble::tribble(
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
  
  output$ggplots <- renderPlotly({ # ggplots ----
    
    ggplotly(summary_util$plot_buffer_data %>%
   # long_buffer_data %>% filter(agency == "Anoka County", distance == 1, ACS == "adj_anydis_per")%>%
   #    separate(
   #      name,
   #      into = c("name", "delete2"),
   #      sep = c("_")
   #    )  %>%
   #    left_join(renamekey, by = c("ACS" = "ACS")) %>%
   #    mutate(acs_short = stringr::str_remove(goodname, "% ")) %>%
   #    mutate(hover_text = stringr::str_wrap(paste0(
   #      "Approx. ",
   #      "<b>", value, "%", "</b>", " of the pop. within ",
   #      distance, " mile of ",
   #      name, " (", status, ")", " falls into the ",
   #      "<b>", acs_short, "</b>",
   #      " category"
   #    ), 55)) %>%
   #    mutate(name = str_replace_all(
   #      name,
   #      c(
   #        "Regional Park" = "RP",
   #        "Regional Trail" = "RT",
   #        "Park Reserve" = "PR",
   #        "Special Recreation Feature" = "SRF"
   #      )
   #    )) %>% 
   #    mutate(
   #      name = forcats::fct_reorder(name, desc(value))
   #    ) %>%
      
      
      ggplot(aes(x = value, y = name, fill = status, shape = type)) + 
      geom_point(size = 4) +
      scale_fill_manual(values = c("Existing" = e_col, "Planned" = p_col, "Search" = s_col)) +
      scale_shape_manual(values = c("Park" = 21, "Trail" = 22)) +
      labs(y = "")+#, x = selected_vars$input_acs) +
      guides(fill = F, shape = F) +
      council_theme())
    
  })
}

    
## To be copied in the UI
# mod_mod_summary_ggplot_ui("mod_summary_ggplot_ui_1")
    
## To be copied in the server
# callModule(mod_mod_summary_ggplot_server, "mod_summary_ggplot_ui_1")
 
