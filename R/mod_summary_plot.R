#' summary_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_summary_plot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    HTML("<p>This plot provides summarized demographic values for all the regional parks and trails. Point location along the x-axis indicates the demographic value which can be compared across and within units or agencies. Color indicates unit status (green = existing, orange = planned, yellow = search). Shape indicates unit type (circle = park, square = trail). The subplot with the blue point indicates the average demographic value within agency boundaries.</p>"),


    # textOutput(ns("avgtext")),

    plotOutput(ns("leg"), height = 100),
    
    hr(),
    # plotlyOutput(outputId = ns("output_plot"), height = 600), # height = as.numeric(unlist(textOutput(ns("TEST"))[[1]]))*25) #this doesn't work, but I'd love to do something like this

    # plotlyOutput(outputId = ns("agency_plot"), height = 300)

    # uiOutput(ns("height")),
    
    plotlyOutput(outputId = ns("subplotlys"))#, height = uiOutput(ns("height")))#900)
  )
}

#' summary_plot Server Function
#'
#' @noRd
mod_summary_plot_server <- function(input, output, session,
                                    selected_vars,
                                    summary_util) {
  ns <- session$ns

  
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
  
  # Plotly styling -------------------------------

  font_family_list <- "Roman, Helvetica, Tahoma, Geneva, Arial, sans-serif"

  x_axis_font_list <- list(
    size = 14,
    family = font_family_list,
    color = "black"
  )

  y_axis_font_list <- list(
    size = 14,
    family = font_family_list,
    color = "black"
  )


  hoverlabel_list <- list(
    font = list(
      size = 18,
      family = font_family_list,
      color = "black"
    ),
    bgcolor = "white",
    brodercolor = list(rep("#FFFFFF", 4))
  )

  tickfont_list <- list(
    size = 12,
    family = font_family_list,
    color = "black"
  )

  
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




  # output$output_plot <- renderPlotly({ # main plotly ----
  #   plot_ly() %>%
  #     plotly::add_markers(
  #       data = summary_util$plotly_buffer_data,
  #       x = ~value,
  #       y = ~name,
  #       symbol = ~type,
  #       color = ~status,
  #       symbols = c("circle", "square"),
  #       colors = c(e_col, p_col, s_col),
  #       hoverinfo = "text",
  #       text = ~hover_text,
  #       marker = list(
  #         size = 14,
  #         opacity = 0.8
  #       )
  #     ) %>%
  #     layout(
  #       showlegend = FALSE,
  #       margin = list(l = 10, r = 45, b = 10, t = 10), # l = left; r = right; t = top; b = bottom
  #       hovermode = "closest",
  #       hoverdistance = "10",
  #       hoverlabel = hoverlabel_list,
  #       xaxis = list(
  #         title = unique(summary_util$plotly_buffer_data$goodname),
  #         font = x_axis_font_list,
  # 
  #         tickfont = tickfont_list,
  #         zeroline = FALSE,
  #         showline = FALSE,
  #         showgrid = TRUE,
  #         ticksuffix = "%"
  #       ),
  #       yaxis = list(
  #         title = "",
  #         tickfont = tickfont_list,
  #         zeroline = FALSE,
  #         showline = FALSE,
  #         showgrid = TRUE,
  #         autorange = "reversed"
  #         )
  #     )
  # })
  # 

  # output$agency_plot <- renderPlotly({ # agency plotly -----
  #   plot_ly() %>%
  #     plotly::add_markers(
  #       data = summary_util$plotly_agency_data,
  #       x = ~avg,
  #       y = ~agency,
  #       hoverinfo = "text",
  #       text = ~hover_text,
  #       marker = list(
  #         size = 14,
  #         opacity = 0.8
  #       )
  #     ) %>%
  #     layout(
  #       showlegend = FALSE,
  #       margin = list(l = 10, r = 45, b = 10, t = 10), # l = left; r = right; t = top; b = bottom
  #       hovermode = "closest",
  #       hoverdistance = "10",
  #       hoverlabel = hoverlabel_list,
  #       xaxis = list(
  #         title = unique(summary_util$plotly_buffer_data$goodname),
  #         font = x_axis_font_list,
  #         tickfont = tickfont_list,
  #         zeroline = FALSE,
  #         showline = FALSE,
  #         showgrid = TRUE,
  #         ticksuffix = "%"
  #       ),
  #       yaxis = list(
  #         title = "",
  #         font = y_axis_font_list,
  #         tickfont = tickfont_list,
  #         zeroline = FALSE,
  #         showline = FALSE,
  #         showgrid = TRUE,
  #         autorange = "reversed"
  #       )
  #     )
  # })
  # 

  
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
            ticksuffix = "%",
            range = c(min(summary_util$plotly_buffer_data %>%.$value)-1, max(summary_util$plotly_buffer_data %>% .$value)+1)
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
            ticksuffix = "%",
            range = c(min(summary_util$plotly_buffer_data %>%.$value)-1, max(summary_util$plotly_buffer_data %>% .$value)+1)
          ),
          yaxis = list(
            title = "",
            tickfont = tickfont_list,
            zeroline = FALSE,
            showline = FALSE,
            showgrid = TRUE,
            autorange = "reversed"
            # range = c("2010-01-01", "2022-01-01")
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
  
  # text agency avg ----
  output$avgtext <- renderText(
    (paste0(
      "\nAverage ", '"', (summary_util$plot_buffer_data[1, 6] %>%
        left_join(renamekey, by = "ACS") %>%
        select(goodname)), '" within "',
      summary_util$agencyavg_data$agency, '":  ',
      summary_util$agencyavg_data$value, "       ", "\n", "dafasf"
    ) # hmm, my new line isn't working
    )
  )
}

## To be copied in the UI
# mod_summary_plot_ui("summary_plot_ui_1")

## To be copied in the server
# callModule(mod_summary_plot_server, "summary_plot_ui_1")
