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
    shiny::p("This plot provides summarized demographic values for all the regional parks and trails. Point location along the x-axis indicates the demographic value which can be compared across and within units or agencies. Color indicates unit status (green = existing, orange = planned, yellow = search). Shape indicates unit type (circle = park, square = trail). The subplot with the blue point indicates the average demographic value within agency boundaries."),

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
  #       data = summary_util$plot_buffer_data,
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
  #         title = unique(summary_util$plot_buffer_data$goodname),
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
  #         title = unique(summary_util$plot_buffer_data$goodname),
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
      plot_ly(height = nrow(summary_util$plotly_agency_data)*30) %>%
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
          margin = list(l = 10, r = 45, b = 0, t = 0, pad = 0), # l = left; r = right; t = top; b = bottom
          hovermode = "closest",
          hoverdistance = "10",
          hoverlabel = hoverlabel_list,
          xaxis = list(
            title = paste0(summary_util$plotly_agency_data$goodname),#(unique(summary_util$plot_buffer_data$goodname)),
            font = x_axis_font_list,
            tickfont = tickfont_list,
            zeroline = FALSE,
            showline = FALSE,
            showgrid = TRUE,
            ticksuffix = if (selected_vars$input_acs != "adj_meanhhi") {"%"} else{"$"},
            range = if(selected_vars$input_acs != "adj_meanhhi") {c(min(summary_util$plot_buffer_data %>%.$value)-1, max(summary_util$plot_buffer_data %>% .$value)+1)}
            else {c(min(summary_util$plot_buffer_data %>%.$value)-1000, max(summary_util$plot_buffer_data %>% .$value)+1000)}
          ),
          yaxis = list(
            title = "",
            font = y_axis_font_list,
            tickfont = tickfont_list,
            zeroline = FALSE,
            showline = FALSE,
            showgrid = TRUE#,
            # autorange = "reversed"
          )
        ),
      
      plot_ly(height = nrow(summary_util$plot_buffer_data[!duplicated(summary_util$plot_buffer_data[, c('name')]), ])*30) %>%
        plotly::add_markers(
          data = summary_util$plot_buffer_data,
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
          margin = list(l = 10, r = 45, b = 0, t = 0, pad = 0, autoexpand = T), # l = left; r = right; t = top; b = bottom
          hovermode = "closest",
          hoverdistance = "10",
          hoverlabel = hoverlabel_list,
          xaxis = list(
            title = unique(summary_util$plot_buffer_data$goodname),
            font = x_axis_font_list,
            
            tickfont = tickfont_list,
            zeroline = FALSE,
            showline = FALSE,
            showgrid = TRUE,
            ticksuffix = if (selected_vars$input_acs != "adj_meanhhi") {"%"} else{"$"},
            range  = if(selected_vars$input_acs != "adj_meanhhi") {c(min(summary_util$plot_buffer_data %>%.$value)-1, max(summary_util$plot_buffer_data %>% .$value)+1)}
            else {c(min(summary_util$plot_buffer_data %>%.$value)-1000, max(summary_util$plot_buffer_data %>% .$value)+1000)}
          ),
          yaxis = list(
            title = "",
            tickfont = tickfont_list,
            zeroline = FALSE,
            showline = FALSE,
            showgrid = TRUE#,
            # autorange = "reversed"
          )
        ),
      
      nrows = 2,
      margin =0, #.04, 
      heights=c(0.07, 0.93),
        # # (nrow(summary_util$plotly_agency_data))/(nrow(summary_util$plot_buffer_data)),
        # # (1 - (nrow(summary_util$plotly_agency_data))/(nrow(summary_util$plot_buffer_data)))),
        # 
        # (nrow(summary_util$plotly_agency_data) / ((nrow(summary_util$plot_buffer_data[!duplicated(summary_util$plot_buffer_data[, c('name')]), ])) + nrow(summary_util$plotly_agency_data))),
        # (1 - (nrow(summary_util$plotly_agency_data) / ((nrow(summary_util$plot_buffer_data[!duplicated(summary_util$plot_buffer_data[, c('name')]), ])) + nrow(summary_util$plotly_agency_data))))
        #   ),
        
      # horizontal_spacing = 0.05,
      # margin = .1,
      # margin = list(l = 10, r = 45, b = 10, t = 10, pad = 2), # l = left; r = right; t = top; b = bottom
      
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

}
## To be copied in the UI
# mod_summary_plot_ui("summary_plot_ui_1")

## To be copied in the server
# callModule(mod_summary_plot_server, "summary_plot_ui_1")
