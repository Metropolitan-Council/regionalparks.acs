
plotly_test_data <- read_csv("data-raw/plotly_test_data.csv") %>% 
  left_join(renamekey, by = c("ACS" = "ACS variable")) %>% 
  mutate(acs_short = stringr::str_remove(goodname, "% " )) %>% 
  mutate(hover_text = stringr::str_wrap(paste0("Approximatley ",
                             "<b>", value, "%", "</b>", " of the population within ", 
                             "<b>", distance, " mile", "</b>",
                             " falls into the ",
                             "<b>", acs_short, "</b>",
                             " category"), 55
  ))


x_title <- renamekey %>% 
  filter(`ACS variable` == unique(plotly_test_data$ACS))

font_family_list <- "Roman, Helvetica, Tahoma, Geneva, Arial, sans-serif"



plot_ly() %>% 
  plotly::add_markers(data = plotly_test_data,
                      x = ~value,
                      y = ~name,
                      symbol = ~type,
                      color = ~status,
                      symbols = c('circle', 'square'),
                      colors = c(e_col, p_col, s_col),
                      hoverinfo = "text",
                      text = ~hover_text,
                      marker = list(
                        size = 10
                      )) %>% 
  layout(
    margin = list(l = 10, r = 45, b = 10, t = 10), # l = left; r = right; t = top; b = bottom
    hovermode = "closest",
    hoverdistance = "10",
    hoverlabel = list(
      font = list(
        size = 20,
        family = font_family_list,
        color = "black"
      ),
      bgcolor = "white",
      stroke = list("#eee", "#eee", "#eee", "#eee")
    ),
    
    xaxis = list(
      title = unique(plotly_test_data$goodname),
      tickfont = list(
        size = 12,
        family = font_family_list,
        color = "black"
      ),
      zeroline = FALSE,
      showline = FALSE,
      showgrid = TRUE,
      ticksuffix = "%"
      
      # range = c("2010-01-01", "2022-01-01")
    ),
    yaxis = list(
      title = "",
      tickfont = list(
        size = 12,
        family = font_family_list,
        color = "black"
      ),
      zeroline = FALSE,
      showline = FALSE,
      showgrid = TRUE
      # range = c("2010-01-01", "2022-01-01")
    )
    
    
  )
