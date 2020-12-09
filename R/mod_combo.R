#' combo UI Function
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
#' 

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


mod_combo_ui <- function(id){
  ns <- NS(id)
  tagList(
    # id = ns("sum_CHOICE"),
hr(),
    fluidPage(
      fluidRow(
        column(
          width = 3,
          selectizeInput(
            ns("ACS"),
            label = h4("ACS variable"),
            choices = list(
              `Age` = list(
                "Age, % under 15" = "adj_ageunder15_per",
                "Age, % 15-24" = "adj_age15_24_per",
                "Age, % 25-64" = "adj_age25_64_per",
                "Age, % 65+" = "adj_age65up_per"
              ),
              `Race` = list(
                "Race, % Am. Indian" = "adj_amindnh_per",
                "Race, % Asian" = "adj_asiannh_per",
                "Race, % Black" = "adj_blacknh_per",
                "Race, % Other + Multi" = "adj_othermultinh_per",
                "Race, % White" = "adj_whitenh_per"
              ),
              `Ethnicity` = list(
                "Ethnicity, % Hispanic" = "adj_hisppop_per",
                "Ethnicity, % not-Hispanic" = "adj_nothisppop_per"
              ),
              `Income` = list("Mean household income ($)" = "adj_meanhhi"),
              `Transportation` = list("% Housholds without a vehicle" = "adj_novehicle_per"),
              `Language` = list("% speaking English less than very well" = "adj_lep_per",
                                "% Spanish speakers" = "adj_span_per")
            ),
            selected = "adj_ageunder15_per"
          )),
        column(
          width = 3,
          selectInput(
            ns("agency"),
            label = h4("Agenc(y/ies)"),
            choices = c(
              "Anoka County",
              "Bloomington",
              "Carver County",
              "Dakota County",
              "MPRB",
              "Ramsey County",
              "Scott County",
              "St. Paul",
              "Three Rivers",
              "Washington County"
            ),
            selected = "Anoka County",
            multiple = TRUE
          )),
        column(
          width = 2,
          radioButtons(
            ns("distance"),
            label = h4("Buffer dist. (mi)"),
            choices = c(1.0, 1.5, 3),
            selected = c(1.0)
          )),
        column(width = 2,
               checkboxGroupInput(
                 ns("type"),
                 label = h4("Type"),
                 choices = c("Park", "Trail"),
                 selected = c("Park", "Trail")
               )),
        column(width = 2,
               checkboxGroupInput(
                 ns("status"),
                 label = h4("Status"),
                 choices = c("Existing", "Planned", "Search"),
                 selected = c("Existing", "Planned", "Search")
               ))
      )
    ),
    hr(),
    tabsetPanel( selected = "Weighted averages",
                 tabPanel("Weighted averages",
                          HTML('<p>This plot is indeted to provide summarized demographic values for all the regional parks and trails. Point location along the x-axis indicates the demographic value which can be compared across and within park/trail status (existing, planned, search) or agencies. Color indicates park/trail status (green = existing, yellow = planned, red = search). Shape indicates park/trail type (circle = park, square = trail). The solid black, vertical line indicates the average demographic value within agency boundaries.</p>'),
                           plotOutput(ns("leg"), height = 100),
                           plotlyOutput(ns("comboplot"), height = 700)
                           ),
                 tabPanel("Buffer map",
                          HTML('<p>This map visualizes the geospatial location of the buffers around the user-selected parks and trails. Demographic data can also be shown here.  THIS MAP IS NOT REACTIVE AT THE MOMENT (but will be updated)')),
                 tabPanel("Download tabular data"),
                 tabPanel("Methods & raw data"))

  )
}
    
#' combo Server Function
#'
#' @noRd 
mod_combo_server <- function(input, output, session){
  ns <- session$ns
 
  output$leg <- renderPlot({
    plot_grid(type_status_legend)
  })
  
  
  filterData3 = reactiveVal(long_buffer_data)
  filtered_df3 <- reactive({
    res <- filterData3() %>% filter(ACS == input$ACS,
                                   agency %in% input$agency,
                                   distance == input$distance,
                                   type %in% input$type,
                                   status %in% input$status)
    res
  })
  
  
  output$comboplot <- renderPlotly({
    ggplotly(
      filtered_df3() %>%
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
            text = paste0((filter(renamekey, `ACS variable` %in% filtered_df3()[1,6]) %>% select(goodname)),
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
             x = paste0(filter(renamekey, `ACS variable` %in% filtered_df3()[1,6]) %>% select(goodname)))+
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
# mod_combo_ui("combo_ui_1")
    
## To be copied in the server
# callModule(mod_combo_server, "combo_ui_1")
 
