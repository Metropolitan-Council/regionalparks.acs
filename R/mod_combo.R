#' combo UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_combo_ui <- function(id){
  ns <- NS(id)
  tagList(
    id = ns("sum_CHOICE"),
    HTML('<p>Select 1 ACS variable from the drop-down menu and 1 or more Agency of interest. The buffer distances roughly map onto walking distance (0.5 mi), biking distance (1.5 mi), or some other distance (3 mi). The reactive points show the values from the American Community Survey (either weighted averages or raw data, depending on tab selection) within a given buffer zone around regional parks and trails.</p>'), hr(),
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
    plotlyOutput(ns("comboplot"), height = 700)
  )
}
    
#' combo Server Function
#'
#' @noRd 
mod_combo_server <- function(input, output, session){
  ns <- session$ns
 
  
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
 
