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
#' @import leaflet
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

recodeadjtable <- tribble(
  ~ ACS,
  ~ nicename,
  "adj_poptotal",
  "Population",
  "adj_ageunder15_per",
  "% under age 15",
  "adj_age15_24_per",
  "% age 15-24",
  "adj_age25_64_per",
  "% age 25-64",
  "adj_age65up_per",
  "% age 65+",
  "adj_whitenh_per",
  "% White",
  "adj_blacknh_per",
  "% Black",
  "adj_asiannh_per",
  "% Asian",
  "adj_amindnh_per",
  "% Am. Indian",
  "adj_othermultinh_per",
  "% Other + Multi",
  "adj_hisppop_per",
  "% Hispanic",
  "adj_nothisppop_per",
  "% not-Hispanic",
  "adj_meanhhi",
  "Mean household income",
  "adj_novehicle_per",
  "% Housholds without a vehicle",
  "adj_lep_per",
  "% speaking English less than very well",
  "adj_span_per",
  "% Spanish speakers"
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
          "Existing" = "#2ec799",
          "Planned" = "#f77614",
          "Search" = "#9591c9"
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

######### ui --------
mod_combo_ui <- function(id){
  ns <- NS(id)
  tagList(
    # id = ns("sum_CHOICE"),
    h3("Select inputs: "),
    fluidPage(
      fluidRow(
        column(
          width = 3,
          selectInput(
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
            selected = "adj_ageunder15_per", selectize = F
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
            multiple = TRUE, selectize = T
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
h3("View data: "),
    tabsetPanel( selected = "Weighted averages",
                 tabPanel("Weighted averages",
                          HTML('<p>This plot is indeted to provide summarized demographic values for all the regional parks and trails. Point location along the x-axis indicates the demographic value which can be compared across and within park/trail status (existing, planned, search) or agencies. Color indicates park/trail status (green = existing, yellow = planned, red = search). Shape indicates park/trail type (circle = park, square = trail). The solid black, vertical line indicates the average demographic value within agency boundaries.</p>'),
                           plotOutput(ns("leg"), height = 100),
                           plotlyOutput(ns("comboplot"), height = 700)
                           ),
                 tabPanel("Buffer map",
                          HTML('<p>This map visualizes the geospatial location of the buffers around the user-selected parks and trails. Demographic data can also be shown here.  THIS MAP IS NOT REACTIVE AT THE MOMENT (but will be updated)'),
                          # dataTableOutput(ns("testbufmap")),
                          leafletOutput(ns("buffermap"), height = 700)),
                 tabPanel("Download tabular data",
                          HTML('<p>Cells show the average weighted value, with the range of all intersecting block groups in parentheses. These data are available for download.</p>'),
                          downloadButton(ns("downloadData"), "Download tabular data"),
                 hr(),
                 dataTableOutput(ns("datatable"))),
                 tabPanel("Methods & raw data"))

  )
}

###### server -----    
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
  
  data_noacs = reactiveVal(long_buffer_data)
  filt_data_noacs <- reactive({
    res <- data_noacs() %>% filter(#ACS == input$ACS,
                                    agency %in% input$agency,
                                    distance == input$distance,
                                    type %in% input$type,
                                    status %in% input$status)
    res
  })
  
  # data_parktrailgeo = reactiveVal(park_trail_geog_LONG)
  # filt_parktrailgeo <- reactive({
  #   res <- data_parktrailgeo() %>%
  #     filter(agency %in% input$agency,
  #            Type %in% input$type,
  #            status2 %in% input$status)
  #   res
  # })
  
  selected_pt = reactive({
    gooddata <- park_trail_geog_LONG %>%
      filter(agency %in% input$agency,
             Type %in% input$type,
             status2 %in% input$status)
    gooddata
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
            "Existing" = "#2ec799",
            "Planned" = "#f77614",
            "Search" = "#9591c9"
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
  
  output$datatable <- renderDataTable({
    filt_data_noacs() %>%
      left_join(recodeadjtable) %>%
      select(-ACS) %>%
      rename(ACS = nicename) %>%
      filter(!is.na(ACS)) %>%
      mutate(value = round(value, 1)) %>%
      select(agency, name, type, status, distance, ACS, value) %>%
      pivot_wider(names_from = ACS, values_from = value) %>%
      separate(name,
               into = c("name", 'delete2'),
               sep = c("_")) %>%
      select(-delete2) %>% #, -Population) %>%
      mutate(name = str_replace_all(
        name,
        c(
          "Regional Park" = "RP",
          "Regional Trail" = "RT",
          "Park Reserve" = "PR"
        )
      )) %>%
      rename(
        Agency = agency,
        Name = name,
        Type = type,
        Status = status,
        Dist. = distance
      ) 
  })
  
  output$downloadData <- downloadHandler(
    filename = "ACS.csv",
    content = function(file) {
      write.csv((filt_data_noacs() %>%
                   left_join(recodeadjtable) %>%
                   select(-ACS) %>%
                   rename(ACS = nicename) %>%
                   filter(!is.na(ACS)) %>%
                   mutate(value = round(value, 1)) %>%
                   select(agency, name, type, status, distance, ACS, value) %>%
                   pivot_wider(names_from = ACS, values_from = value) %>%
                   separate(name,
                            into = c("name", 'delete2'),
                            sep = c("_")) %>%
                   select(-delete2, -Population)), 
                file, row.names = FALSE)
    }
  )
  
  output$buffermap <- renderLeaflet({
    leaflet() %>%
      setView(lat = 44.963,
              lng = -93.22,
              zoom = 9) %>%
      addProviderTiles("CartoDB.Positron",
                       group = "Carto Positron")  %>%
      addPolygons(
        data = agency_boundary,
        group = "Agency boundaries",
        stroke = T,
        color = "black",
        fill = F,
        weight = 3
      )  %>%
      addLayersControl(
        position = "bottomright",
        baseGroups = c(
          "Carto Positron"),
        options = layersControlOptions(collapsed = F)
      ) %>%
    #   htmlwidgets::onRender(
    #     "
    #     function() {
    #         $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:center\">Map layers</label>');
    #     }
    # "
    #   ) %>%
      leaflet::addScaleBar(position = c("bottomleft"))
  })
  
  
  observe({
    if(nrow(selected_pt()) > 0)
    {
      leafletProxy("buffermap", data = selected_pt()) %>%
        addTiles() %>% 
        clearShapes() %>%
        #---- existing
        addPolylines(data = selected_pt() %>% filter(Type == "Trail",
                                                  status2 == "Existing"),
                     color = "#2ec799",
                     weight = 3,
                     stroke = T,
                     opacity = 1,
                     popup = ~paste0(
                       "<b>", "Trail - existing", "</b>", "<br>",
                       selected_pt() %>%
                         filter(Type == "Trail",
                                status2 == "Existing") %>%
                         .$name, "<br>",
                       "<em>",
                       selected_pt() %>%
                         filter(Type == "Trail",
                                status2 == "Existing") %>%
                         .$agency, "</em>"
                     ),
                     highlightOptions = highlightOptions(
                       stroke = TRUE,
                       color = "black",
                       weight = 6,
                       bringToFront = TRUE
                     )) %>%
        addPolygons(data = selected_pt() %>% filter(Type == "Park",
                                                 status2 == "Existing"),
                    color = "#2ec799",
                    fillColor = "#2ec799",
                    fillOpacity = 1,
                    weight = 3,
                    stroke = T,
                    opacity = 1,
                    popup = ~paste0(
                      "<b>", "Park - existing", "</b>", "<br>",
                      selected_pt() %>%
                        filter(Type == "Park",
                               status2 == "Existing") %>%
                        .$name, "<br>",
                      "<em>",
                      selected_pt() %>%
                        filter(Type == "Park",
                               status2 == "Existing") %>%
                        .$agency, "</em>"
                    ),
                    highlightOptions = highlightOptions(
                      stroke = TRUE,
                      color = "black",
                      weight = 6,
                      bringToFront = TRUE)) %>%
        #----- planned
        addPolylines(data = selected_pt() %>% filter(Type == "Trail",
                                                  status2 == "Planned"),
                     color = "#f77614",
                     weight = 3,
                     stroke = T,
                     opacity = 1,
                     popup = ~paste0(
                       "<b>", "Trail - planned", "</b>", "<br>",
                       selected_pt() %>%
                         filter(Type == "Trail",
                                status2 == "Planned") %>%
                         .$name, "<br>",
                       "<em>",
                       selected_pt() %>%
                         filter(Type == "Trail",
                                status2 == "Planned") %>%
                         .$agency, "</em>"
                     ),
                     highlightOptions = highlightOptions(
                       stroke = TRUE,
                       color = "black",
                       weight = 6,
                       bringToFront = TRUE)) %>%
        addPolygons(data = selected_pt() %>% filter(Type == "Park",
                                                 status2 == "Planned"),
                    color = "#f77614",
                    fillColor = "#f77614",
                    fillOpacity = 1,
                    weight = 3,
                    stroke = T,
                    opacity = 1,
                    popup = ~paste0(
                      "<b>", "Park - planned", "</b>", "<br>",
                      selected_pt() %>%
                        filter(Type == "Park",
                               status2 == "Planned") %>%
                        .$name, "<br>",
                      "<em>",
                      selected_pt() %>%
                        filter(Type == "Park",
                               status2 == "Planned") %>%
                        .$agency, "</em>"
                    ),
                    highlightOptions = highlightOptions(
                      stroke = TRUE,
                      color = "black",
                      weight = 6,
                      bringToFront = TRUE)) %>%
        
        #----- search
        addPolylines(data = selected_pt() %>% filter(Type == "Trail",
                                                  status2 == "Search"),
                     color = "#9591c9",
                     weight = 3,
                     stroke = T,
                     opacity = 1,
                     popup = ~paste0(
                       "<b>", "Trail - search", "</b>", "<br>",
                       selected_pt() %>%
                         filter(Type == "Trail",
                                status2 == "Search") %>%
                         .$name, "<br>",
                       "<em>",
                       selected_pt() %>%
                         filter(Type == "Trail",
                                status2 == "Search") %>%
                         .$agency, "</em>"
                     ),
                     highlightOptions = highlightOptions(
                       stroke = TRUE,
                       color = "black",
                       weight = 6,
                       bringToFront = TRUE)) %>%
        addPolygons(data = selected_pt() %>% filter(Type == "Park",
                                                 status2 == "Search"),
                    color = "#9591c9",
                    fillColor = "#9591c9",
                    fillOpacity = 1,
                    weight = 3,
                    stroke = T,
                    opacity = 1,
                    popup = ~paste0(
                      "<b>", "Park - search", "</b>", "<br>",
                      selected_pt() %>%
                        filter(Type == "Park",
                               status2 == "Search") %>%
                        .$name, "<br>",
                      "<em>",
                      selected_pt() %>%
                        filter(Type == "Park",
                               status2 == "Search") %>%
                        .$agency, "</em>"
                    ),
                    highlightOptions = highlightOptions(
                      stroke = TRUE,
                      color = "black",
                      weight = 6,
                      bringToFront = TRUE)) 
    }
  })
  

}
    
## To be copied in the UI
# mod_combo_ui("combo_ui_1")
    
## To be copied in the server
# callModule(mod_combo_server, "combo_ui_1")
 
