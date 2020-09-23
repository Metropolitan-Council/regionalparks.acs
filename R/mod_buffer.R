#' notes UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_buffer_ui <- function(id) {
  ns <- NS(id)
    tabPanel(
      id = "buffer",
      "ACS characteristics within park/trail buffer zones", fluid = TRUE,
             titlePanel("ACS characteristics within park/trail buffer zones"),
      sidebarLayout(
        
        sidebarPanel(
          # sliderInput("distance", "Buffer width (miles):",  
          #             min = 1, max = 5, value = 1, step = 2),
          
          radioButtons("distance", "Chose buffer distance (miles):",
                       choices = list("1 mile", "3 miles", "5 miles")),
          
          selectInput("acs", "Choose ACS variable:",
                      list(`Age` = list("Age, under 18",
                                        "Age, 10-19",
                                        "Age, 18-39",
                                        "Age, 40-64",
                                        "Age, 65 and over"),
                           `Disability` = list("Disability, any disability"),
                           `Ethnicity` = list("Ethnicity, Hispanic",
                                              "Ethnicity, Not Hispanic"),
                           `Income` = list("Income, Median Household Income"),
                           `National Origin` = list("Origin, foreign-born",
                                                    "Origin, US-born"),
                           `Race` = list("Race, American Indian",
                                         "Race, Asian",
                                         "Race, Black",
                                         "Race, Pacific Islander",
                                         "Race, White",
                                         "Race, Multiracial",
                                         "Race, Other"))),
          
          selectInput("agency", "Choose agency:",
                               list("Anoka County Parks and Recreation",
                                       "Bloomington Parks and Recreation",
                                       "Carver County Parks and Recreation" , 
                                       "Dakota County Parks",
                                       "Minneapolis Park and Recreation Board",
                                       "Ramsey County Parks and Recreation" ,
                                       "Scott County Parks",
                                       "St. Paul Parks and Recreation",
                                       "Three Rivers Park District",
                                       "Washington County Parks"))
        ),
        mainPanel(
          plotOutput("distPlot")
        )
      )

    )

}


