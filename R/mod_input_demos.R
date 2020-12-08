#' input_demos UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_input_demos_ui <- function(id) {
  ns <- NS(id)
  tagList(
    absolutePanel(
      id = ns("controls_ct"),
      class = "panel panel-default", fixed = FALSE,
      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
      width = 330, height = "auto",
      ## radio button inputs -----
      HTML("<div id='input_demos_ui_1-inputCensusTracts' class='form-group shiny-input-radiogroup shiny-input-container shiny-bound-input'>
              <label class='control-label' for='input_demos_ui_1-inputCensusTracts'><div></div></label>
              <div class='shiny-options-group'>
                   <div>
<h5>Age</h5>
</div>
<div class='radio'>
                  <label>
                    <input type='radio' name='input_demos_ui_1-inputCensusTracts' value='Age, under 18'>
                    <span>Age, under 18</span>
                  </label>
                </div>
 <div class='radio'>
                  <label>
                    <input type='radio' name='input_demos_ui_1-inputCensusTracts' value='Age, 10-19' checked='checked'>
                    <span>Age, 10-19</span>
                  </label>
                </div>
                <div class='radio'>
                  <label>
                    <input type='radio' name='input_demos_ui_1-inputCensusTracts' value='Age, 18-39' checked='checked'>
                    <span>Age, 18-39</span>
                  </label>
                </div>
                <div class='radio'>
                  <label>
                    <input type='radio' name='input_demos_ui_1-inputCensusTracts' value='Age, 40-64'>
                    <span>Age, 40-64</span>
                  </label>
                </div>
                <div class='radio'>
                  <label>
                    <input type='radio' name='input_demos_ui_1-inputCensusTracts' value='Age, 65 and over'>
                    <span>Age, 65 and over</span>
                  </label>
                </div>

                <div>
  <h5>Disability</h5>
</div><div class='radio'>
                  <label>
                    <input type='radio' name='input_demos_ui_1-inputCensusTracts' value='Disability, any disability'>
                    <span>Disability, any disability</span>
                  </label>
                </div>
                <div>
  <h5>Ethnicity</h5>

</div><div class='radio'>
                  <label>
                    <input type='radio' name='input_demos_ui_1-inputCensusTracts' value='Ethnicity, Hispanic'>
                    <span>Ethnicity, Hispanic</span>
                  </label>
                </div>
                <div class='radio'>
                  <label>
                    <input type='radio' name='input_demos_ui_1-inputCensusTracts' value='Ethnicity, Not Hispanic'>
                    <span>Ethnicity, Not Hispanic</span>
                  </label>
                </div>
                <div><h5>Income</h5></div><div class='radio'>
                  <label>
                    <input type='radio' name='input_demos_ui_1-inputCensusTracts' value='Income, Median Household Income'>
                    <span>Income, Median Household Income</span>
                  </label>
                </div>
                <div><h5>National Origin</h5></div><div class='radio'>
                  <label>
                    <input type='radio' name='input_demos_ui_1-inputCensusTracts' value='Origin, foreign-born'>
                    <span>Origin, foreign-born</span>
                  </label>
                </div>
                <div class='radio'>
                  <label>
                    <input type='radio' name='input_demos_ui_1-inputCensusTracts' value='Origin, US-born'>
                    <span>Origin, US-born</span>
                  </label>
                </div>
                <h5>Race</h5><div class='radio'>
                  <label>
                    <input type='radio' name='input_demos_ui_1-inputCensusTracts' value='Race, American Indian'>
                    <span>Race, American Indian</span>
                  </label>
                </div>
                <div class='radio'>
                  <label>
                    <input type='radio' name='input_demos_ui_1-inputCensusTracts' value='Race, Asian'>
                    <span>Race, Asian</span>
                  </label>
                </div>
                <div class='radio'>
                  <label>
                    <input type='radio' name='input_demos_ui_1-inputCensusTracts' value='Race, Black'>
                    <span>Race, Black</span>
                  </label>
                </div>
                <div class='radio'>
                  <label>
                    <input type='radio' name='input_demos_ui_1-inputCensusTracts' value='Race, Pacific Islander'>
                    <span>Race, Pacific Islander</span>
                  </label>
                </div>
                <div class='radio'>
                  <label>
                    <input type='radio' name='input_demos_ui_1-inputCensusTracts' value='Race, White'>
                    <span>Race, White</span>
                  </label>
                </div>
                <div class='radio'>
                  <label>
                    <input type='radio' name='input_demos_ui_1-inputCensusTracts' value='Race, Multiracial'>
                    <span>Race, Multiracial</span>
                  </label>
                </div>
                 <div class='radio'>
                  <label>
                    <input type='radio' name='input_demos_ui_1-inputCensusTracts' value='Race, Other'>
                    <span>Race, Other</span>
                  </label>
                </div>
              </div>
            </div>"),

      tags$div(
        tags$a(
          href = "https://metrocouncil.org", target = "_blank",
          img(src = "www/mark.png", align = "right", style = "padding: 1%")
        ),
        "For an accessible version of this information, please contact us at",
        tags$a(href = "mailto:research@metc.state.mn.us", "research@metc.state.mn.us"),
        style = "font-size: 1.1rem;
             display: block;
             text-align: left;
             margin: 1%;"
      ),



# radioButtons(inputId = ns("inputCensusTracts"), label = "Census Tracts",
# choices = sort(table_ct$category))
    )
  )
}

#' input_demos Server Function
#'
#' @noRd
mod_input_demos_server <- function(input, output, session) {
  ns <- session$ns


  vals <- reactiveValues()

  observeEvent(input$inputCensusTracts, {
    vals$selected_var <- input$inputCensusTracts
    vals$color_pal <- dplyr::filter(table_ct, category == input$inputCensusTracts)[[3]]
    vals$tract_data <- census_tract[input$inputCensusTracts]
  })


  return(vals)
}

## To be copied in the UI
# mod_input_demos_ui("input_demos_ui_1")

## To be copied in the server
# callModule(mod_input_demos_server, "input_demos_ui_1")
