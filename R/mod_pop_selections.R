#' pop_selections UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_pop_selections_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    absolutePanel(
      id = ns("inputPop"),
      class = "panel panel-default", fixed = FALSE,
      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
      width = 330, height = "auto",
      ## radio button inputs -----
      HTML("<div id='pop_selections_ui_1-inputPop' class='form-group shiny-input-radiogroup shiny-input-container shiny-bound-input'>
              <label class='control-label' for='pop_selections_ui_1-inputPop'><div></div></label>
              <div class='shiny-options-group'>
                   <div>
<h5>Population</h5>
</div>
<div class='radio'>
                  <label>
                    <input type='radio' name='pop_selections_ui_1-inputPop' value='pop_est' checked='checked'>
                    <span>2019 pop. estimate</span>
                  </label>
                </div>
 <div class='radio'>
                  <label>
                    <input type='radio' name='pop_selections_ui_1-inputPop' value='hh_est'>
                    <span>2010-2040 absolute growth</span>
                  </label>
                </div>
                <div class='radio'>
                  <label>
                    <input type='radio' name='pop_selections_ui_1-inputPop' value='2010-2040 relative growth'>
                    <span>2010-2040 relative growth</span>
                  </label>
                </div>
                <div class='radio'>
                  <label>
                    <input type='radio' name='pop_selections_ui_1-inputPop' value='2040 pop. forecast'>
                    <span>2040 pop. forecast</span>
                  </label>
                </div>
           </div>
           </div>"),
      
      # tags$div(
      #   tags$a(
      #     href = "https://metrocouncil.org", target = "_blank",
      #     img(src = "www/mark.png", align = "right", style = "padding: 1%")
      #   ),
      #   "For an accessible version of this information, please contact us at",
      #   tags$a(href = "mailto:research@metc.state.mn.us", "research@metc.state.mn.us"),
      #   style = "font-size: 1.1rem;
      #        display: block;
      #        text-align: left;
      #        margin: 1%;"
      # ),
    )
  )
}

#' pop_selections Server Function
#'
#' @noRd 
mod_pop_selections_server <- function(input, output, session){
  ns <- session$ns
  
  
  vals <- reactiveValues()
  
  observeEvent(input$inputPop, {
    vals$selected_var <- input$inputPop
    vals$pop_data <- est_pop[input$inputPop]
  })
  
  return(vals)
  
}

## To be copied in the UI
# mod_pop_selections_ui("pop_selections_ui_1")

## To be copied in the server
# callModule(mod_pop_selections_server, "pop_selections_ui_1")
