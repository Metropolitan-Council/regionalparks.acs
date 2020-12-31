#' pop_selections UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pop_selections_ui <- function(id) {
  ns <- NS(id)
  tagList(
        # radioButtons(
          ns("input_pop"),
        #   label = h5("Observed population and forecasts"),
        #   
        #   choices = c("2019 population" = "PopEst_2019",
        #               "2019 pop. density" = "PopDens_2019",
        #               "2040 forecast pop." = "POP2040",
        #               "2040 forecast pop. dens." = "popdens_2040_mi",
        #               "2010-2040, absolute growth" = "growth_abs_10_40",
        #               "2010-2040, relative growth" = "growth_rel_10_40"),
        #   selected = "PopEst_2019"
        
    HTML(
      "<div id='pop_selections_ui_1-input_pop' class='form-group shiny-input-radiogroup shiny-input-container shiny-bound-input'>
                <label class='control-label' for='pop_selections_ui_1-input_pop'><div></div></label>
                <div class='shiny-options-group'>
                     
  <div><h5>Observed</h5></div>
  <div class='radio'> <label> <input type='radio' name='pop_selections_ui_1-input_pop' value='PopEst_2019' checked='checked'> <span>2019 population</span></label></div>
  <div class='radio'> <label> <input type='radio' name='pop_selections_ui_1-input_pop' value='PopDens_2019'> <span>2019 pop. density</span> </label> </div>
  
    <div><h5>Forecast</h5></div>
  <div class='radio'> <label> <input type='radio' name='pop_selections_ui_1-input_pop' value='POP2040'> <span>2040 population</span></label></div>
  <div class='radio'> <label> <input type='radio' name='pop_selections_ui_1-input_pop' value='popdens_2040_mi'> <span>2040 pop. density</span> </label> </div>
  
      <div><h5>Growth</h5></div>
  <div class='radio'> <label> <input type='radio' name='pop_selections_ui_1-input_pop' value='growth_abs_10_40'> <span>2010-2040, absolute growth</span></label></div>
  <div class='radio'> <label> <input type='radio' name='pop_selections_ui_1-input_pop' value='growth_rel_10_40'> <span>2010-2040, relative growth</span> </label> </div>
  
            </div>
             </div>"
    )
      ) 
  
  
  
  # radio button inputs -----
 

}



#' pop_selections Server Function
#'
#' @noRd
mod_pop_selections_server <- function(input, output, session) {
  ns <- session$ns

  input_values <- reactiveValues() # start with an empty reactiveValues object.

  observeEvent(input$input_pop, { # only update when the user changes the ACS input
    input_values$input_pop <- input$input_pop # create/update the ACS input value in our reactiveValues object
  })

  return(input_values)
}

## To be copied in the UI
# mod_pop_selections_ui("pop_selections_ui_1")

## To be copied in the server
# callModule(mod_pop_selections_server, "pop_selections_ui_1")
