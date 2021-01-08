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
    HTML("<form class='well'>
                  <div id='input_demos_ui_1-input_acsmap' class='form-group shiny-input-radiogroup shiny-input-container shiny-bound-input'>
                    <label class='control-label' id='input_demos_ui_1-input_acsmap-label' for='input_demos_ui_1-input_acsmap'>
                      <h4>ACS characteristics</h4>
                    </label>
                    <div class='shiny-options-group'>
                    
                    <h5>Ability</h5>

                      <div class='radio'>
                        <label>
                          <input type='radio' name='input_demos_ui_1-input_acsmap' value='adj_anydis_per'>
                          <span>% any disability</span>
                        </label>
                      </div>
                    
                    
                    <h5>Age</h5>

                      <div class='radio'>
                        <label>
                          <input type='radio' name='input_demos_ui_1-input_acsmap' value='adj_ageunder15_per' checked='checked'>
                          <span>% under 15</span>
                        </label>
                      </div>
                      <div class='radio'>
                        <label>
                          <input type='radio' name='input_demos_ui_1-input_acsmap' value='adj_age15_24_per'>
                          <span>% 15-24</span>
                        </label>
                      </div>
                                            <div class='radio'>
                        <label>
                          <input type='radio' name='input_demos_ui_1-input_acsmap' value='adj_age25_64_per'>
                          <span>% 25-64</span>
                        </label>
                      </div>
                                            <div class='radio'>
                        <label>
                          <input type='radio' name='input_demos_ui_1-input_acsmap' value='adj_age65up_per'>
                          <span>% over 64</span>
                        </label>
                      </div>
                      
                                            
                      
                                            <h5>Ethnicity & Race</h5>

                      <div class='radio'>
                        <label>
                          <input type='radio' name='input_demos_ui_1-input_acsmap' value='adj_hisppop_per'>
                          <span>% Hispanic</span>
                        </label>
                      </div>
                      <div class='radio'>
                        <label>
                          <input type='radio' name='input_demos_ui_1-input_acsmap' value='adj_nothisppop_per'>
                          <span>% not Hispanic</span>
                        </label>
                      </div>
                                            <div class='radio'>
                        <label>
                          <input type='radio' name='input_demos_ui_1-input_acsmap' value='adj_amindnh_per'>
                          <span>% American Indian</span>
                        </label>
                      </div>
                      <div class='radio'>
                        <label>
                          <input type='radio' name='input_demos_ui_1-input_acsmap' value='adj_asiannh_per'>
                          <span>% Asian</span>
                        </label>
                      </div>
                                            <div class='radio'>
                        <label>
                          <input type='radio' name='input_demos_ui_1-input_acsmap' value='adj_blacknh_per'>
                          <span>% Black</span>
                        </label>
                      </div>                      <div class='radio'>
                        <label>
                          <input type='radio' name='input_demos_ui_1-input_acsmap' value='adj_othermultinh_per'>
                          <span>% Other + Multi</span>
                        </label>
                      </div>                      <div class='radio'>
                        <label>
                          <input type='radio' name='input_demos_ui_1-input_acsmap' value='adj_whitenh_per'>
                          <span>% White</span>
                        </label>
                      </div>
                      
                                                 <h5>Income</h5>

                      <div class='radio'>
                        <label>
                          <input type='radio' name='input_demos_ui_1-input_acsmap' value='adj_meanhhi'>
                          <span>Mean household income $</span>
                        </label>
                      </div>
                      
                                                                   <h5>Language</h5>

                      <div class='radio'>
                        <label>
                          <input type='radio' name='input_demos_ui_1-input_acsmap' value='adj_lep_per'>
                          <span>% limited English proficiency</span>
                        </label>
                      </div>
                                       <div class='radio'>
                        <label>
                          <input type='radio' name='input_demos_ui_1-input_acsmap' value='adj_span_per'>
                          <span>% Spanish-speaking</span>
                        </label>
                      </div>
                      
                                                                  <h5>National Origin</h5>

                      <div class='radio'>
                        <label>
                          <input type='radio' name='input_demos_ui_1-input_acsmap' value='adj_forborn_per'>
                          <span>% forgein-born</span>
                        </label>
                      </div>
                      <div class='radio'>
                        <label>
                          <input type='radio' name='input_demos_ui_1-input_acsmap' value='adj_usborn_per'>
                          <span>% US-born</span>
                        </label>
                      </div>
                    

                      
                      
                                             <h5>Transportation</h5>

                      <div class='radio'>
                        <label>
                          <input type='radio' name='input_demos_ui_1-input_acsmap' value='adj_novehicle_per'>
                          <span>% households without a  vehicle</span>
                        </label>
                      </div>
                      
                      
                    </div>
                  </div>
                </form>")
  )
}



#' input_demos Server Function
#'
#' @noRd
mod_input_demos_server <- function(input, output, session) {
  ns <- session$ns

  input_values <- reactiveValues() # start with an empty reactiveValues object.

  observeEvent(input$input_acsmap, { # only update when the user changes the ACS input
    input_values$input_acsmap <- input$input_acsmap # create/update the ACS input value in our reactiveValues object
  })

  return(input_values)
}



## To be copied in the UI
# mod_input_demos_ui("input_demos_ui_1")

## To be copied in the server
# callModule(mod_input_demos_server, "input_demos_ui_1")
