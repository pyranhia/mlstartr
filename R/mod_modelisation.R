#' modelisation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_modelisation_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' modelisation Server Functions
#'
#' @noRd 
mod_modelisation_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_modelisation_ui("modelisation_1")
    
## To be copied in the server
# mod_modelisation_server("modelisation_1")
