#' evaluation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_evaluation_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' evaluation Server Functions
#'
#' @noRd 
mod_evaluation_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_evaluation_ui("evaluation_1")
    
## To be copied in the server
# mod_evaluation_server("evaluation_1")
