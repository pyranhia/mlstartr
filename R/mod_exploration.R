#' exploration UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_exploration_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' exploration Server Functions
#'
#' @noRd 
mod_exploration_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_exploration_ui("exploration_1")
    
## To be copied in the server
# mod_exploration_server("exploration_1")
