#' pretraitement UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_pretraitement_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' pretraitement Server Functions
#'
#' @noRd 
mod_pretraitement_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_pretraitement_ui("pretraitement_1")
    
## To be copied in the server
# mod_pretraitement_server("pretraitement_1")
