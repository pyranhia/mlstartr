#' intro UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_intro_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Bienvenue dans l'application d'initiation au Machine Learning"),
    p("Cette application vous guide pas à pas..."),
    #img(src = "www/illu_intro.png", alt = "Illustration ML", width = "100%")
  )
}

#' intro Server Functions
#'
#' @noRd
mod_intro_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # Nada
  })
}

## To be copied in the UI
# mod_intro_ui("intro_1")

## To be copied in the server
# mod_intro_server("intro_1")
