#' dataset UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_dataset_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Choisir un jeu de données"),
    selectInput(
      ns("dataset"),
      "Jeu de données disponible :",
      choices = c("iris", "mtcars", "penguins")
    )
  )
}

#' dataset Server Functions
#'
#' @noRd
mod_dataset_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    dataset_r <- reactive({
      switch(input$dataset,
             "iris" = iris,
             "mtcars" = mtcars,
             "penguins" = {
               if (!requireNamespace("palmerpenguins", quietly = TRUE)) {
                 stop("Le package {palmerpenguins} n'est pas installé.")
               }
               palmerpenguins::penguins
             }
      )
    })

    return(dataset_r)  # utile pour les modules suivants
  })
}
## To be copied in the UI
# mod_dataset_ui("dataset_1")

## To be copied in the server
# mod_dataset_server("dataset_1")
