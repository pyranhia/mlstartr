#' dataset UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @importFrom shiny NS tagList
mod_dataset_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(
      ns("dataset"),
      "Jeu de donn\u00e9es :",
      choices = c(
        "California Housing (r\u00e9gression)"   = "housing",
        "Titanic (classification binaire)"      = "titanic",
        "Penguins (classification multiclasse)" = "penguins"
      )
    ),
    DT::DTOutput(ns("dataset_table"))
  )
}

#' dataset Server Functions
#'
#' @noRd
#' @importFrom dplyr mutate
#' @importFrom utils data
utils::globalVariables("survived")
mod_dataset_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Variables exposees par dataset
    housing_vars  <- c("median_house_value", "median_income", "housing_median_age",
                       "total_rooms", "total_bedrooms", "population",
                       "households", "ocean_proximity")
    titanic_vars  <- c("survived", "pclass", "sex", "age", "fare", "sibsp", "parch")
    penguins_vars <- c("species", "bill_length_mm", "bill_depth_mm",
                       "flipper_length_mm", "body_mass_g", "sex")

    dataset_r <- reactive({
      switch(input$dataset,
             "housing" = {
               e <- new.env()
               data("housing", package = "datapyranhia", envir = e)
               e$housing[, housing_vars]
             },
             "titanic" = {
               e <- new.env()
               data("titanic", package = "datapyranhia", envir = e)
               e$titanic[, titanic_vars] |>
                 dplyr::mutate(survived = factor(survived, levels = c(0, 1),
                                                 labels = c("non", "oui")))
             },
             "penguins" = {
               palmerpenguins::penguins[, penguins_vars]
             }
      )
    })

    output$dataset_table <- DT::renderDT({
      req(dataset_r())
      DT::datatable(
        dataset_r(),
        options = list(
          pageLength = 5,
          dom = "tip",
          pagingType = "simple"
        ),
        rownames = FALSE,
        class = "stripe hover"
      )
    })

    return(list(
      data    = dataset_r,
      dataset = reactive(input$dataset)
    ))
  })
}

## To be copied in the UI
# mod_dataset_ui("dataset_1")

## To be copied in the server
# mod_dataset_server("dataset_1")
