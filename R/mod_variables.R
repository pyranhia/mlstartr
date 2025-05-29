#' variables UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_variables_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Sélection des variables"),
    uiOutput(ns("target_ui")),
    uiOutput(ns("predictors_ui")),
    textOutput(ns("task_type"))
  )
}

#' variables Server Functions
#'
#' @noRd
mod_variables_server <- function(id, dataset_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Générer dynamiquement les noms de variables une fois le dataset chargé
    observeEvent(dataset_r(), {
      vars <- names(dataset_r())

      output$target_ui <- renderUI({
        selectInput(ns("target"),
                    "Variable réponse :",
                    choices = vars)
      })

      output$predictors_ui <- renderUI({
        selectInput(ns("predictors"),
                    "Variables prédictives :",
                    choices = vars,
                    multiple = TRUE)
      })
    })

    # Déduire le type d'apprentissage
    task_type <- reactive({
      req(input$target)
      target_col <- dataset_r()[[input$target]]
      if (is.numeric(target_col)) {
        "régression"
      } else {
        "classification"
      }
    })

    output$task_type <- renderText({
      req(task_type())
      paste("Type de tâche détecté :", task_type())
    })

    return(
      list(
        target = reactive(input$target),
        predictors = reactive(input$predictors),
        task_type = task_type
      )
    )
  })
}

## To be copied in the UI
# mod_variables_ui("variables_1")

## To be copied in the server
# mod_variables_server("variables_1")
