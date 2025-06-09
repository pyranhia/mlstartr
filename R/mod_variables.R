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
    h3("Sélectionner des variables"),
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

    # Function to classify variables by type
    classify_variables <- function(data) {
      continuous_vars <- c()
      discrete_vars <- c()

      for (var_name in names(data)) {
        if (is.numeric(data[[var_name]])) {
          continuous_vars <- c(continuous_vars, var_name)
        } else {
          discrete_vars <- c(discrete_vars, var_name)
        }
      }

      # Generate choice structure for selectInput
      choices_list <- list()

      if (length(continuous_vars) > 0) {
        # Generate named vector for continuous variables
        cont_choices <- continuous_vars
        names(cont_choices) <- continuous_vars
        choices_list[["Variables continues"]] <- cont_choices
      }

      if (length(discrete_vars) > 0) {
        # Generate named vector for discrete variables
        disc_choices <- discrete_vars
        names(disc_choices) <- discrete_vars
        choices_list[["Variables discrètes"]] <- disc_choices
      }

      return(choices_list)
    }

    # Dynamically generate variable names once the dataset has been loaded
    observeEvent(dataset_r(), {
      choices_grouped <- classify_variables(dataset_r())

      output$target_ui <- renderUI({
        selectInput(ns("target"),
                    "Variable réponse :",
                    choices = choices_grouped)
      })

      output$predictors_ui <- renderUI({
        selectInput(ns("predictors"),
                    "Variables prédictives :",
                    choices = choices_grouped,
                    multiple = TRUE)
      })
    })

    # Deduce the type of learning (regression vs classification)
    task_type <- reactive({
      req(input$target)
      target_col <- dataset_r()[[input$target]]
      if (is.numeric(target_col) && !is.factor(target_col)) {
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
