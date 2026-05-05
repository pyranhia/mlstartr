#' variables UI Function
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @importFrom stats setNames
#' @importFrom shiny NS tagList
mod_variables_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Sélectionner des variables"),
    uiOutput(ns("target_ui")),
    uiOutput(ns("predictors_ui")),
    textOutput(ns("task_type")),
    hr(),
    actionButton(ns("validate"), "Valider la configuration",
                 class = "btn-primary")
  )
}

#' variables Server Functions
#'
#' @noRd
mod_variables_server <- function(id, dataset_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Classify variables by type (numeric vs categorical)
    classify_variables <- function(data, exclude = NULL) {
      vars <- setdiff(names(data), exclude)
      continuous_vars <- vars[sapply(data[vars], is.numeric)]
      discrete_vars   <- vars[!sapply(data[vars], is.numeric)]

      choices_list <- list()
      if (length(continuous_vars) > 0) {
        choices_list[["Variables continues"]] <- setNames(continuous_vars, continuous_vars)
      }
      if (length(discrete_vars) > 0) {
        choices_list[["Variables discretes"]] <- setNames(discrete_vars, discrete_vars)
      }
      choices_list
    }

    # When dataset changes, reset target selector
    observeEvent(dataset_r(), {
      choices_grouped <- classify_variables(dataset_r())

      output$target_ui <- renderUI({
        selectInput(ns("target"), "Variable reponse :",
                    choices = choices_grouped)
      })
    })

    # When target changes, update predictors (exclude target)
    observeEvent(input$target, {
      req(input$target)
      choices_grouped <- classify_variables(dataset_r(), exclude = input$target)

      output$predictors_ui <- renderUI({
        selectInput(ns("predictors"), "Variables predictives :",
                    choices = choices_grouped,
                    multiple = TRUE,
                    selected = names(unlist(choices_grouped)))
      })
    })

    # Detect task type
    task_type <- reactive({
      req(input$target)
      target_col <- dataset_r()[[input$target]]
      if (is.numeric(target_col) && !is.factor(target_col)) "regression" else "classification"
    })

    output$task_type <- renderText({
      req(task_type())
      paste("Type de tache detecte :", task_type())
    })

    # Validation flag
    validated <- reactiveVal(FALSE)

    observeEvent(input$validate, {
      req(input$target, input$predictors)
      validated(TRUE)
    })

    # Reset validation if dataset or target changes
    observeEvent(list(dataset_r(), input$target), {
      validated(FALSE)
    })

    return(list(
      target     = reactive(input$target),
      predictors = reactive(input$predictors),
      task_type  = task_type,
      validated  = validated
    ))
  })
}
