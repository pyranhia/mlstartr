#' variables UI Function
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @importFrom shiny NS tagList
mod_variables_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("target_ui")),
    uiOutput(ns("predictors_ui")),
    uiOutput(ns("task_type_ui")),
    hr(),
    actionButton(ns("validate"), "Valider la configuration",
                 class = "btn-primary")
  )
}

#' variables Server Functions
#'
#' @noRd
#' @importFrom stats setNames
mod_variables_server <- function(id, dataset_r, code_log, dataset_name) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Cible par defaut selon le dataset
    default_target <- function(data) {
      nms <- names(data)
      if ("median_house_value" %in% nms) return("median_house_value")
      if ("survived"           %in% nms) return("survived")
      if ("species"            %in% nms) return("species")
      nms[1]
    }

    # Classifier les variables par type
    classify_variables <- function(data, exclude = NULL) {
      vars         <- setdiff(names(data), exclude)
      numeric_vars <- vars[sapply(data[vars], is.numeric)]
      other_vars   <- vars[!sapply(data[vars], is.numeric)]

      choices <- list()
      if (length(numeric_vars) > 0)
        choices[["Variables continues"]]  <- setNames(numeric_vars, numeric_vars)
      if (length(other_vars) > 0)
        choices[["Variables discr\u00e8tes"]] <- setNames(other_vars, other_vars)
      choices
    }

    # Quand le dataset change, reinitialiser la cible avec la valeur par defaut
    observeEvent(dataset_r(), {
      target_default <- default_target(dataset_r())
      choices_all    <- classify_variables(dataset_r())

      output$target_ui <- renderUI({
        selectInput(ns("target"), "Variable r\u00e9ponse :",
                    choices  = choices_all,
                    selected = target_default)
      })
    })

    # Quand la cible change, mettre a jour les predicteurs (exclure la cible)
    observeEvent(input$target, {
      req(input$target)
      choices_pred <- classify_variables(dataset_r(), exclude = input$target)

      output$predictors_ui <- renderUI({
        selectInput(ns("predictors"), "Variables pr\u00e9dictives :",
                    choices  = choices_pred,
                    multiple = TRUE,
                    selected = names(unlist(choices_pred)))
      })
    })

    # Type de tache
    task_type <- reactive({
      req(input$target)
      target_col <- dataset_r()[[input$target]]
      if (is.numeric(target_col) && !is.factor(target_col)) "regression" else "classification"
    })

    output$task_type_ui <- renderUI({
      req(task_type())
      label <- if (task_type() == "regression") "R\u00e9gression" else "Classification"
      div(
        class = "text-muted mt-2",
        style = "font-size: 0.9rem;",
        paste("T\u00e2che d\u00e9tect\u00e9e :", label)
      )
    })

    # Validation
    validated <- reactiveVal(FALSE)

    observeEvent(input$validate, {
      req(input$target, input$predictors)
      validated(TRUE)

      bloc <- paste0(
        "# Chargement des donnees\n",
        "library(datapyranhia)\n",
        "library(tidymodels)\n\n",
        'data("', dataset_name(), '", package = "datapyranhia")\n\n',
        "# Variables\n",
        'target     <- "', input$target, '"\n',
        'predictors <- c(', paste0('"', input$predictors, '"', collapse = ", "), ")\n"
      )
      message(bloc)
      current <- code_log()
      current$dataset <- bloc
      code_log(current)
    })

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

## To be copied in the UI
# mod_variables_ui("variables_1")

## To be copied in the server
# mod_variables_server("variables_1")
