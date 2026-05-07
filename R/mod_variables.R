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

    # Cible fixee par dataset
    fixed_target <- reactive({
      nms <- names(dataset_r())
      if ("median_house_value" %in% nms) return("median_house_value")
      if ("survived"           %in% nms) return("survived")
      if ("species"            %in% nms) return("species")
      nms[1]
    })

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

    # Affichage fixe de la variable cible
    output$target_ui <- renderUI({
      req(fixed_target())
      div(
        tags$label("Variable r\u00e9ponse :"),
        div(
          style = "padding: 0.4rem 0.75rem; background-color: #EEF6FB; border-radius: 6px; font-weight: 600;",
          fixed_target()
        )
      )
    })

    # Quand la cible change, mettre a jour les predicteurs (exclure la cible)
    observeEvent(fixed_target(), {
      req(fixed_target())
      choices_pred <- classify_variables(dataset_r(), exclude = fixed_target())

      output$predictors_ui <- renderUI({
        selectInput(ns("predictors"), "Variables pr\u00e9dictives :",
                    choices  = choices_pred,
                    multiple = TRUE,
                    selected = names(unlist(choices_pred)))
      })
    })

    # Type de tache
    task_type <- reactive({
      req(fixed_target())
      target_col <- dataset_r()[[fixed_target()]]
      if (is.numeric(target_col) && !is.factor(target_col)) "regression" else "classification"
    })

    # Encadre contextuel selon la variable cible
    output$task_type_ui <- renderUI({
      req(task_type(), fixed_target(), dataset_r())

      is_reg <- task_type() == "regression"
      target <- fixed_target()

      if (is_reg) {
        msg <- tagList(
          tags$strong(target), " est une variable num\u00e9rique continue.",
          " Pr\u00e9dire une valeur chiffr\u00e9e, c\u2019est de la ",
          tags$strong("r\u00e9gression"), "."
        )
        couleur <- "#6BAED6"
        label   <- "T\u00e2che : r\u00e9gression"
      } else {
        niveaux     <- levels(factor(dataset_r()[[target]]))
        n           <- length(niveaux)
        niveaux_str <- paste(
          paste0("\u00ab\u00a0", niveaux, "\u00a0\u00bb"),
          collapse = ", "
        )
        type_str <- if (n == 2) "classification binaire" else "classification multiclasse"
        msg <- tagList(
          tags$strong(target), " est une cat\u00e9gorie avec ", n,
          " valeurs possibles : ", niveaux_str, ".",
          " Pr\u00e9dire une cat\u00e9gorie, c\u2019est de la ",
          tags$strong(type_str), "."
        )
        couleur <- "#F17D52"
        label   <- paste0("T\u00e2che : ", type_str)
      }

      div(
        style = paste0(
          "margin-top: 0.75rem; padding: 0.6rem 1rem; border-radius: 6px; ",
          "border-left: 4px solid ", couleur, "; background-color: #F8FAFB;"
        ),
        p(style = paste0("margin: 0 0 0.25rem 0; font-weight: 600; color: ", couleur, ";"), label),
        p(style = "margin: 0;", msg)
      )
    })

    # Validation
    validated <- reactiveVal(FALSE)

    observeEvent(input$validate, {
      req(fixed_target(), input$predictors)
      validated(TRUE)

      bloc <- paste0(
        "# Chargement des donnees\n",
        "library(datapyranhia)\n",
        "library(tidymodels)\n\n",
        'data("', dataset_name(), '", package = "datapyranhia")\n\n',
        "# Variables\n",
        'target     <- "', fixed_target(), '"\n',
        'predictors <- c(', paste0('"', input$predictors, '"', collapse = ", "), ")\n"
      )
      message(bloc)
      current <- code_log()
      current$dataset <- bloc
      code_log(current)
    })

    observeEvent(dataset_r(), {
      validated(FALSE)
    })
    observeEvent(list(fixed_target(), input$predictors), {
      validated(FALSE)
    }, ignoreInit = TRUE)

    return(list(
      target     = fixed_target,
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
