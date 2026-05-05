#' modelisation UI Function
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @importFrom shiny NS tagList
mod_modelisation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::card(
      bslib::card_header("Param\u00e8tres du mod\u00e8le"),
      sliderInput(
        ns("n_trees"),
        "Nombre d'arbres :",
        min = 100, max = 500, value = 100, step = 50
      ),
      div(
        style = "text-align: center; margin-top: 1rem;",
        actionButton(ns("train"), "Entra\u00eener le mod\u00e8le",
                     class = "btn-success btn-lg")
      )
    ),
    uiOutput(ns("results_section"))
  )
}

#' modelisation Server Functions
#'
#' @noRd
#' @importFrom parsnip rand_forest set_engine set_mode fit
#' @importFrom workflows workflow add_recipe add_model
mod_modelisation_server <- function(id, pretraitement_r, vars_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    fitted_workflow_r <- reactiveVal(NULL)

    observeEvent(input$train, {
      req(
        pretraitement_r$train(),
        pretraitement_r$test(),
        pretraitement_r$recipe(),
        vars_r$task_type()
      )

      withProgress(message = "Entra\u00eenement en cours...", value = 0, {

        incProgress(0.2, detail = "Pr\u00e9paration de la recette...")
        mode <- if (vars_r$task_type() == "regression") "regression" else "classification"

        incProgress(0.2, detail = "Sp\u00e9cification du mod\u00e8le...")
        spec <- parsnip::rand_forest(trees = input$n_trees) |>
          parsnip::set_engine("ranger") |>
          parsnip::set_mode(mode)

        incProgress(0.2, detail = "Construction du workflow...")
        wf <- workflows::workflow() |>
          workflows::add_recipe(pretraitement_r$recipe()) |>
          workflows::add_model(spec)

        incProgress(0.2, detail = "Entra\u00eenement du mod\u00e8le...")
        fitted <- parsnip::fit(wf, data = pretraitement_r$train())

        incProgress(0.2, detail = "Termin\u00e9 !")
        fitted_workflow_r(fitted)
      })
    })

    # Section resultats
    output$results_section <- renderUI({
      req(fitted_workflow_r())
      tagList(
        hr(),
        bslib::card(
          bslib::card_header("Mod\u00e8le entra\u00een\u00e9"),
          verbatimTextOutput(ns("model_summary")),
          hr(),
          div(
            style = "text-align: center; margin-top: 1rem;",
            actionButton(ns("validate"), "Valider et passer \u00e0 l'\u00e9valuation",
                         class = "btn-primary btn-lg")
          )
        )
      )
    })

    output$model_summary <- renderPrint({
      req(fitted_workflow_r())
      fitted_workflow_r()
    })

    # Validation
    validated <- reactiveVal(FALSE)

    observeEvent(input$validate, {
      validated(TRUE)
    })

    observeEvent(list(pretraitement_r$train(), pretraitement_r$recipe()), {
      validated(FALSE)
      fitted_workflow_r(NULL)
    })

    return(list(
      fitted_workflow = fitted_workflow_r,
      validated       = validated
    ))
  })
}

## To be copied in the UI
# mod_modelisation_ui("modelisation_1")

## To be copied in the server
# mod_modelisation_server("modelisation_1")
