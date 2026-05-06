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
      style = "background-color: #f0f7ff;",
      bslib::card_body(
        p(style = "font-size: 1rem; margin: 0;",
          "Le ", strong("Random Forest"), " est un algorithme qui construit un grand nombre
          d'arbres de d\u00e9cision et combine leurs pr\u00e9dictions. Plus le nombre d'arbres
          est \u00e9lev\u00e9, plus le mod\u00e8le est stable, mais plus l'entra\u00eenement est long.
          Une fois entra\u00een\u00e9, le mod\u00e8le est pr\u00eat \u00e0 \u00eatre \u00e9valu\u00e9 sur le jeu de test.")
      )
    ),
    br(),
    bslib::layout_columns(
      col_widths = c(4, 8),
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
  )
}

#' modelisation Server Functions
#'
#' @noRd
#' @importFrom parsnip rand_forest set_engine set_mode fit
#' @importFrom workflows workflow add_recipe add_model
mod_modelisation_server <- function(id, pretraitement_r, vars_r, code_log) {
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

        bloc <- paste0(
          "# Modelisation\n",
          "spec <- rand_forest(trees = ", input$n_trees, ") |>\n",
          "  set_engine('ranger') |>\n",
          "  set_mode('", if (vars_r$task_type() == "regression") "regression" else "classification", "')\n\n",
          "wf <- workflow() |>\n",
          "  add_recipe(rec) |>\n",
          "  add_model(spec)\n\n",
          "fitted <- fit(wf, data = train)\n"
        )
        message(bloc)
        code_log(c(code_log(), list(bloc)))
      })
    })

    # Section resultats
    output$results_section <- renderUI({
      req(fitted_workflow_r())
      bslib::card(
        bslib::card_header("Mod\u00e8le entra\u00een\u00e9"),
        bslib::card_body(
          uiOutput(ns("model_summary")),
          hr(),
          div(
            style = "text-align: center; margin-top: 1rem;",
            actionButton(ns("validate"), "Valider et passer \u00e0 l'\u00e9valuation",
                         class = "btn-primary btn-lg")
          )
        )
      )
    })

    output$model_summary <- renderUI({
      req(fitted_workflow_r())
      wf  <- fitted_workflow_r()
      fit <- wf$fit$fit$fit

      tagList(
        tags$table(
          class = "table table-sm",
          tags$tbody(
            tags$tr(
              tags$td(strong("Type")),
              tags$td(fit$treetype)
            ),
            tags$tr(
              tags$td(strong("Nombre d'arbres")),
              tags$td(fit$num.trees)
            ),
            tags$tr(
              tags$td(strong("Observations d'entra\u00eenement")),
              tags$td(fit$num.samples)
            ),
            tags$tr(
              tags$td(strong("Variables utilis\u00e9es")),
              tags$td(fit$num.independent.variables)
            ),
            if (vars_r$task_type() == "regression") {
              tags$tr(
                tags$td(strong("R\u00b2 estim\u00e9")),
                tags$td(round(fit$r.squared, 3))
              )
            }
          )
        )
      )
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
