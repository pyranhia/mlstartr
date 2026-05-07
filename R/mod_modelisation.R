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
      class = "card-pedagogique",
      bslib::card_body(
        p(style = "font-size: 1rem; margin: 0;",
          "Le ", strong("Random Forest"), " construit un grand nombre d\u2019arbres de d\u00e9cision
          sur des sous-\u00e9chantillons al\u00e9atoires des donn\u00e9es, puis combine leurs pr\u00e9dictions.
          Cette combinaison le rend robuste et g\u00e9n\u00e9ralement plus performant qu\u2019un seul arbre.
          C\u2019est un bon algorithme de d\u00e9part : il fonctionne bien sur beaucoup de probl\u00e8mes
          sans n\u00e9cessiter beaucoup de r\u00e9glages.")
      )
    ),
    br(),
    bslib::layout_columns(
      col_widths = c(4, 8),
      bslib::card(
        bslib::card_header("Param\u00e8tres du mod\u00e8le"),
        bslib::card_body(
          sliderInput(
            ns("n_trees"),
            "Nombre d'arbres :",
            min = 100, max = 500, value = 100, step = 50
          ),
          p(style = "font-size: 0.875rem; color: #555; margin-top: -0.5rem;",
            "Plus le nombre d\u2019arbres est \u00e9lev\u00e9, plus le mod\u00e8le est stable \u2014
            mais plus l\u2019entra\u00eenement est long. 100 arbres est un bon point de d\u00e9part."),
          div(
            style = "text-align: center; margin-top: 1rem;",
            actionButton(ns("train"), "Entra\u00eener le mod\u00e8le",
                         class = "btn-success btn-lg")
          )
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
mod_modelisation_server <- function(id, pretraitement_r, vars_r, code_log, dataset_r) {
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
          "  set_mode('", mode, "')\n\n",
          "wf <- workflow() |>\n",
          "  add_recipe(rec) |>\n",
          "  add_model(spec)\n\n",
          "fitted <- fit(wf, data = train)\n"
        )
        message(bloc)
        current <- code_log()
        current$modelisation <- bloc
        code_log(current)
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
            actionButton(ns("validate"), "Valider et passer \u00e0 l\u2019\u00e9valuation",
                         class = "btn-primary btn-lg")
          )
        )
      )
    })

    output$model_summary <- renderUI({
      req(fitted_workflow_r(), vars_r$task_type())
      wf   <- fitted_workflow_r()
      fit  <- wf$fit$fit$fit
      task <- vars_r$task_type()

      # Tableau de résumé
      resume <- tags$table(
        class = "table table-sm",
        tags$tbody(
          tags$tr(
            tags$td(strong("Nombre d\u2019arbres")),
            tags$td(fit$num.trees)
          ),
          tags$tr(
            tags$td(strong("Observations d\u2019entra\u00eenement")),
            tags$td(fit$num.samples)
          ),
          tags$tr(
            tags$td(strong("Variables utilis\u00e9es par arbre")),
            tags$td(fit$mtry)
          )
        )
      )

      # Interpretation contextuelle
      interpretation <- if (task == "regression") {
        rsq <- round(fit$r.squared, 3)
        qualite <- if (rsq >= 0.8) {
          list(couleur = "#00A896", texte = "tr\u00e8s bonne")
        } else if (rsq >= 0.6) {
          list(couleur = "#6BAED6", texte = "correcte")
        } else {
          list(couleur = "#F17D52", texte = "faible")
        }
        div(
          style = paste0(
            "margin-top: 0.75rem; padding: 0.6rem 1rem; border-radius: 6px; ",
            "border-left: 4px solid ", qualite$couleur, "; background-color: #F8FAFB;"
          ),
          p(style = paste0("margin: 0 0 0.25rem 0; font-weight: 600; color: ", qualite$couleur, ";"),
            paste0("R\u00b2 estim\u00e9 : ", rsq)),
          p(style = "margin: 0; font-size: 0.9rem;",
            paste0(
              "Sur le jeu d\u2019entra\u00eenement, le mod\u00e8le explique ", round(rsq * 100), "% ",
              "de la variance de ", vars_r$target(), ". ",
              "C\u2019est une performance ", qualite$texte, ". ",
              "L\u2019\u00e9valuation sur le jeu de test donnera une image plus fiable."
            )
          )
        )
      } else {
        # Classification : afficher l'erreur OOB
        oob <- round(fit$prediction.error * 100, 1)
        qualite <- if (oob <= 10) {
          list(couleur = "#00A896", texte = "tr\u00e8s bon")
        } else if (oob <= 25) {
          list(couleur = "#6BAED6", texte = "correct")
        } else {
          list(couleur = "#F17D52", texte = "faible")
        }
        div(
          style = paste0(
            "margin-top: 0.75rem; padding: 0.6rem 1rem; border-radius: 6px; ",
            "border-left: 4px solid ", qualite$couleur, "; background-color: #F8FAFB;"
          ),
          p(style = paste0("margin: 0 0 0.25rem 0; font-weight: 600; color: ", qualite$couleur, ";"),
            paste0("Erreur estim\u00e9e : ", oob, "%")),
          p(style = "margin: 0; font-size: 0.9rem;",
            paste0(
              "Le mod\u00e8le se trompe sur environ ", oob, "% des observations. ",
              "C\u2019est un indicateur pr\u00e9liminaire \u2014 ",
              "l\u2019\u00e9valuation sur le jeu de test donnera le verdict final."
            )
          )
        )
      }

      tagList(resume, interpretation)
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

    observeEvent(input$n_trees, {
      validated(FALSE)
      fitted_workflow_r(NULL)
    }, ignoreInit = TRUE)

    observeEvent(dataset_r(), {
      validated(FALSE)
      fitted_workflow_r(NULL)
    }, ignoreInit = TRUE)

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
