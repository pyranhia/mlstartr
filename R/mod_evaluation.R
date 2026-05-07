#' evaluation UI Function
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @importFrom shiny NS tagList
#' @importFrom stats predict
utils::globalVariables(c(".pred", ".pred_class", ".pred_bin", ".truth_bin"))
mod_evaluation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("card_intro")),
    br(),
    uiOutput(ns("main_section")),
    hr(),
    div(
      style = "text-align: center; margin-top: 1rem;",
      actionButton(ns("go_export"), "Voir le code R g\u00e9n\u00e9r\u00e9",
                   class = "btn-primary btn-lg")
    )
  )
}

#' evaluation Server Functions
#'
#' @noRd
#' @importFrom yardstick rmse rsq accuracy precision recall conf_mat
#' @importFrom parsnip predict.model_fit
#' @importFrom ggplot2 autoplot
mod_evaluation_server <- function(id, pretraitement_r, modelisation_r, vars_r, code_log, session_root) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Predictions sur le test set
    predictions_r <- reactive({
      req(modelisation_r$fitted_workflow(), pretraitement_r$test())
      fitted <- modelisation_r$fitted_workflow()
      test   <- pretraitement_r$test()
      cols_modele <- fitted$pre$mold$blueprint$ptypes$predictors |> names()
      req(all(cols_modele %in% names(test)))
      preds <- predict(fitted, new_data = test)
      dplyr::bind_cols(test, preds)
    })

    observeEvent(input$go_export, {
      bslib::nav_select(id = "tabs", selected = "export", session = session_root)
    })

    # Card intro adaptative selon le type de tache
    output$card_intro <- renderUI({
      req(vars_r$task_type())
      task <- vars_r$task_type()

      if (task == "regression") {
        bslib::card(
          class = "card-pedagogique",
          bslib::card_body(
            p(style = "font-size: 1rem; margin: 0;",
              "Le mod\u00e8le est maintenant \u00e9valu\u00e9 sur le ",
              strong("jeu de test"), " \u003a des donn\u00e9es qu\u2019il n\u2019a jamais vues. ",
              "C\u2019est le vrai test de ses capacit\u00e9s de g\u00e9n\u00e9ralisation."),
            p(style = "font-size: 1rem; margin: 0; margin-top: 0.5rem;",
              "La ", strong("RMSE"), " mesure l\u2019erreur moyenne de pr\u00e9diction, exprim\u00e9e dans la m\u00eame unit\u00e9 que ",
              vars_r$target(), ". ",
              "Le ", strong("R\u00b2"), " indique la proportion de variance expliqu\u00e9e par le mod\u00e8le \u003a ",
              "plus il est proche de 1, mieux c\u2019est.")
          )
        )
      } else {
        bslib::card(
          class = "card-pedagogique",
          bslib::card_body(
            p(style = "font-size: 1rem; margin: 0;",
              "Le mod\u00e8le est maintenant \u00e9valu\u00e9 sur le ",
              strong("jeu de test"), " \u003a des donn\u00e9es qu\u2019il n\u2019a jamais vues. ",
              "C\u2019est le vrai test de ses capacit\u00e9s de g\u00e9n\u00e9ralisation."),
            p(style = "font-size: 1rem; margin: 0; margin-top: 0.5rem;",
              "L\u2019", strong("accuracy"), " mesure le taux global de bonnes pr\u00e9dictions. ",
              "La ", strong("matrice de confusion"), " d\u00e9taille les erreurs par classe : ",
              "chaque ligne repr\u00e9sente ce que le mod\u00e8le a pr\u00e9dit, ",
              "chaque colonne ce qui \u00e9tait r\u00e9ellement observ\u00e9. ",
              "Les cases de la diagonale sont les bonnes pr\u00e9dictions.")
          )
        )
      }
    })

    # Log evaluation
    observeEvent(predictions_r(), {
      req(vars_r$task_type(), vars_r$target())
      target_col <- vars_r$target()
      task       <- vars_r$task_type()

      if (task == "regression") {
        bloc <- paste0(
          "# Evaluation\n",
          "test_pred <- predict(fitted, new_data = test) |>\n",
          "  bind_cols(test)\n\n",
          "metrics <- metric_set(rmse, rsq)\n",
          "metrics(test_pred, truth = ", target_col, ", estimate = .pred)\n"
        )
      } else {
        bloc <- paste0(
          "# Evaluation\n",
          "test_pred <- predict(fitted, new_data = test) |>\n",
          "  bind_cols(test)\n\n",
          "accuracy(test_pred, truth = ", target_col, ", estimate = .pred_class)\n",
          "conf_mat(test_pred, truth = ", target_col, ", estimate = .pred_class)\n"
        )
      }
      message(bloc)
      current <- code_log()
      current$evaluation <- bloc
      code_log(current)
    })

    output$main_section <- renderUI({
      req(predictions_r(), vars_r$task_type(), vars_r$target())
      task       <- vars_r$task_type()
      target_col <- vars_r$target()
      df         <- predictions_r()

      if (task == "regression") {
        rmse_val <- yardstick::rmse(df,
                                    truth    = !!rlang::sym(target_col),
                                    estimate = .pred
        )$.estimate
        rsq_val <- yardstick::rsq(df,
                                  truth    = !!rlang::sym(target_col),
                                  estimate = .pred
        )$.estimate

        # Interpretation RMSE et R2
        rsq_qualite <- if (rsq_val >= 0.8) {
          list(couleur = "#00A896", texte = "tr\u00e8s bonne")
        } else if (rsq_val >= 0.6) {
          list(couleur = "#6BAED6", texte = "correcte")
        } else {
          list(couleur = "#F17D52", texte = "faible")
        }

        tagList(
          bslib::layout_columns(
            col_widths = c(3, 3),
            bslib::value_box(
              title = "RMSE",
              value = round(rmse_val, 1),
              theme = "primary"
            ),
            bslib::value_box(
              title = "R\u00b2",
              value = round(rsq_val, 3),
              theme = "success"
            )
          ),
          div(
            style = paste0(
              "margin-top: 0.75rem; padding: 0.6rem 1rem; border-radius: 6px; ",
              "border-left: 4px solid ", rsq_qualite$couleur, "; background-color: #F8FAFB;"
            ),
            p(style = paste0("margin: 0 0 0.25rem 0; font-weight: 600; color: ", rsq_qualite$couleur, ";"),
              paste0("Performance ", rsq_qualite$texte, " (R\u00b2 = ", round(rsq_val, 3), ")")),
            p(style = "margin: 0; font-size: 0.9rem;",
              paste0(
                "Le mod\u00e8le explique ", round(rsq_val * 100), "% de la variance de ", target_col, ". ",
                "En moyenne, ses pr\u00e9dictions s\u2019\u00e9cartent de ", round(rmse_val, 1),
                " de la valeur r\u00e9elle."
              )
            )
          )
        )
      } else {
        acc_val <- yardstick::accuracy(df,
                                       truth    = !!rlang::sym(target_col),
                                       estimate = .pred_class
        )$.estimate

        acc_qualite <- if (acc_val >= 0.9) {
          list(couleur = "#00A896", texte = "tr\u00e8s bonne")
        } else if (acc_val >= 0.75) {
          list(couleur = "#6BAED6", texte = "correcte")
        } else {
          list(couleur = "#F17D52", texte = "faible")
        }

        tagList(
          bslib::layout_columns(
            col_widths = c(3),
            bslib::value_box(
              title = "Accuracy",
              value = scales::percent(acc_val, accuracy = 0.1),
              theme = "primary"
            )
          ),
          div(
            style = paste0(
              "margin-top: 0.75rem; margin-bottom: 1rem; padding: 0.6rem 1rem; border-radius: 6px; ",
              "border-left: 4px solid ", acc_qualite$couleur, "; background-color: #F8FAFB;"
            ),
            p(style = paste0("margin: 0 0 0.25rem 0; font-weight: 600; color: ", acc_qualite$couleur, ";"),
              paste0("Performance ", acc_qualite$texte, " (accuracy = ", scales::percent(acc_val, accuracy = 0.1), ")")),
            p(style = "margin: 0; font-size: 0.9rem;",
              paste0(
                "Le mod\u00e8le pr\u00e9dit correctement ", scales::percent(acc_val, accuracy = 0.1),
                " des observations du jeu de test. ",
                "La matrice de confusion ci-dessous d\u00e9taille les erreurs par classe."
              )
            )
          ),
          br(),
          bslib::layout_columns(
            col_widths = c(6, 6),
            bslib::card(
              bslib::card_header("Matrice de confusion"),
              plotOutput(ns("cm_plot"), height = "300px")
            ),
            bslib::card(
              bslib::card_header("M\u00e9triques par classe"),
              DT::DTOutput(ns("class_metrics"))
            )
          )
        )
      }
    })

    output$cm_plot <- renderPlot({
      req(predictions_r(), vars_r$target())
      df         <- predictions_r()
      target_col <- vars_r$target()

      cm <- yardstick::conf_mat(df,
                                truth    = !!rlang::sym(target_col),
                                estimate = .pred_class
      )
      autoplot(cm, type = "heatmap") +
        ggplot2::scale_fill_gradient(low = "white", high = "#6BAED6") +
        ggplot2::theme_minimal(base_size = 14) +
        ggplot2::labs(x = "Pr\u00e9diction", y = "R\u00e9alit\u00e9") +
        ggplot2::coord_fixed()
    })

    output$class_metrics <- DT::renderDT({
      req(predictions_r(), vars_r$target())
      df         <- predictions_r()
      target_col <- vars_r$target()
      classes    <- levels(df[[target_col]])

      metrics_df <- purrr::map_dfr(classes, function(cls) {
        n_cls  <- sum(df[[target_col]] == cls)
        df_bin <- df |>
          dplyr::mutate(
            .truth_bin = factor(
              ifelse(!!rlang::sym(target_col) == cls, cls, "other"),
              levels = c(cls, "other")
            ),
            .pred_bin = factor(
              ifelse(.pred_class == cls, cls, "other"),
              levels = c(cls, "other")
            )
          )
        prec_cls <- yardstick::precision(df_bin,
                                         truth = .truth_bin, estimate = .pred_bin
        )$.estimate
        rec_cls <- yardstick::recall(df_bin,
                                     truth = .truth_bin, estimate = .pred_bin
        )$.estimate

        data.frame(
          Classe    = cls,
          n         = n_cls,
          Precision = round(prec_cls, 3),
          Recall    = round(rec_cls, 3)
        )
      })

      DT::datatable(
        metrics_df,
        rownames = FALSE,
        options  = list(dom = "t", pageLength = 10)
      )
    })
  })
}

## To be copied in the UI
# mod_evaluation_ui("evaluation_1")

## To be copied in the server
# mod_evaluation_server("evaluation_1")
