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
    bslib::card(
      style = "background-color: #f0f7ff;",
      bslib::card_body(
        p(style = "font-size: 1rem; margin: 0;",
          "L'\u00e9valuation mesure la qualit\u00e9 des pr\u00e9dictions du mod\u00e8le sur le ",
          strong("jeu de test"), ", des donn\u00e9es qu'il n'a jamais vues pendant l'entra\u00eenement."),
        p(style = "font-size: 1rem; margin: 0;",
          "Dans le cas d’une ", strong("r\u00e9gression"), ", la ", strong("RMSE"),
          " mesure l'erreur moyenne de pr\u00e9diction, exprim\u00e9e dans la m\u00eame unit\u00e9 que
          la variable cible. Le ", strong("R\u00b2"), " indique la proportion de variance
          expliqu\u00e9e par le mod\u00e8le (entre 0 et 1)."),
        p(style = "font-size: 1rem; margin: 0;",
          "Dans le cas d’une ", strong("classification"), ", l'", strong("accuracy"),
          " mesure le taux de bonnes pr\u00e9dictions. La ", strong("pr\u00e9cision"),
          " mesure parmi les pr\u00e9dictions positives combien sont correctes, le ",
          strong("recall"), " mesure parmi les vrais positifs combien ont \u00e9t\u00e9 d\u00e9tect\u00e9s.
          La ", strong("matrice de confusion"), " d\u00e9taille les erreurs par classe.")
      )
    ),
    br(),
    uiOutput(ns("main_section"))
  )
}

#' evaluation Server Functions
#'
#' @noRd
#' @importFrom yardstick rmse rsq accuracy precision recall conf_mat
#' @importFrom parsnip predict.model_fit
#' @importFrom ggplot2 autoplot
mod_evaluation_server <- function(id, pretraitement_r, modelisation_r, vars_r, code_log) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Predictions sur le test set
    predictions_r <- reactive({
      req(modelisation_r$fitted_workflow(), pretraitement_r$test())
      fitted <- modelisation_r$fitted_workflow()
      test   <- pretraitement_r$test()
      preds  <- predict(fitted, new_data = test)
      dplyr::bind_cols(test, preds)
    })

    # Log evaluation
    observeEvent(predictions_r(), {
      req(vars_r$task_type(), vars_r$target())
      df         <- predictions_r()
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
      code_log(c(code_log(), list(bloc)))
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

        bslib::layout_columns(
          col_widths = c(3, 3),
          bslib::value_box(
            title = "RMSE",
            value = round(rmse_val, 3),
            theme = "primary"
          ),
          bslib::value_box(
            title = "R\u00b2",
            value = round(rsq_val, 3),
            theme = "success"
          )
        )
      } else {
        acc_val <- yardstick::accuracy(df,
                                       truth    = !!rlang::sym(target_col),
                                       estimate = .pred_class
        )$.estimate

        tagList(
          # Ligne 1 : accuracy
          bslib::layout_columns(
            col_widths = c(3),
            bslib::value_box(
              title = "Accuracy",
              value = scales::percent(acc_val, accuracy = 0.1),
              theme = "primary"
            )
          ),
          br(),
          # Ligne 2 : CM + metriques par classe
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
        n_cls <- sum(df[[target_col]] == cls)
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
