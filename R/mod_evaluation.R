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
    uiOutput(ns("metrics_section")),
    uiOutput(ns("cm_section"))
  )
}

#' evaluation Server Functions
#'
#' @noRd
#' @importFrom yardstick rmse rsq accuracy precision recall conf_mat
#' @importFrom tune collect_predictions
#' @importFrom parsnip predict.model_fit
#' @importFrom ggplot2 autoplot
mod_evaluation_server <- function(id, pretraitement_r, modelisation_r, vars_r) {
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

    # Section metriques
    output$metrics_section <- renderUI({
      req(predictions_r(), vars_r$task_type(), vars_r$target())
      task       <- vars_r$task_type()
      target_col <- vars_r$target()
      df         <- predictions_r()

      if (task == "regression") {
        rmse_val <- yardstick::rmse(df, truth = !!rlang::sym(target_col), estimate = .pred)$.estimate
        rsq_val  <- yardstick::rsq(df,  truth = !!rlang::sym(target_col), estimate = .pred)$.estimate

        bslib::layout_columns(
          col_widths = c(6, 6),
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

        bslib::value_box(
          title = "Accuracy",
          value = scales::percent(acc_val, accuracy = 0.1),
          theme = "primary"
        )
      }
    })

    # Matrice de confusion (classification uniquement)
    output$cm_section <- renderUI({
      req(vars_r$task_type() == "classification")
      tagList(
        hr(),
        bslib::card(
          bslib::card_header("Matrice de confusion"),
          plotOutput(ns("cm_plot")),
          hr(),
          bslib::card_header("M\u00e9triques par classe"),
          DT::DTOutput(ns("class_metrics"))
        )
      )
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

      prec <- yardstick::precision(df,
                                   truth     = !!rlang::sym(target_col),
                                   estimate  = .pred_class,
                                   estimator = "macro"
      )

      rec <- yardstick::recall(df,
                               truth     = !!rlang::sym(target_col),
                               estimate  = .pred_class,
                               estimator = "macro"
      )

      # Metriques par classe
      classes <- levels(df[[target_col]])

      metrics_df <- purrr::map_dfr(classes, function(cls) {
        df_bin <- df |>
          dplyr::mutate(
            .truth_bin = factor(ifelse(!!rlang::sym(target_col) == cls, cls, "other"), levels = c(cls, "other")),
            .pred_bin  = factor(ifelse(.pred_class == cls, cls, "other"), levels = c(cls, "other"))
          )
        prec_cls <- yardstick::precision(df_bin, truth = .truth_bin, estimate = .pred_bin)$.estimate
        rec_cls  <- yardstick::recall(df_bin,    truth = .truth_bin, estimate = .pred_bin)$.estimate

        data.frame(
          Classe     = cls,
          Precision  = round(prec_cls, 3),
          Recall     = round(rec_cls, 3)
        )
      })

      DT::datatable(metrics_df, rownames = FALSE, options = list(dom = "t"))
    })
  })
}

## To be copied in the UI
# mod_evaluation_ui("evaluation_1")

## To be copied in the server
# mod_evaluation_server("evaluation_1")
