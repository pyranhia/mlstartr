#' exploration UI Function
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @importFrom shiny NS tagList
mod_exploration_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::card(
      style = "background-color: #f0f7ff;",
      bslib::card_body(
        p(style = "font-size: 1rem; margin: 0;",
          "Avant d'entra\u00eener un mod\u00e8le, il est essentiel de ", strong("comprendre vos donn\u00e9es"),
          ". Observez la distribution de la variable r\u00e9ponse : est-elle sym\u00e9trique, asym\u00e9trique ?
          Rep\u00e9rez les valeurs manquantes. Explorez les corr\u00e9lations entre pr\u00e9dicteurs :
          des variables tr\u00e8s corr\u00e9l\u00e9es entre elles apportent une information redondante.")
      )
    ),
    br(),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header("Distribution de la variable r\u00e9ponse"),
        uiOutput(ns("target_options")),
        plotOutput(ns("target_plot"))
      ),
      bslib::card(
        bslib::card_header("Matrice de corr\u00e9lation des pr\u00e9dicteurs"),
        uiOutput(ns("corr_section"))
      )
    ),
    br(),
    bslib::card(
      bslib::card_header("Statistiques descriptives des pr\u00e9dicteurs"),
      DT::DTOutput(ns("desc_stats"))
    ),
    hr(),
    div(
      style = "text-align: center; margin-top: 1rem;",
      actionButton(ns("validate"), "Valider et passer au pr\u00e9traitement",
                   class = "btn-primary btn-lg")
    )
  )
}

#' exploration Server Functions
#'
#' @noRd
#' @importFrom ggplot2 ggplot aes geom_histogram geom_bar geom_tile geom_text
#'   scale_fill_gradient2 labs theme_minimal theme element_text
#' @importFrom stats cor median
#' @importFrom rlang .data
utils::globalVariables(c("Var1", "Var2", "Correlation"))
mod_exploration_server <- function(id, dataset_r, vars_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    base_theme <- function() {
      theme_minimal(base_size = 14)
    }

    selected_data <- reactive({
      req(dataset_r(), vars_r$target(), vars_r$predictors())
      dataset_r()[, c(vars_r$target(), vars_r$predictors()), drop = FALSE]
    })

    # Option transformation axe X (visuelle uniquement)
    output$target_options <- renderUI({
      req(vars_r$task_type())
      if (vars_r$task_type() != "regression") return(NULL)
      selectInput(
        ns("axis_transform"),
        "Transformation de l'axe X (visualisation uniquement) :",
        choices = c("Aucune" = "none", "Racine carr\u00e9e" = "sqrt", "Logarithme" = "log"),
        selected = "none"
      )
    })

    # Distribution de la variable cible
    output$target_plot <- renderPlot({
      req(selected_data(), vars_r$target(), vars_r$task_type())
      target_col <- vars_r$target()
      task       <- vars_r$task_type()
      df         <- selected_data()

      if (task == "regression") {
        x_vals <- df[[target_col]]
        transf <- if (!is.null(input$axis_transform)) input$axis_transform else "none"
        x_vals <- switch(transf,
                         "sqrt" = sqrt(x_vals),
                         "log"  = log(x_vals),
                         x_vals
        )
        df[[target_col]] <- x_vals
        x_label <- switch(transf,
                          "sqrt" = paste("sqrt(", target_col, ")"),
                          "log"  = paste("log(", target_col, ")"),
                          target_col
        )
        ggplot(df, aes(x = .data[[target_col]])) +
          geom_histogram(fill = "#6BAED6", color = "white", bins = 30) +
          labs(x = x_label, y = "Effectif") +
          base_theme()
      } else {
        ggplot(df, aes(x = .data[[target_col]])) +
          geom_bar(fill = "#F17D52") +
          labs(x = target_col, y = "Effectif") +
          base_theme()
      }
    })

    # Stats descriptives
    output$desc_stats <- DT::renderDT({
      req(selected_data(), vars_r$predictors())
      df       <- selected_data()[, vars_r$predictors(), drop = FALSE]
      num_vars <- df[, sapply(df, is.numeric), drop = FALSE]
      if (ncol(num_vars) == 0) return(DT::datatable(data.frame()))

      stats <- data.frame(
        Variable = names(num_vars),
        Min      = sapply(num_vars, min,    na.rm = TRUE),
        Moyenne  = sapply(num_vars, mean,   na.rm = TRUE),
        Mediane  = sapply(num_vars, median, na.rm = TRUE),
        Max      = sapply(num_vars, max,    na.rm = TRUE),
        NAs      = sapply(num_vars, function(x) sum(is.na(x)))
      )
      DT::datatable(
        stats,
        rownames = FALSE,
        options  = list(pageLength = 10, dom = "t")
      ) |>
        DT::formatRound(columns = c("Min", "Moyenne", "Mediane", "Max"), digits = 2)
    })

    # Matrice de correlation
    output$corr_section <- renderUI({
      req(selected_data(), vars_r$predictors())
      df       <- selected_data()[, vars_r$predictors(), drop = FALSE]
      num_vars <- df[, sapply(df, is.numeric), drop = FALSE]

      if (ncol(num_vars) >= 2) {
        plotOutput(ns("corr_plot"))
      } else {
        p("Pas assez de variables num\u00e9riques pour calculer les corr\u00e9lations.")
      }
    })

    output$corr_plot <- renderPlot({
      req(selected_data(), vars_r$predictors())
      df       <- selected_data()[, vars_r$predictors(), drop = FALSE]
      num_vars <- df[, sapply(df, is.numeric), drop = FALSE]
      if (ncol(num_vars) < 2) return(NULL)

      corr_mat <- cor(num_vars, use = "pairwise.complete.obs")
      corr_df  <- as.data.frame(as.table(corr_mat))
      names(corr_df) <- c("Var1", "Var2", "Correlation")

      ggplot(corr_df, aes(x = .data[["Var1"]], y = .data[["Var2"]], fill = .data[["Correlation"]])) +
        geom_tile(color = "white") +
        scale_fill_gradient2(
          low = "#F17D52", mid = "white", high = "#6BAED6",
          midpoint = 0, limits = c(-1, 1)
        ) +
        geom_text(aes(label = round(.data[["Correlation"]], 2)), size = 4) +
        labs(x = NULL, y = NULL) +
        ggplot2::coord_fixed() +
        base_theme() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })

    # Validation
    validated <- reactiveVal(FALSE)

    observeEvent(input$validate, {
      validated(TRUE)
    })

    observeEvent(list(dataset_r(), vars_r$target(), vars_r$predictors()), {
      validated(FALSE)
    })

    return(list(validated = validated))
  })
}

## To be copied in the UI
# mod_exploration_ui("exploration_1")

## To be copied in the server
# mod_exploration_server("exploration_1")
