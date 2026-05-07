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
      class = "card-pedagogique",
      bslib::card_body(
        p(style = "font-size: 1rem; margin: 0;",
          "Avant d\u2019entra\u00eener un mod\u00e8le, il est essentiel de ", strong("comprendre vos donn\u00e9es"),
          ". Observez la distribution de la variable r\u00e9ponse, rep\u00e9rez les valeurs manquantes,
          et explorez les relations entre pr\u00e9dicteurs. Ces observations vont guider
          vos choix de pr\u00e9traitement \u00e0 l\u2019\u00e9tape suivante.")
      )
    ),
    br(),
    uiOutput(ns("contenu"))
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

    # Reactive qui ne se declenche que si dataset et variables sont coherents
    selected_data <- reactive({
      req(dataset_r(), vars_r$target(), vars_r$predictors())
      df        <- dataset_r()
      cols_want <- c(vars_r$target(), vars_r$predictors())
      req(all(cols_want %in% names(df)))
      df[, cols_want, drop = FALSE]
    })

    # Tout le contenu est conditionne par la coherence dataset/variables
    output$contenu <- renderUI({
      req(selected_data())
      tagList(
        bslib::layout_columns(
          col_widths = c(6, 6),
          bslib::card(
            bslib::card_header("Distribution de la variable r\u00e9ponse"),
            uiOutput(ns("target_options")),
            plotOutput(ns("target_plot")),
            uiOutput(ns("target_alert"))
          ),
          bslib::card(
            bslib::card_header("Matrice de corr\u00e9lation des pr\u00e9dicteurs"),
            uiOutput(ns("corr_section")),
            uiOutput(ns("corr_alert"))
          )
        ),
        br(),
        bslib::card(
          bslib::card_header("Statistiques descriptives des pr\u00e9dicteurs"),
          DT::DTOutput(ns("desc_stats")),
          uiOutput(ns("nas_alert"))
        ),
        hr(),
        div(
          style = "text-align: center; margin-top: 1rem;",
          actionButton(ns("validate"), "Valider et passer au pr\u00e9traitement",
                       class = "btn-primary btn-lg")
        )
      )
    })

    # Option transformation axe X (visuelle uniquement)
    output$target_options <- renderUI({
      req(selected_data(), vars_r$task_type())
      if (vars_r$task_type() != "regression") return(NULL)
      selectInput(
        ns("axis_transform"),
        "Transformation de l'axe X (visualisation uniquement) :",
        choices  = c("Aucune" = "none", "Racine carr\u00e9e" = "sqrt", "Logarithme" = "log"),
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
        transf  <- if (!is.null(input$axis_transform)) input$axis_transform else "none"
        x_vals  <- switch(transf, "sqrt" = sqrt(x_vals), "log" = log(x_vals), x_vals)
        df[[target_col]] <- x_vals
        x_label <- switch(transf,
                          "sqrt" = paste("sqrt(", target_col, ")"),
                          "log"  = paste("log(",  target_col, ")"),
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

    # Alerte distribution variable cible
    output$target_alert <- renderUI({
      req(selected_data(), vars_r$target(), vars_r$task_type())
      target_col <- vars_r$target()
      task       <- vars_r$task_type()
      df         <- selected_data()

      if (task == "regression") {
        vals <- df[[target_col]]
        vals <- vals[!is.na(vals)]
        skew <- mean((vals - mean(vals))^3) / (stats::sd(vals)^3)

        if (skew > 0.5) {
          alert_box(
            "Distribution asym\u00e9trique \u00e0 droite",
            tagList(
              "L\u2019asymm\u00e9trie vaut ", round(skew, 2), ". ",
              "Essayez la transformation ", strong("racine carr\u00e9e"), " ou ", strong("logarithme"),
              " ci-dessus pour visualiser l\u2019effet sur la distribution \u2014 ",
              "une distribution plus sym\u00e9trique aide g\u00e9n\u00e9ralement le mod\u00e8le \u00e0 mieux apprendre."
            ),
            couleur = "#F17D52"
          )
        } else {
          alert_box(
            "Distribution sym\u00e9trique",
            "La distribution est approximativement sym\u00e9trique. Aucune transformation n\u00e9cessaire.",
            couleur = "#00A896"
          )
        }
      } else {
        counts   <- table(df[[target_col]])
        props    <- counts / sum(counts)
        min_prop <- min(props)
        max_prop <- max(props)

        if (max_prop / min_prop > 2) {
          classe_min <- names(props)[which.min(props)]
          alert_box(
            "Classes d\u00e9s\u00e9quilibr\u00e9es",
            paste0(
              "La classe la moins repr\u00e9sent\u00e9e (\u00ab\u00a0", classe_min, "\u00a0\u00bb) ",
              "repr\u00e9sente seulement ", round(min_prop * 100), "% des observations. ",
              "Un mod\u00e8le entra\u00een\u00e9 sur des classes d\u00e9s\u00e9quilibr\u00e9es peut apprendre ",
              "\u00e0 tout pr\u00e9dire comme la classe majoritaire. Gardez cela en t\u00eate lors de l\u2019\u00e9valuation."
            ),
            couleur = "#F17D52"
          )
        }
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

    # Alerte valeurs manquantes
    output$nas_alert <- renderUI({
      req(selected_data(), vars_r$predictors())
      df        <- selected_data()[, vars_r$predictors(), drop = FALSE]
      na_counts <- sapply(df, function(x) sum(is.na(x)))
      vars_na   <- na_counts[na_counts > 0]

      if (length(vars_na) == 0) {
        alert_box(
          "Aucune valeur manquante",
          "Toutes les variables pr\u00e9dictives sont compl\u00e8tes. Aucune imputation ne sera n\u00e9cessaire.",
          couleur = "#00A896"
        )
      } else {
        detail <- paste(
          sapply(names(vars_na), function(v) paste0(v, " (", vars_na[[v]], " NA)")),
          collapse = ", "
        )
        alert_box(
          "Valeurs manquantes d\u00e9tect\u00e9es",
          paste0(
            "Les variables suivantes contiennent des valeurs manquantes : ", detail, ". ",
            "Au pr\u00e9traitement, vous pourrez les imputer automatiquement par la m\u00e9diane."
          ),
          couleur = "#F17D52"
        )
      }
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

    # Alerte correlations fortes
    output$corr_alert <- renderUI({
      req(selected_data(), vars_r$predictors())
      df       <- selected_data()[, vars_r$predictors(), drop = FALSE]
      num_vars <- df[, sapply(df, is.numeric), drop = FALSE]
      if (ncol(num_vars) < 2) return(NULL)

      corr_mat <- cor(num_vars, use = "pairwise.complete.obs")
      corr_mat[lower.tri(corr_mat, diag = TRUE)] <- NA
      forte <- which(abs(corr_mat) > 0.8, arr.ind = TRUE)

      if (nrow(forte) == 0) return(NULL)

      paires <- apply(forte, 1, function(idx) {
        v1 <- rownames(corr_mat)[idx[1]]
        v2 <- colnames(corr_mat)[idx[2]]
        r  <- round(corr_mat[idx[1], idx[2]], 2)
        paste0(v1, " / ", v2, " (r = ", r, ")")
      })

      alert_box(
        "Corr\u00e9lations \u00e9lev\u00e9es entre pr\u00e9dicteurs",
        paste0(
          "Les paires suivantes sont fortement corr\u00e9l\u00e9es : ",
          paste(paires, collapse = " ; "), ". ",
          "Des variables tr\u00e8s corr\u00e9l\u00e9es apportent une information redondante. ",
          "Vous pouvez envisager d\u2019en retirer certaines dans la s\u00e9lection des pr\u00e9dicteurs."
        ),
        couleur = "#F17D52"
      )
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

# Composant interne : boite d'alerte stylisee
alert_box <- function(titre, message, couleur = "#F17D52") {
  div(
    style = paste0(
      "margin-top: 0.75rem; padding: 0.6rem 1rem; border-radius: 6px; ",
      "border-left: 4px solid ", couleur, "; background-color: #F8FAFB;"
    ),
    p(style = paste0("margin: 0 0 0.25rem 0; font-weight: 600; color: ", couleur, ";"), titre),
    p(style = "margin: 0; font-size: 0.9rem;", message)
  )
}

## To be copied in the UI
# mod_exploration_ui("exploration_1")

## To be copied in the server
# mod_exploration_server("exploration_1")
