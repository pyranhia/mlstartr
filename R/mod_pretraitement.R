#' pretraitement UI Function
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @importFrom shiny NS tagList
mod_pretraitement_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::card(
      class = "card-pedagogique",
      bslib::card_body(
        p(style = "font-size: 1rem; margin: 0;",
          "L\u2019exploration vous a permis de mieux conna\u00eetre vos donn\u00e9es. ",
          "Il faut maintenant les ", strong("pr\u00e9parer"), " avant d\u2019entra\u00eener le mod\u00e8le. ",
          "Cette \u00e9tape se fait en deux temps : s\u00e9parer les donn\u00e9es en un ",
          strong("jeu d\u2019entra\u00eenement"), " et un ", strong("jeu de test"),
          ", puis appliquer les ", strong("transformations"), " n\u00e9cessaires.")
      )
    ),
    br(),
    bslib::layout_columns(
      col_widths = c(4, 4, 4),
      bslib::card(
        bslib::card_header("S\u00e9paration train/test"),
        bslib::card_body(
          p(style = "font-size: 0.875rem; color: #555; margin-bottom: 0.75rem;",
            "Le mod\u00e8le apprend sur le jeu d\u2019entra\u00eenement et est \u00e9valu\u00e9 sur le jeu de test, ",
            "qu\u2019il n\u2019aura ", em("jamais vu"), " pendant l\u2019apprentissage. ",
            "Cela garantit une \u00e9valuation honn\u00eate de ses performances r\u00e9elles."
          ),
          radioButtons(
            ns("split_prop"),
            "Proportion entra\u00eenement / test :",
            choices  = c("70/30" = 0.7, "75/25" = 0.75, "80/20" = 0.8),
            selected = 0.8
          ),
          uiOutput(ns("split_summary"))
        )
      ),
      bslib::card(
        bslib::card_header("Transformations de la variable cible"),
        bslib::card_body(
          uiOutput(ns("target_options"))
        )
      ),
      bslib::card(
        bslib::card_header("Transformations des pr\u00e9dicteurs"),
        bslib::card_body(
          uiOutput(ns("predictors_options"))
        )
      )
    ),
    hr(),
    div(
      style = "text-align: center; margin-top: 1rem;",
      actionButton(ns("validate"), "Valider et passer \u00e0 la mod\u00e9lisation",
                   class = "btn-primary btn-lg")
    )
  )
}

#' pretraitement Server Functions
#'
#' @noRd
#' @importFrom recipes recipe step_impute_median step_normalize step_dummy
#'   step_mutate step_unknown all_numeric_predictors
#'   all_nominal_predictors all_outcomes
#' @importFrom rsample initial_split training testing
#' @importFrom stats as.formula
#' @importFrom rlang sym :=
mod_pretraitement_server <- function(id, dataset_r, vars_r, code_log) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Caracteristiques des predicteurs (calculees une fois)
    pred_info <- reactive({
      req(dataset_r(), vars_r$predictors())
      df      <- dataset_r()[, vars_r$predictors(), drop = FALSE]
      has_nas <- any(is.na(df[, sapply(df, is.numeric), drop = FALSE]))
      has_cat <- any(sapply(df, function(x) is.character(x) | is.factor(x)))
      list(has_nas = has_nas, has_cat = has_cat)
    })

    # Resume du split
    output$split_summary <- renderUI({
      req(dataset_r(), input$split_prop, vars_r$target())
      req(vars_r$target() %in% names(dataset_r()))
      df      <- dataset_r()
      df      <- df[!is.na(df[[vars_r$target()]]), ]
      n       <- nrow(df)
      prop    <- as.numeric(input$split_prop)
      n_train <- round(n * prop)
      n_test  <- n - n_train
      tagList(
        hr(),
        p(style = "font-size: 0.875rem; margin: 0;",
          paste("Total :", n, "observations")),
        p(style = "font-size: 0.875rem; margin: 0;",
          paste("Entra\u00eenement :", n_train, "| Test :", n_test))
      )
    })

    # Options variable cible (conseil fixe base sur la skewness brute)
    output$target_options <- renderUI({
      req(vars_r$task_type())

      if (vars_r$task_type() == "regression") {
        target_col <- vars_r$target()
        vals       <- dataset_r()[[target_col]]
        vals       <- vals[!is.na(vals)]
        skew       <- mean((vals - mean(vals))^3) / (stats::sd(vals)^3)

        conseil <- if (skew > 0.5) {
          p(style = "font-size: 0.875rem; color: #F17D52; margin-bottom: 0.75rem;",
            paste0("La distribution est asym\u00e9trique \u00e0 droite (asymm\u00e9trie = ", round(skew, 2), "). ",
                   "Une transformation racine carr\u00e9e ou logarithme est recommand\u00e9e.")
          )
        } else {
          p(style = "font-size: 0.875rem; color: #00A896; margin-bottom: 0.75rem;",
            "La distribution est sym\u00e9trique. Aucune transformation n\u00e9cessaire."
          )
        }

        tagList(
          conseil,
          selectInput(
            ns("transformation"),
            "Transformation :",
            choices  = c("Aucune" = "none", "Racine carr\u00e9e" = "sqrt", "Logarithme" = "log"),
            selected = "none"
          )
        )
      } else {
        p(style = "font-size: 0.875rem; color: #555;",
          "Aucune transformation de la variable cible n\u2019est appliqu\u00e9e en classification.")
      }
    })
    # Options predicteurs
    output$predictors_options <- renderUI({
      req(pred_info())
      info <- pred_info()

      tagList(
        # Imputation
        if (info$has_nas) {
          tagList(
            checkboxInput(ns("impute"), "Imputer les valeurs manquantes", value = TRUE),
            p(style = "font-size: 0.875rem; color: #555; margin-top: -0.5rem; margin-bottom: 0.75rem;",
              "Des valeurs manquantes ont \u00e9t\u00e9 d\u00e9tect\u00e9es dans vos pr\u00e9dicteurs num\u00e9riques. ",
              "Elles seront remplac\u00e9es par la ", strong("m\u00e9diane"), " de chaque variable.")
          )
        } else {
          p(style = "font-size: 0.875rem; color: #00A896; margin-bottom: 0.75rem;",
            "Aucune valeur manquante dans les pr\u00e9dicteurs num\u00e9riques.")
        },

        # Normalisation
        checkboxInput(ns("normalize"), "Normaliser les pr\u00e9dicteurs num\u00e9riques", value = FALSE),
        p(style = "font-size: 0.875rem; color: #555; margin-top: -0.5rem; margin-bottom: 0.75rem;",
          "La normalisation centre chaque variable \u00e0 0 et la r\u00e9duit \u00e0 un \u00e9cart-type de 1. ",
          "Le Random Forest n\u2019en a pas besoin, mais c\u2019est une bonne habitude \u00e0 conna\u00eetre."),

        # Encodage categoriel (automatique, juste informe)
        if (info$has_cat) {
          div(
            style = "margin-top: 0.5rem; padding: 0.5rem 0.75rem; background-color: #EEF6FB; border-radius: 6px;",
            p(style = "font-size: 0.875rem; margin: 0;",
              "Vos pr\u00e9dicteurs contiennent des ", strong("variables cat\u00e9gorielles"),
              ". Elles seront automatiquement encod\u00e9es en variables num\u00e9riques ",
              "(encodage ", em("dummy"), ") pour que le mod\u00e8le puisse les utiliser.")
          )
        }
      )
    })

    # Donnees splitees
    split_r <- reactive({
      req(dataset_r(), input$split_prop, vars_r$target())
      df   <- dataset_r()
      df   <- df[!is.na(df[[vars_r$target()]]), ]
      prop <- as.numeric(input$split_prop)
      rsample::initial_split(df, prop = prop)
    })

    train_r <- reactive({
      req(split_r())
      rsample::training(split_r())
    })

    test_r <- reactive({
      req(split_r())
      rsample::testing(split_r())
    })

    # Recette
    recipe_r <- reactive({
      req(train_r(), vars_r$target(), vars_r$predictors(), vars_r$task_type())
      df      <- dataset_r()[, vars_r$predictors(), drop = FALSE]
      has_nas <- any(is.na(df[, sapply(df, is.numeric), drop = FALSE]))
      has_cat <- any(sapply(df, function(x) is.character(x) | is.factor(x)))

      preds <- vars_r$predictors()
      f     <- stats::as.formula(paste(vars_r$target(), "~", paste(preds, collapse = " + ")))
      rec   <- recipes::recipe(f, data = train_r())

      # Transformation cible
      transf <- if (vars_r$task_type() == "regression" && isTruthy(input$transformation)) {
        input$transformation
      } else "none"

      if (transf == "log") {
        rec <- rec |> recipes::step_mutate(
          !!vars_r$target() := log(!!rlang::sym(vars_r$target())),
          skip = TRUE
        )
      } else if (transf == "sqrt") {
        rec <- rec |> recipes::step_mutate(
          !!vars_r$target() := sqrt(!!rlang::sym(vars_r$target())),
          skip = TRUE
        )
      }

      # Imputation
      if (has_nas && isTruthy(input$impute)) {
        rec <- rec |> recipes::step_impute_median(recipes::all_numeric_predictors())
      }

      # Normalisation
      if (isTruthy(input$normalize)) {
        rec <- rec |> recipes::step_normalize(recipes::all_numeric_predictors())
      }

      # Encodage categoriel
      if (has_cat) {
        rec <- rec |>
          recipes::step_unknown(recipes::all_nominal_predictors()) |>
          recipes::step_dummy(recipes::all_nominal_predictors())
      }

      rec
    })

    # Validation
    validated <- reactiveVal(FALSE)

    observeEvent(input$validate, {
      validated(TRUE)

      transf  <- if (isTruthy(input$transformation)) input$transformation else "none"
      df      <- dataset_r()[, vars_r$predictors(), drop = FALSE]
      has_nas <- any(is.na(df[, sapply(df, is.numeric), drop = FALSE]))
      has_cat <- any(sapply(df, function(x) is.character(x) | is.factor(x)))

      steps <- ""
      if (transf == "log")  steps <- paste0(steps, " |>\n  step_mutate(", vars_r$target(), " = log(", vars_r$target(), "), skip = TRUE)")
      if (transf == "sqrt") steps <- paste0(steps, " |>\n  step_mutate(", vars_r$target(), " = sqrt(", vars_r$target(), "), skip = TRUE)")
      if (has_nas && isTruthy(input$impute)) steps <- paste0(steps, " |>\n  step_impute_median(all_numeric_predictors())")
      if (isTruthy(input$normalize)) steps <- paste0(steps, " |>\n  step_normalize(all_numeric_predictors())")
      if (has_cat) steps <- paste0(steps, " |>\n  step_unknown(all_nominal_predictors()) |>\n  step_dummy(all_nominal_predictors())")

      bloc <- paste0(
        "# Split train/test\n",
        "split <- initial_split(data, prop = ", input$split_prop, ")\n",
        "train <- training(split)\n",
        "test  <- testing(split)\n\n",
        "# Recette\n",
        "rec <- recipe(", vars_r$target(), " ~ ., data = train)",
        steps, "\n"
      )
      message(bloc)
      current <- code_log()
      current$pretraitement <- bloc
      code_log(current)
    })

    observeEvent(list(dataset_r(), vars_r$target(), vars_r$predictors()), {
      validated(FALSE)
    })

    observeEvent(list(input$split_prop, input$transformation, input$normalize, input$impute), {
      validated(FALSE)
    }, ignoreInit = TRUE)

    return(list(
      train     = train_r,
      test      = test_r,
      recipe    = recipe_r,
      validated = validated
    ))
  })
}

## To be copied in the UI
# mod_pretraitement_ui("pretraitement_1")

## To be copied in the server
# mod_pretraitement_server("pretraitement_1")#' pretraitement UI Function
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @importFrom shiny NS tagList
mod_pretraitement_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::card(
      class = "card-pedagogique",
      bslib::card_body(
        p(style = "font-size: 1rem; margin: 0;",
          "L\u2019exploration vous a permis de mieux conna\u00eetre vos donn\u00e9es. ",
          "Il faut maintenant les ", strong("pr\u00e9parer"), " avant d\u2019entra\u00eener le mod\u00e8le. ",
          "Cette \u00e9tape se fait en deux temps : s\u00e9parer les donn\u00e9es en un ",
          strong("jeu d\u2019entra\u00eenement"), " et un ", strong("jeu de test"),
          ", puis appliquer les ", strong("transformations"), " n\u00e9cessaires.")
      )
    ),
    br(),
    bslib::layout_columns(
      col_widths = c(4, 4, 4),
      bslib::card(
        bslib::card_header("S\u00e9paration train/test"),
        bslib::card_body(
          p(style = "font-size: 0.875rem; color: #555; margin-bottom: 0.75rem;",
            "Le mod\u00e8le apprend sur le jeu d\u2019entra\u00eenement et est \u00e9valu\u00e9 sur le jeu de test, ",
            "qu\u2019il n\u2019aura ", em("jamais vu"), " pendant l\u2019apprentissage. ",
            "Cela garantit une \u00e9valuation honn\u00eate de ses performances r\u00e9elles."
          ),
          radioButtons(
            ns("split_prop"),
            "Proportion entra\u00eenement / test :",
            choices  = c("70/30" = 0.7, "75/25" = 0.75, "80/20" = 0.8),
            selected = 0.8
          ),
          uiOutput(ns("split_summary"))
        )
      ),
      bslib::card(
        bslib::card_header("Transformations de la variable cible"),
        bslib::card_body(
          uiOutput(ns("target_options"))
        )
      ),
      bslib::card(
        bslib::card_header("Transformations des pr\u00e9dicteurs"),
        bslib::card_body(
          uiOutput(ns("predictors_options"))
        )
      )
    ),
    hr(),
    div(
      style = "text-align: center; margin-top: 1rem;",
      actionButton(ns("validate"), "Valider et passer \u00e0 la mod\u00e9lisation",
                   class = "btn-primary btn-lg")
    )
  )
}

#' pretraitement Server Functions
#'
#' @noRd
#' @importFrom recipes recipe step_impute_median step_normalize step_dummy
#'   step_mutate step_unknown all_numeric_predictors
#'   all_nominal_predictors all_outcomes
#' @importFrom rsample initial_split training testing
#' @importFrom stats as.formula
#' @importFrom rlang sym :=
mod_pretraitement_server <- function(id, dataset_r, vars_r, code_log) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Caracteristiques des predicteurs (calculees une fois)
    pred_info <- reactive({
      req(dataset_r(), vars_r$predictors())
      df      <- dataset_r()[, vars_r$predictors(), drop = FALSE]
      has_nas <- any(is.na(df[, sapply(df, is.numeric), drop = FALSE]))
      has_cat <- any(sapply(df, function(x) is.character(x) | is.factor(x)))
      list(has_nas = has_nas, has_cat = has_cat)
    })

    # Resume du split
    output$split_summary <- renderUI({
      req(dataset_r(), input$split_prop, vars_r$target())
      req(vars_r$target() %in% names(dataset_r()))
      df      <- dataset_r()
      df      <- df[!is.na(df[[vars_r$target()]]), ]
      n       <- nrow(df)
      prop    <- as.numeric(input$split_prop)
      n_train <- round(n * prop)
      n_test  <- n - n_train
      tagList(
        hr(),
        p(style = "font-size: 0.875rem; margin: 0;",
          paste("Total :", n, "observations")),
        p(style = "font-size: 0.875rem; margin: 0;",
          paste("Entra\u00eenement :", n_train, "| Test :", n_test))
      )
    })

    # Options variable cible (conseil fixe base sur la skewness brute)
    output$target_options <- renderUI({
      req(vars_r$task_type())

      if (vars_r$task_type() == "regression") {
        target_col <- vars_r$target()
        vals       <- dataset_r()[[target_col]]
        vals       <- vals[!is.na(vals)]
        skew       <- mean((vals - mean(vals))^3) / (stats::sd(vals)^3)

        conseil <- if (skew > 0.5) {
          p(style = "font-size: 0.875rem; color: #F17D52; margin-bottom: 0.75rem;",
            paste0("La distribution est asym\u00e9trique \u00e0 droite (asymm\u00e9trie = ", round(skew, 2), "). ",
                   "Une transformation racine carr\u00e9e ou logarithme est recommand\u00e9e.")
          )
        } else {
          p(style = "font-size: 0.875rem; color: #00A896; margin-bottom: 0.75rem;",
            "La distribution est sym\u00e9trique. Aucune transformation n\u00e9cessaire."
          )
        }

        tagList(
          conseil,
          selectInput(
            ns("transformation"),
            "Transformation :",
            choices  = c("Aucune" = "none", "Racine carr\u00e9e" = "sqrt", "Logarithme" = "log"),
            selected = "none"
          )
        )
      } else {
        p(style = "font-size: 0.875rem; color: #555;",
          "Aucune transformation de la variable cible n\u2019est appliqu\u00e9e en classification.")
      }
    })
    # Options predicteurs
    output$predictors_options <- renderUI({
      req(pred_info())
      info <- pred_info()

      tagList(
        # Imputation
        if (info$has_nas) {
          tagList(
            checkboxInput(ns("impute"), "Imputer les valeurs manquantes", value = TRUE),
            p(style = "font-size: 0.875rem; color: #555; margin-top: -0.5rem; margin-bottom: 0.75rem;",
              "Des valeurs manquantes ont \u00e9t\u00e9 d\u00e9tect\u00e9es dans vos pr\u00e9dicteurs num\u00e9riques. ",
              "Elles seront remplac\u00e9es par la ", strong("m\u00e9diane"), " de chaque variable.")
          )
        } else {
          p(style = "font-size: 0.875rem; color: #00A896; margin-bottom: 0.75rem;",
            "Aucune valeur manquante dans les pr\u00e9dicteurs num\u00e9riques.")
        },

        # Normalisation
        checkboxInput(ns("normalize"), "Normaliser les pr\u00e9dicteurs num\u00e9riques", value = FALSE),
        p(style = "font-size: 0.875rem; color: #555; margin-top: -0.5rem; margin-bottom: 0.75rem;",
          "La normalisation centre chaque variable \u00e0 0 et la r\u00e9duit \u00e0 un \u00e9cart-type de 1. ",
          "Le Random Forest n\u2019en a pas besoin, mais c\u2019est une bonne habitude \u00e0 conna\u00eetre."),

        # Encodage categoriel (automatique, juste informe)
        if (info$has_cat) {
          div(
            style = "margin-top: 0.5rem; padding: 0.5rem 0.75rem; background-color: #EEF6FB; border-radius: 6px;",
            p(style = "font-size: 0.875rem; margin: 0;",
              "Vos pr\u00e9dicteurs contiennent des ", strong("variables cat\u00e9gorielles"),
              ". Elles seront automatiquement encod\u00e9es en variables num\u00e9riques ",
              "(encodage ", em("dummy"), ") pour que le mod\u00e8le puisse les utiliser.")
          )
        }
      )
    })

    # Donnees splitees
    split_r <- reactive({
      req(dataset_r(), input$split_prop, vars_r$target())
      df   <- dataset_r()
      df   <- df[!is.na(df[[vars_r$target()]]), ]
      prop <- as.numeric(input$split_prop)
      rsample::initial_split(df, prop = prop)
    })

    train_r <- reactive({
      req(split_r())
      rsample::training(split_r())
    })

    test_r <- reactive({
      req(split_r())
      rsample::testing(split_r())
    })

    # Recette
    recipe_r <- reactive({
      req(train_r(), vars_r$target(), vars_r$predictors(), vars_r$task_type())
      req(all(vars_r$predictors() %in% names(dataset_r())))
      df      <- dataset_r()[, vars_r$predictors(), drop = FALSE]
      has_nas <- any(is.na(df[, sapply(df, is.numeric), drop = FALSE]))
      has_cat <- any(sapply(df, function(x) is.character(x) | is.factor(x)))

      preds <- vars_r$predictors()
      f     <- stats::as.formula(paste(vars_r$target(), "~", paste(preds, collapse = " + ")))
      rec   <- recipes::recipe(f, data = train_r())

      # Transformation cible
      transf <- if (vars_r$task_type() == "regression" && isTruthy(input$transformation)) {
        input$transformation
      } else "none"

      if (transf == "log") {
        rec <- rec |> recipes::step_mutate(
          !!vars_r$target() := log(!!rlang::sym(vars_r$target())),
          skip = TRUE
        )
      } else if (transf == "sqrt") {
        rec <- rec |> recipes::step_mutate(
          !!vars_r$target() := sqrt(!!rlang::sym(vars_r$target())),
          skip = TRUE
        )
      }

      # Imputation
      if (has_nas && isTruthy(input$impute)) {
        rec <- rec |> recipes::step_impute_median(recipes::all_numeric_predictors())
      }

      # Normalisation
      if (isTruthy(input$normalize)) {
        rec <- rec |> recipes::step_normalize(recipes::all_numeric_predictors())
      }

      # Encodage categoriel
      if (has_cat) {
        rec <- rec |>
          recipes::step_unknown(recipes::all_nominal_predictors()) |>
          recipes::step_dummy(recipes::all_nominal_predictors())
      }

      rec
    })

    # Validation
    validated <- reactiveVal(FALSE)

    observeEvent(input$validate, {
      validated(TRUE)

      transf  <- if (isTruthy(input$transformation)) input$transformation else "none"
      df      <- dataset_r()[, vars_r$predictors(), drop = FALSE]
      has_nas <- any(is.na(df[, sapply(df, is.numeric), drop = FALSE]))
      has_cat <- any(sapply(df, function(x) is.character(x) | is.factor(x)))

      steps <- ""
      if (transf == "log")  steps <- paste0(steps, " |>\n  step_mutate(", vars_r$target(), " = log(", vars_r$target(), "), skip = TRUE)")
      if (transf == "sqrt") steps <- paste0(steps, " |>\n  step_mutate(", vars_r$target(), " = sqrt(", vars_r$target(), "), skip = TRUE)")
      if (has_nas && isTruthy(input$impute)) steps <- paste0(steps, " |>\n  step_impute_median(all_numeric_predictors())")
      if (isTruthy(input$normalize)) steps <- paste0(steps, " |>\n  step_normalize(all_numeric_predictors())")
      if (has_cat) steps <- paste0(steps, " |>\n  step_unknown(all_nominal_predictors()) |>\n  step_dummy(all_nominal_predictors())")

      bloc <- paste0(
        "# Split train/test\n",
        "split <- initial_split(data, prop = ", input$split_prop, ")\n",
        "train <- training(split)\n",
        "test  <- testing(split)\n\n",
        "# Recette\n",
        "rec <- recipe(", vars_r$target(), " ~ ., data = train)",
        steps, "\n"
      )
      message(bloc)
      current <- code_log()
      current$pretraitement <- bloc
      code_log(current)
    })

    observeEvent(list(dataset_r(), vars_r$target(), vars_r$predictors()), {
      validated(FALSE)
    })

    observeEvent(list(input$split_prop, input$transformation, input$normalize, input$impute), {
      validated(FALSE)
    }, ignoreInit = TRUE)

    return(list(
      train     = train_r,
      test      = test_r,
      recipe    = recipe_r,
      validated = validated
    ))
  })
}

## To be copied in the UI
# mod_pretraitement_ui("pretraitement_1")

## To be copied in the server
# mod_pretraitement_server("pretraitement_1")
