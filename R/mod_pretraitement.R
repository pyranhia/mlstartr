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
      style = "background-color: #f0f7ff;",
      bslib::card_body(
        p(style = "font-size: 1rem; margin: 0;",
          "Le pr\u00e9traitement pr\u00e9pare vos donn\u00e9es avant l'entra\u00eenement. La ",
          strong("s\u00e9paration train/test"), " permet d'\u00e9valuer le mod\u00e8le sur des donn\u00e9es
          qu'il n'a jamais vues. Les ", strong("transformations"), " am\u00e9liorent la qualit\u00e9
          des pr\u00e9dictions : corriger une asym\u00e9trie sur la variable cible, imputer
          les valeurs manquantes, normaliser les pr\u00e9dicteurs.")
      )
    ),
    br(),
    bslib::layout_columns(
      col_widths = c(4, 4, 4),
      bslib::card(
        bslib::card_header("S\u00e9paration train/test"),
        radioButtons(
          ns("split_prop"),
          "Proportion entra\u00eenement / test :",
          choices = c("70/30" = 0.7, "75/25" = 0.75, "80/20" = 0.8),
          selected = 0.8
        ),
        uiOutput(ns("split_summary"))
      ),
      bslib::card(
        bslib::card_header("Variable cible"),
        uiOutput(ns("target_options"))
      ),
      bslib::card(
        bslib::card_header("Pr\u00e9dicteurs"),
        uiOutput(ns("predictors_options"))
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
        p(paste("Total :", n, "observations")),
        p(paste("Entra\u00eenement :", n_train, "observations")),
        p(paste("Test :", n_test, "observations"))
      )
    })

    # Options variable cible
    output$target_options <- renderUI({
      req(vars_r$task_type())
      if (vars_r$task_type() == "regression") {
        selectInput(
          ns("transformation"),
          "Transformation :",
          choices = c(
            "Aucune"           = "none",
            "Racine carr\u00e9e" = "sqrt",
            "Logarithme"       = "log"
          ),
          selected = "none"
        )
      } else {
        p(class = "text-muted",
          "Aucune transformation disponible pour la classification.")
      }
    })

    # Options predicteurs
    output$predictors_options <- renderUI({
      req(dataset_r(), vars_r$predictors())
      req(all(vars_r$predictors() %in% names(dataset_r())))
      df      <- dataset_r()[, vars_r$predictors(), drop = FALSE]
      has_nas <- any(is.na(df))
      has_cat <- any(sapply(df, function(x) is.character(x) | is.factor(x)))

      tagList(
        if (has_nas) {
          checkboxInput(
            ns("impute"),
            "Imputer les valeurs manquantes (m\u00e9diane)",
            value = TRUE
          )
        },
        checkboxInput(
          ns("normalize"),
          "Normaliser les variables num\u00e9riques",
          value = FALSE
        ),
        if (has_cat) {
          p(class = "text-muted",
            "\u2139\ufe0f Encodage des variables cat\u00e9gorielles appliqu\u00e9 automatiquement.")
        }
      )
    })

    # Split train/test (NAs de la cible retires)
    split_r <- reactive({
      req(dataset_r(), input$split_prop, vars_r$target())
      req(vars_r$target() %in% names(dataset_r()))
      df <- dataset_r()
      df <- df[!is.na(df[[vars_r$target()]]), ]
      rsample::initial_split(df, prop = as.numeric(input$split_prop))
    })

    train_r <- reactive(rsample::training(split_r()))
    test_r  <- reactive(rsample::testing(split_r()))

    # Construction de la recette
    recipe_r <- reactive({
      req(train_r(), vars_r$target(), vars_r$predictors())
      req(vars_r$target() %in% names(train_r()))
      req(all(vars_r$predictors() %in% names(train_r())))

      df      <- dataset_r()[, vars_r$predictors(), drop = FALSE]
      has_nas <- any(is.na(df))
      has_cat <- any(sapply(df, function(x) is.character(x) | is.factor(x)))

      f   <- as.formula(paste(vars_r$target(), "~ ."))
      rec <- recipes::recipe(f, data = train_r())

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

      transf <- if (isTruthy(input$transformation)) input$transformation else "none"
      df     <- dataset_r()[, vars_r$predictors(), drop = FALSE]
      has_nas <- any(is.na(df))
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
