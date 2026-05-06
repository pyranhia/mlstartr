#' intro UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @importFrom shiny NS tagList
mod_intro_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Titre
    div(
      style = "text-align: center; padding: 2rem 0;",
      h1("MLstartr"),
      p(style = "font-size: 1.2rem; color: #6c757d;",
        "Une application pour vous initier au machine learning supervis\u00e9, pas \u00e0 pas.")
    ),

    # Accroche
    bslib::card(
      class = "card-pedagogique",
      bslib::card_body(
        h2("C'est quoi le machine learning ?"),
        p(style = "font-size: 1.1rem;",
          "Le machine learning supervis\u00e9, c'est montrer des exemples \u00e0 un algorithme
          (des donn\u00e9es avec les bonnes r\u00e9ponses), le laisser apprendre des motifs,
          puis lui demander de pr\u00e9dire sur de nouvelles donn\u00e9es qu'il n'a jamais vues.")
      )
    ),

    # Pipeline
    br(),
    h3("Les \u00e9tapes du pipeline"),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 6, md = 4, lg = 2),
      bslib::card(
        style = "text-align: center; background-color: #f8f9fa;",
        bslib::card_body(
          padding = "0.5rem",
          strong("1. Donn\u00e9es"),
          p(class = "text-muted",
            "Choisir un jeu de donn\u00e9es et les variables")
        )
      ),
      bslib::card(
        style = "text-align: center; background-color: #f8f9fa;",
        bslib::card_body(
          padding = "0.5rem",
          strong("2. Exploration"),
          p(class = "text-muted",
            "Visualiser et comprendre les donn\u00e9es")
        )
      ),
      bslib::card(
        style = "text-align: center; background-color: #f8f9fa;",
        bslib::card_body(
          padding = "0.5rem",
          strong("3. Pr\u00e9traitement"),
          p(class = "text-muted",
            "Pr\u00e9parer les donn\u00e9es pour le mod\u00e8le")
        )
      ),
      bslib::card(
        style = "text-align: center; background-color: #f8f9fa;",
        bslib::card_body(
          padding = "0.5rem",
          strong("4. Mod\u00e9lisation"),
          p(class = "text-muted",
            "Entra\u00eener un Random Forest")
        )
      ),
      bslib::card(
        style = "text-align: center; background-color: #f8f9fa;",
        bslib::card_body(
          padding = "0.5rem",
          strong("5. \u00c9valuation"),
          p(class = "text-muted",
            "\u00c9valuer les performances du mod\u00e8le")
        )
      ),
      bslib::card(
        style = "text-align: center; background-color: #f8f9fa;",
        bslib::card_body(
          padding = "0.5rem",
          strong("6. Export"),
          p(class = "text-muted",
            "R\u00e9cup\u00e9rer le code R g\u00e9n\u00e9r\u00e9")
        )
      )
    ),

    # Datasets
    br(),
    h3("Les jeux de donn\u00e9es disponibles"),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, md = 4),
      bslib::card(
        class = "card-autre",
        bslib::card_header("California Housing"),
        bslib::card_body(
          p(style = "font-size: 1rem;",
            "Pr\u00e9dire le", strong("prix m\u00e9dian des logements"),
            "dans diff\u00e9rents quartiers de Californie."),
          p(class = "text-muted",
            "T\u00e2che : R\u00e9gression \u2014 20 640 observations")
        )
      ),
      bslib::card(
        class = "card-autre",
        bslib::card_header("Titanic"),
        bslib::card_body(
          p(style = "font-size: 1rem;",
            "Pr\u00e9dire si un passager du Titanic a", strong("surv\u00e9cu ou non"),
            "selon son profil."),
          p(class = "text-muted",
            "T\u00e2che : Classification binaire \u2014 1 309 observations")
        )
      ),
      bslib::card(
        class = "card-autre",
        bslib::card_header("Penguins"),
        bslib::card_body(
          p(style = "font-size: 1rem;",
            "Identifier l'", strong("esp\u00e8ce de pingouin"),
            "en fonction de ses caract\u00e9ristiques morphologiques."),
          p(class = "text-muted",
            "T\u00e2che : Classification multiclasse \u2014 344 observations")
        )
      )
    ),

    # Bouton
    br(),
    div(
      style = "text-align: center; padding-bottom: 2rem;",
      actionButton(
        ns("start"),
        "C'est parti !",
        class = "btn-primary btn-lg"
      )
    )
  )
}

#' intro Server Functions
#'
#' @noRd
mod_intro_server <- function(id, session_root) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$start, {
      bslib::nav_show(id = "tabs", target = "data", session = session_root)
      bslib::nav_select(id = "tabs", selected = "data", session = session_root)
    })
  })
}

## To be copied in the UI
# mod_intro_ui("intro_1")

## To be copied in the server
# mod_intro_server("intro_1")
