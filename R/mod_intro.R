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
      style = "text-align: center; padding: 2rem 0 1rem 0;",
      h1("MLstartr"),
      p(style = "font-size: 1.2rem; color: #6c757d;",
        "Initiez-vous au machine learning supervis\u00e9, pas \u00e0 pas.")
    ),

    # Accroche
    bslib::card(
      class = "card-pedagogique",
      bslib::card_body(
        h2(style = "margin-top: 0;", "C\u2019est quoi le machine learning ?"),
        p(style = "font-size: 1.1rem; margin-bottom: 0.5rem;",
          "Vous avez des donn\u00e9es, et une question \u00e0 laquelle vous voulez r\u00e9pondre. ",
          "Le machine learning supervis\u00e9, c\u2019est montrer des exemples \u00e0 un algorithme \u0028des ",
          "donn\u00e9es avec les bonnes r\u00e9ponses\u0029 pour qu\u2019il apprenne \u00e0 pr\u00e9dire ",
          "sur de nouveaux cas qu\u2019il n\u2019a jamais vus."),
        p(style = "font-size: 1.1rem; margin-bottom: 0;",
          "Ce n\u2019est pas de la magie. C\u2019est une suite d\u2019\u00e9tapes, et vous allez les faire toutes.")
      )
    ),

    # Pipeline
    br(),
    h3("Les \u00e9tapes du pipeline"),
    p(style = "color: #555;", "Cliquez sur une \u00e9tape pour en savoir plus."),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 6, md = 4, lg = 2),

      # 1. Donnees
      bslib::card(
        style = "text-align: center; background-color: #f8f9fa;",
        bslib::card_body(
          padding = "0.75rem",
          strong("1. Donn\u00e9es"),
          tags$details(
            style = "text-align: left; margin-top: 0.5rem;",
            tags$summary(style = "cursor: pointer; color: #00A896; font-size: 0.85rem;",
                         "En savoir plus"),
            p(style = "font-size: 0.85rem; margin-top: 0.5rem; color: #555;",
              "Choisissez un jeu de donn\u00e9es et d\u00e9finissez ce que vous voulez pr\u00e9dire. ",
              "C\u2019est le point de d\u00e9part de tout pipeline ML.")
          )
        )
      ),

      # 2. Exploration
      bslib::card(
        style = "text-align: center; background-color: #f8f9fa;",
        bslib::card_body(
          padding = "0.75rem",
          strong("2. Exploration"),
          tags$details(
            style = "text-align: left; margin-top: 0.5rem;",
            tags$summary(style = "cursor: pointer; color: #00A896; font-size: 0.85rem;",
                         "En savoir plus"),
            p(style = "font-size: 0.85rem; margin-top: 0.5rem; color: #555;",
              "Avant d\u2019entra\u00eener un mod\u00e8le, il faut comprendre ses donn\u00e9es. ",
              "Distribution des variables, valeurs manquantes, corr\u00e9lations \u003a ",
              "ces observations guident les \u00e9tapes suivantes.")
          )
        )
      ),

      # 3. Pretraitement
      bslib::card(
        style = "text-align: center; background-color: #f8f9fa;",
        bslib::card_body(
          padding = "0.75rem",
          strong("3. Pr\u00e9traitement"),
          tags$details(
            style = "text-align: left; margin-top: 0.5rem;",
            tags$summary(style = "cursor: pointer; color: #00A896; font-size: 0.85rem;",
                         "En savoir plus"),
            p(style = "font-size: 0.85rem; margin-top: 0.5rem; color: #555;",
              "Les donn\u00e9es brutes ne sont pas toujours pr\u00eates \u00e0 l\u2019emploi. ",
              "On s\u00e9pare un jeu d\u2019entra\u00eenement et un jeu de test, ",
              "puis on transforme les variables si n\u00e9cessaire.")
          )
        )
      ),

      # 4. Modelisation
      bslib::card(
        style = "text-align: center; background-color: #f8f9fa;",
        bslib::card_body(
          padding = "0.75rem",
          strong("4. Mod\u00e9lisation"),
          tags$details(
            style = "text-align: left; margin-top: 0.5rem;",
            tags$summary(style = "cursor: pointer; color: #00A896; font-size: 0.85rem;",
                         "En savoir plus"),
            p(style = "font-size: 0.85rem; margin-top: 0.5rem; color: #555;",
              "On entra\u00eene un Random Forest sur le jeu d\u2019entra\u00eenement. ",
              "L\u2019algorithme apprend les relations entre les variables et la cible.")
          )
        )
      ),

      # 5. Evaluation
      bslib::card(
        style = "text-align: center; background-color: #f8f9fa;",
        bslib::card_body(
          padding = "0.75rem",
          strong("5. \u00c9valuation"),
          tags$details(
            style = "text-align: left; margin-top: 0.5rem;",
            tags$summary(style = "cursor: pointer; color: #00A896; font-size: 0.85rem;",
                         "En savoir plus"),
            p(style = "font-size: 0.85rem; margin-top: 0.5rem; color: #555;",
              "On mesure les performances du mod\u00e8le sur le jeu de test \u003a ",
              "des donn\u00e9es qu\u2019il n\u2019a jamais vues. ",
              "C\u2019est l\u00e0 qu\u2019on sait si le mod\u00e8le g\u00e9n\u00e9ralise bien.")
          )
        )
      ),

      # 6. Export
      bslib::card(
        style = "text-align: center; background-color: #f8f9fa;",
        bslib::card_body(
          padding = "0.75rem",
          strong("6. Export"),
          tags$details(
            style = "text-align: left; margin-top: 0.5rem;",
            tags$summary(style = "cursor: pointer; color: #00A896; font-size: 0.85rem;",
                         "En savoir plus"),
            p(style = "font-size: 0.85rem; margin-top: 0.5rem; color: #555;",
              "Chaque choix que vous faites dans l\u2019application g\u00e9n\u00e8re du code R. ",
              "Vous repartez avec un script reproductible.")
          )
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
          p(style = "font-size: 1rem; margin-bottom: 0.25rem;",
            "Peut-on pr\u00e9dire le ", strong("prix m\u00e9dian des logements"),
            " dans un quartier californien ?"),
          p(class = "text-muted", style = "margin: 0;",
            "R\u00e9gression \u003a 20 640 observations")
        )
      ),
      bslib::card(
        class = "card-autre",
        bslib::card_header("Titanic"),
        bslib::card_body(
          p(style = "font-size: 1rem; margin-bottom: 0.25rem;",
            "Peut-on pr\u00e9dire si un passager a ",
            strong("surv\u00e9cu"), " selon son profil ?"),
          p(class = "text-muted", style = "margin: 0;",
            "Classification binaire \u003a 1 309 observations")
        )
      ),
      bslib::card(
        class = "card-autre",
        bslib::card_header("Penguins"),
        bslib::card_body(
          p(style = "font-size: 1rem; margin-bottom: 0.25rem;",
            "Peut-on identifier l\u2019",
            strong("esp\u00e8ce d\u2019un manchot"),
            "\u00e0 partir de ses mensurations ?"),
          p(class = "text-muted", style = "margin: 0;",
            "Classification multiclasse \u003a 344 observations")
        )
      )
    ),

    # Bouton
    br(),
    div(
      style = "text-align: center; padding-bottom: 2rem;",
      actionButton(
        ns("start"),
        "C\u2019est parti !",
        class = "btn-primary btn-lg"
      )
    ),

    # Footer
    br(),
    div(
      style = "text-align: center; padding: 1rem 0; color: #5A8AAD; font-size: 0.8rem;",
      p(style = "margin-bottom: 0.5rem;",
        "Cette application illustre les grandes \u00e9tapes d\u2019un pipeline de machine learning supervis\u00e9. ",
        "Elle est con\u00e7ue \u00e0 des fins p\u00e9dagogiques et ne couvre pas l\u2019ensemble des cas d\u2019usage r\u00e9els ",
        "(tuning d\u2019hyperparam\u00e8tres, validation crois\u00e9e, gestion avanc\u00e9e des donn\u00e9es...)."
      ),
      p(style = "margin-bottom: 0.5rem;",
        "Vous voulez aller plus loin ? ",
        tags$a(href = "https://pyranhia.eu/formations.html", target = "_blank",
               style = "color: #00A896; text-decoration: underline;",
               "D\u00e9couvrez les formations Pyranhia")
      ),
      p(style = "margin-top: 1rem; font-size: 0.8rem;",
        "Une application p\u00e9dagogique ",
        tags$a(href = "https://pyranhia.eu", target = "_blank",
               style = "color: #00A896;",
               "Pyranhia"),
        " \u2014 2025"
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
