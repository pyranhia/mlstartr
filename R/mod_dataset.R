#' dataset UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @importFrom shiny NS tagList
mod_dataset_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(
      ns("dataset"),
      "Jeu de donn\u00e9es :",
      choices = c(
        "California Housing (r\u00e9gression)"   = "housing",
        "Titanic (classification binaire)"      = "titanic",
        "Penguins (classification multiclasse)" = "penguins"
      )
    ),
    uiOutput(ns("dataset_context")),
    br(),
    DT::DTOutput(ns("dataset_table"))
  )
}

#' dataset Server Functions
#'
#' @noRd
#' @importFrom dplyr mutate
#' @importFrom utils data
utils::globalVariables("survived")
mod_dataset_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    housing_vars  <- c("median_house_value", "median_income", "housing_median_age",
                       "total_rooms", "total_bedrooms", "population",
                       "households", "ocean_proximity")
    titanic_vars  <- c("survived", "pclass", "sex", "age", "fare", "sibsp", "parch")
    penguins_vars <- c("species", "bill_length_mm", "bill_depth_mm",
                       "flipper_length_mm", "body_mass_g", "sex")

    # Metadonnees contextuelles par dataset
    dataset_meta <- list(
      housing = list(
        titre    = "California Housing",
        question = "Peut-on pr\u00e9dire le prix m\u00e9dian d\u2019un logement californien \u00e0 partir de ses caract\u00e9ristiques ?",
        contexte = "Ces donn\u00e9es proviennent du recensement am\u00e9ricain de 1990. Chaque ligne d\u00e9crit un p\u00e2t\u00e9 de maisons (bloc) en Californie : sa localisation, les caract\u00e9ristiques des logements et de leurs habitants.",
        tache    = "r\u00e9gression",
        tache_explication = "Le prix m\u00e9dian est une valeur num\u00e9rique continue (ex. 250 000 $). Pr\u00e9dire un nombre, c\u2019est de la r\u00e9gression.",
        badge_color = "#6BAED6",
        variables = list(
          median_house_value = "prix m\u00e9dian des logements du bloc (en dollars) (ce qu\u2019on cherche \u00e0 pr\u00e9dire)",
          median_income      = "revenu m\u00e9dian des habitants (en dizaines de milliers de dollars)",
          housing_median_age = "\u00e2ge m\u00e9dian des logements (en ann\u00e9es)",
          total_rooms        = "nombre total de pi\u00e8ces dans le bloc",
          total_bedrooms     = "nombre total de chambres dans le bloc",
          population         = "nombre d\u2019habitants dans le bloc",
          households         = "nombre de m\u00e9nages dans le bloc",
          ocean_proximity    = "proximit\u00e9 de l\u2019oc\u00e9an (cat\u00e9gorie : INLAND, NEAR BAY...)"
        )
      ),
      titanic = list(
        titre    = "Titanic",
        question = "Peut-on pr\u00e9dire si un passager du Titanic a surv\u00e9cu, en fonction de son profil ?",
        contexte = "Le Titanic a coul\u00e9 en 1912. Sur 2 224 passagers, 1 502 ont p\u00e9ri. Ces donn\u00e9es d\u00e9crivent les passagers : leur classe, leur \u00e2ge, leur sexe... et s\u2019ils ont surv\u00e9cu.",
        tache    = "classification binaire",
        tache_explication = "La survie est une cat\u00e9gorie : oui ou non. Pr\u00e9dire une cat\u00e9gorie parmi deux, c\u2019est de la classification binaire.",
        badge_color = "#F17D52",
        variables = list(
          survived = "a surv\u00e9cu ? (oui / non) (ce qu\u2019on cherche \u00e0 pr\u00e9dire)",
          pclass   = "classe du billet (1 = 1\u00e8re classe, 2 = 2\u00e8me, 3 = 3\u00e8me)",
          sex      = "sexe du passager",
          age      = "\u00e2ge du passager (en ann\u00e9es)",
          fare     = "prix du billet (en livres sterling)",
          sibsp    = "nombre de fr\u00e8res/s\u0153urs ou conjoint(e)s \u00e0 bord",
          parch    = "nombre de parents ou enfants \u00e0 bord"
        )
      ),
      penguins = list(
        titre    = "Penguins",
        question = "Peut-on identifier l\u2019esp\u00e8ce d\u2019un manchot \u00e0 partir de ses mensurations ?",
        contexte = "Ces donn\u00e9es ont \u00e9t\u00e9 collect\u00e9es entre 2007 et 2009 sur des manchots de l\u2019archipel Palmer en Antarctique. Trois esp\u00e8ces sont repr\u00e9sent\u00e9es : Ad\u00e9lie, Chinstrap et Gentoo.",
        tache    = "classification multiclasse",
        tache_explication = "L\u2019esp\u00e8ce est une cat\u00e9gorie parmi trois possibles. Pr\u00e9dire une cat\u00e9gorie parmi plusieurs, c\u2019est de la classification multiclasse.",
        badge_color = "#00A896",
        variables = list(
          species           = "esp\u00e8ce du manchot (Ad\u00e9lie, Chinstrap, Gentoo) (ce qu\u2019on cherche \u00e0 pr\u00e9dire)",
          bill_length_mm    = "longueur du bec (en mm)",
          bill_depth_mm     = "profondeur du bec (en mm)",
          flipper_length_mm = "longueur des ailes (en mm)",
          body_mass_g       = "masse corporelle (en grammes)",
          sex               = "sexe du manchot"
        )
      )
    )

    # Card contextuelle reactive
    output$dataset_context <- renderUI({
      req(input$dataset)
      meta <- dataset_meta[[input$dataset]]

      bslib::card(
        class = "card-autre",
        bslib::card_body(
          # Titre + badge type de tache
          div(
            style = "display: flex; align-items: center; gap: 1rem; margin-bottom: 0.75rem;",
            h4(style = "margin: 0;", meta$titre),
            tags$span(
              style = paste0(
                "background-color: ", meta$badge_color, "; color: white; ",
                "padding: 2px 10px; border-radius: 12px; font-size: 0.8rem; font-weight: 600;"
              ),
              meta$tache
            )
          ),
          # Question ML
          div(
            style = "background-color: #EEF6FB; border-radius: 6px; padding: 0.6rem 1rem; margin-bottom: 0.75rem;",
            tags$strong("Question : "),
            tags$em(meta$question)
          ),
          # Contexte narratif
          p(style = "color: #555; margin-bottom: 0.75rem;", meta$contexte),
          # Explication type de tache
          p(tags$strong("Type de t\u00e2che : "), meta$tache_explication),
          # Glossaire des variables (depliable)
          tags$details(
            tags$summary(
              style = "cursor: pointer; color: #00A896; font-weight: 600;",
              "D\u00e9tail des variables"
            ),
            tags$ul(
              style = "margin-top: 0.5rem; padding-left: 1.2rem;",
              lapply(names(meta$variables), function(v) {
                tags$li(tags$code(v), " \u003a ", meta$variables[[v]])
              })
            )
          )
        )
      )
    })

    dataset_r <- reactive({
      switch(input$dataset,
             "housing" = {
               e <- new.env()
               data("housing", package = "datapyranhia", envir = e)
               e$housing[, housing_vars]
             },
             "titanic" = {
               e <- new.env()
               data("titanic", package = "datapyranhia", envir = e)
               e$titanic[, titanic_vars] |>
                 dplyr::mutate(survived = factor(survived, levels = c(0, 1),
                                                 labels = c("non", "oui")))
             },
             "penguins" = {
               palmerpenguins::penguins[, penguins_vars]
             }
      )
    })

    output$dataset_table <- DT::renderDT({
      req(dataset_r())
      DT::datatable(
        dataset_r(),
        options = list(pageLength = 5, dom = "tip", pagingType = "simple"),
        rownames = FALSE,
        class = "stripe hover"
      )
    })

    return(list(
      data    = dataset_r,
      dataset = reactive(input$dataset)
    ))
  })
}

## To be copied in the UI
# mod_dataset_ui("dataset_1")

## To be copied in the server
# mod_dataset_server("dataset_1")
