#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib page_navbar nav_panel nav_spacer bs_theme card card_header
#' @noRd

app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    bslib::page_navbar(
      title = "MLstartr",
      id = "tabs",
      theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),

      # Tab 1 - Introduction
      bslib::nav_panel(
        title = "Introduction",
        value = "intro",
        mod_intro_ui("intro_1")
      ),

      # Tab 2 - Données
      bslib::nav_panel(
        title = "Données",
        value = "data",
        bslib::card(
          bslib::card_header("1. Choix du jeu de données"),
          mod_dataset_ui("dataset_1")
        ),
        bslib::card(
          bslib::card_header("2. Sélection des variables"),
          mod_variables_ui("vars_1")
        )
      ),

      # Tab 3 - Exploration (masqué)
      bslib::nav_panel(
        title = "Exploration",
        value = "exploration",
        mod_exploration_ui("exploration_1")
      ),

      # Tab 4 - Prétraitement (masqué)
      bslib::nav_panel(
        title = "Prétraitement",
        value = "pretraitement",
        mod_pretraitement_ui("pretraitement_1")
      ),

      # Tab 5 - Modélisation (masqué)
      bslib::nav_panel(
        title = "Modélisation",
        value = "modelisation",
        mod_modelisation_ui("modelisation_1")
      ),

      # Tab 6 - Évaluation (masqué)
      bslib::nav_panel(
        title = "Évaluation",
        value = "evaluation",
        mod_evaluation_ui("evaluation_1")
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' @import shiny
#' @importFrom golem add_resource_path favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path("www", app_sys("app/www"))

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "mlstartr"
    )
  )
}
