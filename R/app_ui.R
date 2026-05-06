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

      # Tab 2 - Donn\u00e9es
      bslib::nav_panel(
        title = "Donn\u00e9es",
        value = "data",
        bslib::card(
          style = "background-color: #f0f7ff;",
          bslib::card_body(
            p(style = "font-size: 1rem; margin: 0;",
              "Dans cette \u00e9tape, vous choisissez sur quoi vous allez travailler. Le ",
              strong("jeu de donn\u00e9es"), " contient les exemples que l'algorithme va utiliser
        pour apprendre. La ", strong("variable r\u00e9ponse"), " est ce que vous voulez pr\u00e9dire.
        Les ", strong("variables pr\u00e9dictives"), " sont les informations utilis\u00e9es
        pour faire la pr\u00e9diction.")
          )
        ),
        br(),
        bslib::layout_columns(
          col_widths = c(8, 4),
          bslib::card(
            bslib::card_header("1. Choix du jeu de donn\u00e9es"),
            mod_dataset_ui("dataset_1")
          ),
          bslib::card(
            bslib::card_header("2. S\u00e9lection des variables"),
            mod_variables_ui("vars_1")
          )
        )
      ),

      # Tab 3 - Exploration (masqu\u00e9)
      bslib::nav_panel(
        title = "Exploration",
        value = "exploration",
        mod_exploration_ui("exploration_1")
      ),

      # Tab 4 - Pr\u00e9traitement (masqu\u00e9)
      bslib::nav_panel(
        title = "Pr\u00e9traitement",
        value = "pretraitement",
        mod_pretraitement_ui("pretraitement_1")
      ),

      # Tab 5 - Mod\u00e9lisation (masqu\u00e9)
      bslib::nav_panel(
        title = "Mod\u00e9lisation",
        value = "modelisation",
        mod_modelisation_ui("modelisation_1")
      ),

      # Tab 6 - \u00c9valuation (masqu\u00e9)
      bslib::nav_panel(
        title = "\u00c9valuation",
        value = "evaluation",
        mod_evaluation_ui("evaluation_1")
      ),

      # Tab 7 - Export (masqu\u00e9)
      bslib::nav_panel(
        title = "Export",
        value = "export",
        mod_export_ui("export_1")
      ),

      bslib::nav_spacer(),
      bslib::nav_item(
        actionButton(
          "reset",
          "Recommencer",
          class = "btn-outline-light btn-sm"
        )
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
    ),
    tags$style(HTML("
      .bslib-page-fill { overflow-y: auto !important; height: auto !important; }
      .tab-content { overflow: visible !important; }
      .tab-pane { overflow: visible !important; }
    "))
  )
}
