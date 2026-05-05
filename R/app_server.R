#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # Modules tab 2
  dataset <- mod_dataset_server("dataset_1")
  vars    <- mod_variables_server("vars_1", dataset_r = dataset)

  # Cacher les onglets suivants au demarrage
  bslib::nav_hide(id = "tabs", target = "exploration")
  bslib::nav_hide(id = "tabs", target = "pretraitement")
  bslib::nav_hide(id = "tabs", target = "modelisation")
  bslib::nav_hide(id = "tabs", target = "evaluation")

  # Deverrouiller l'exploration quand tab 2 est valide
  observeEvent(vars$validated(), {
    if (isTRUE(vars$validated())) {
      bslib::nav_show(id = "tabs", target = "exploration")
      bslib::nav_select(id = "tabs", selected = "exploration")
    } else {
      bslib::nav_hide(id = "tabs", target = "exploration")
      bslib::nav_hide(id = "tabs", target = "pretraitement")
      bslib::nav_hide(id = "tabs", target = "modelisation")
      bslib::nav_hide(id = "tabs", target = "evaluation")
    }
  })

  # Modules tabs suivants (stubs pour l'instant)
  mod_intro_server("intro_1")
  mod_exploration_server("exploration_1")
  mod_pretraitement_server("pretraitement_1")
  mod_modelisation_server("modelisation_1")
  mod_evaluation_server("evaluation_1")
}
