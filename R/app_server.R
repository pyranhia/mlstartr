#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # Cacher les onglets suivants au demarrage
  bslib::nav_hide(id = "tabs", target = "exploration")
  bslib::nav_hide(id = "tabs", target = "pretraitement")
  bslib::nav_hide(id = "tabs", target = "modelisation")
  bslib::nav_hide(id = "tabs", target = "evaluation")

  # Modules
  mod_intro_server("intro_1")
  dataset       <- mod_dataset_server("dataset_1")
  vars          <- mod_variables_server("vars_1", dataset_r = dataset)
  exploration   <- mod_exploration_server("exploration_1", dataset_r = dataset, vars_r = vars)
  pretraitement <- mod_pretraitement_server("pretraitement_1", dataset_r = dataset, vars_r = vars)
  modelisation  <- mod_modelisation_server("modelisation_1", pretraitement_r = pretraitement, vars_r = vars)
  mod_evaluation_server("evaluation_1")

  # Verrouillage des onglets
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

  observeEvent(exploration$validated(), {
    if (isTRUE(exploration$validated())) {
      bslib::nav_show(id = "tabs", target = "pretraitement")
      bslib::nav_select(id = "tabs", selected = "pretraitement")
    } else {
      bslib::nav_hide(id = "tabs", target = "pretraitement")
      bslib::nav_hide(id = "tabs", target = "modelisation")
      bslib::nav_hide(id = "tabs", target = "evaluation")
    }
  })

  observeEvent(pretraitement$validated(), {
    if (isTRUE(pretraitement$validated())) {
      bslib::nav_show(id = "tabs", target = "modelisation")
      bslib::nav_select(id = "tabs", selected = "modelisation")
    } else {
      bslib::nav_hide(id = "tabs", target = "modelisation")
      bslib::nav_hide(id = "tabs", target = "evaluation")
    }
  })

  observeEvent(modelisation$validated(), {
    if (isTRUE(modelisation$validated())) {
      bslib::nav_show(id = "tabs", target = "evaluation")
      bslib::nav_select(id = "tabs", selected = "evaluation")
    } else {
      bslib::nav_hide(id = "tabs", target = "evaluation")
    }
  })
}
