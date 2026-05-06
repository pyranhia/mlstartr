app_server <- function(input, output, session) {

  # Cacher les onglets suivants au demarrage
  bslib::nav_hide(id = "tabs", target = "exploration")
  bslib::nav_hide(id = "tabs", target = "pretraitement")
  bslib::nav_hide(id = "tabs", target = "modelisation")
  bslib::nav_hide(id = "tabs", target = "evaluation")

  # Log du code genere
  code_log <- reactiveVal(list(
    dataset       = NULL,
    pretraitement = NULL,
    modelisation  = NULL,
    evaluation    = NULL
  ))

  # Modules
  mod_intro_server("intro_1")
  dataset       <- mod_dataset_server("dataset_1")
  vars <- mod_variables_server(
    "vars_1",
    dataset_r    = dataset$data,
    code_log     = code_log,
    dataset_name = dataset$dataset
  )
  exploration   <- mod_exploration_server("exploration_1", dataset_r = dataset$data, vars_r = vars)
  pretraitement <- mod_pretraitement_server("pretraitement_1", dataset_r = dataset$data, vars_r = vars, code_log = code_log)
  modelisation  <- mod_modelisation_server("modelisation_1", pretraitement_r = pretraitement, vars_r = vars, code_log = code_log)
  mod_evaluation_server("evaluation_1", pretraitement_r = pretraitement, modelisation_r = modelisation, vars_r = vars, code_log = code_log)

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
