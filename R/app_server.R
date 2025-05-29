#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  mod_intro_server("intro_1")
  dataset <- mod_dataset_server("dataset_1")
  vars <- mod_variables_server("vars_1", dataset_r = dataset)
}
