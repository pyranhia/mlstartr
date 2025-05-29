#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd


app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    shinydashboard::dashboardPage(
      shinydashboard::dashboardHeader(title = "MLstartr"),
      shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          id = "tabs",
          shinydashboard::menuItem("Introduction", tabName = "intro"),
          shinydashboard::menuItem("Données", tabName = "data")
          #shinydashboard::menuItem("Modèle", tabName = "model", disabled = TRUE)
        )
      ),
      shinydashboard::dashboardBody(
        shinydashboard::tabItems(

          shinydashboard::tabItem(
            tabName = "intro",
            mod_intro_ui("intro_1")
          ),

          shinydashboard::tabItem(
            tabName = "data",
            fluidRow(
              shinydashboard::box(
                width = 12,
                title = "1. Choix du jeu de données",
                status = "primary",
                solidHeader = TRUE,
                mod_dataset_ui("dataset_1")
              )
            ),
            fluidRow(
              shinydashboard::box(
                width = 12,
                title = "2. Sélection des variables",
                status = "info",
                solidHeader = TRUE,
                mod_variables_ui("vars_1")
              )
            )
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "mlstartr"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
