#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

library(shinydashboard)

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage(
      dashboardHeader(title = "MLstartr"),
      dashboardSidebar(
        sidebarMenu(
          id = "tabs",
          menuItem("Introduction", tabName = "intro")
          #menuItem("Données", tabName = "data", disabled = TRUE)
          #menuItem("Modèle", tabName = "model", disabled = TRUE)
        )
      ),
      dashboardBody(
        tabItems(
          tabItem(tabName = "intro",
                  mod_intro_ui("intro_1")
          )
          #tabItem(tabName = "data",
          #        mod_data_ui("data_1")
          #),
          #tabItem(tabName = "model",
          #        mod_model_ui("model_1")
          #)
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
