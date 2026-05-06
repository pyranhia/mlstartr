#' export UI Function
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @importFrom shiny NS tagList
mod_export_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::card(
      class = "card-pedagogique",
      bslib::card_body(
        p(style = "font-size: 1rem; margin: 0;",
          "Voici le code R correspondant aux choix que vous avez effectu\u00e9s dans l'application. ",
          "Vous pouvez le copier et l'ex\u00e9cuter dans RStudio pour reproduire votre analyse.")
      )
    ),
    br(),
    bslib::card(
      bslib::card_header("Code R g\u00e9n\u00e9r\u00e9"),
      bslib::card_body(
        verbatimTextOutput(ns("code_output")),
        br(),
        div(
          style = "text-align: center;",
          downloadButton(ns("download"), "T\u00e9l\u00e9charger le script R",
                         class = "btn-primary btn-lg")
        )
      )
    )
  )
}

#' export Server Functions
#'
#' @noRd
mod_export_server <- function(id, code_log) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Assembler le code
    full_code <- reactive({
      blocs <- Filter(Negate(is.null), code_log())
      if (length(blocs) == 0) return("# Aucun code g\u00e9n\u00e9r\u00e9 pour l'instant.")
      paste(blocs, collapse = "\n\n")
    })

    output$code_output <- renderText({
      full_code()
    })

    output$download <- downloadHandler(
      filename = function() "mlstartr_script.R",
      content  = function(file) {
        writeLines(full_code(), file)
      }
    )
  })
}

## To be copied in the UI
# mod_export_ui("export_1")

## To be copied in the server
# mod_export_server("export_1")
