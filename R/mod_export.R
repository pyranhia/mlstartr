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
          "Voici le code R correspondant aux choix que vous avez effectu\u00e9s dans l\u2019application. ",
          "Vous pouvez le copier et l\u2019ex\u00e9cuter dans RStudio pour reproduire votre analyse. ",
          "Les lignes d\u2019installation sont comment\u00e9es \u2014 ex\u00e9cutez-les une seule fois si les packages ne sont pas encore install\u00e9s.")
      )
    ),
    br(),
    bslib::card(
      bslib::card_header(
        div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          span("Code R g\u00e9n\u00e9r\u00e9"),
          tags$button(
            id = ns("copy_btn"),
            class = "btn btn-sm btn-outline-secondary",
            onclick = sprintf(
              "navigator.clipboard.writeText(document.getElementById('%s').innerText).then(function() {
                var btn = document.getElementById('%s');
                btn.innerText = 'Copi\u00e9 !';
                setTimeout(function() { btn.innerText = 'Copier'; }, 2000);
              });",
              ns("code_output"),
              ns("copy_btn")
            ),
            "Copier"
          )
        )
      ),
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

    # En-tete avec les installations
    header_code <- paste0(
      "# Installation des packages (a executer une seule fois)\n",
      "# install.packages(c('tidymodels', 'ranger', 'pak'))\n",
      "# pak::pak('pyranhia/datapyranhia')
    )

    # Assembler le code
    full_code <- reactive({
      blocs <- Filter(Negate(is.null), code_log())
      if (length(blocs) == 0) return("# Aucun code g\u00e9n\u00e9r\u00e9 pour l\u2019instant.")
      paste(c(header_code, blocs), collapse = "\n\n")
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
