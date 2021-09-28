#' file_input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_file_input_ui <- function(id, label = "CSV file") {
  ns <- NS(id)
  tagList(
    fileInput(
      inputId = ns("file_input"),
      label = label
    )
  )
}

#' file_input Server Function
#'
#' @noRd
mod_file_input_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    user_file <- reactive({
      # If no file is selected, don't do anything
      validate(need(input$file_input, message = FALSE))
      input$file_input
    })

    df <- reactive({
      if (fs::path_ext(user_file()$name) == "csv") {
        readr::read_csv(file = user_file()$datapath)
      } else if (fs::path_ext(user_file()$name) == "tsv") {
        readr::read_tsv(file = user_file()$datapath)
      } else if (fs::path_ext(user_file()$name) == "xslx" | fs::path_ext(user_file()$name) == "xsl") {
        readxl::read_excel(path = user_file()$datapath)
      } else if (fs::path_ext(user_file()$name) == "ods") {
        readODS::read_ods(path = user_file()$datapath)
      }
    })

    return(df)
  })
}

## To be copied in the UI
# mod_file_input_ui("file_input_ui_1")

## To be copied in the server
# callModule(mod_file_input_server, "file_input_ui_1")
