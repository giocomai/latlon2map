#' file_input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_file_input_ui <- function(id, label = "CSV file"){
  ns <- NS(id)
  tagList(
    fileInput(inputId = ns("file_input"),
              label = label)
  )
}
    
#' file_input Server Function
#'
#' @noRd 
mod_file_input_server <- function(input, output, session){
  ns <- session$ns
  user_file <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$file_input, message = FALSE))
    input$file_input
  })
  
  df <- reactive({
    readr::read_csv(file = user_file()$datapath)
  })

  observe({
    cat(paste0(user_file()$name, " uploaded", "\n"))
  })
  
  return(df)
}
    
## To be copied in the UI
# mod_file_input_ui("file_input_ui_1")
    
## To be copied in the server
# callModule(mod_file_input_server, "file_input_ui_1")
 
