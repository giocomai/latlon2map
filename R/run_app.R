#' Run the Shiny Application
#'
#' @param ... A series of options to be used inside the app.
#' @param max_file_size Maximum file size to accept for upload expressed in MB, defaults to 100.
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
ll_app <- function(max_file_size = 100,
  ...
) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui, 
      server = app_server
    ), 
    golem_opts = list(shiny.maxRequestSize=max_file_size*1024^2, ...)
  )
}
