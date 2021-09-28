#' Run the Shiny Application
#'
#' @param ... A series of options to be used inside the app.
#' @param max_file_size Maximum file size to accept for upload expressed in MB, defaults to 100.
#' @param ll_folder_path If given, sets the folder to use for caching, corresponds to `ll_set_folder()`. Useful e.g. for Docker deployments. Defaults to NULL.
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
ll_app <- function(max_file_size = 100,
                   ll_folder_path = NULL,
                   ...) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server
    ),
    golem_opts = list(
      shiny.maxRequestSize = max_file_size * 1024^2,
      ll_folder_path = ll_folder_path,
      ...
    )
  )
}
