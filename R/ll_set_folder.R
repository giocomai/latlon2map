#' Set folder for caching data
#'
#' @param path A path to a location. If the folder does not exist, it will be created.
#'
#' @return The path to the caching folder, if previously set.
#' @export

#' @examples
ll_set_folder <- function(path = NULL) {
  if (is.null(path)) {
    path <- Sys.getenv("ll_base_folder")
  } else {
    Sys.setenv(ll_base_folder = path)
  }
  if (path == "") {
    path <- getwd()
  }
  path
}
