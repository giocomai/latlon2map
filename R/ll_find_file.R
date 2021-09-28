#' Find file names. Mostly used internally
#'
#' @param geo
#' @param level
#' @param resolution
#' @param year
#' @param name Name of specific dataset being downloaded. Defaults to abl, i.e. administrative boundary line
#' @param file_type
#'
#' @return
#' @export
#'
#' @examples
ll_find_file <- function(geo,
                         level,
                         resolution,
                         year,
                         name = "abl",
                         file_type = "rds") {
  base_folder <- ll_set_folder()
  name <- stringr::str_replace_all(string = name, pattern = "[[:punct:]]", replacement = "_")
  file_name <- paste0(paste(c(geo, level, resolution, year), collapse = "-"), "-", name, ".", file_type)
  full_path <- fs::path(base_folder, "ll_data", file_type, geo, level, resolution, year, file_name)
  if (file_type == "shp") {
    full_path %>% fs::path_dir()
  } else {
    full_path
  }
}
