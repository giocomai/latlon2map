#' Create folders to store geographic data
#'
#' @param geo The geographic unit of reference as a two-letter code
#' @param level E.g. NUTS0, NUTS1, or county, state, ecc.
#' @param resolution Either resolution level as given by the data distributor, or generic such as "high", "low", or "default
#' @param file_type By defaults, it creates folder for zip, shp, and rds files.
#'
#' @return
#' @export
#'
#' @examples
ll_create_folders <- function(geo,
                              level,
                              resolution,
                              year,
                              file_type = c("shp", "zip", "rds")) {
  base_folder <- ll_set_folder()
  purrr::walk(file_type, .f = function(x) {
    fs::dir_create(fs::path(base_folder, "ll_data", x, geo, level, resolution, year), recurse = TRUE)
  })
}
