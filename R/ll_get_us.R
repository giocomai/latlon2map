#' Get US counties 
#' 
#' Source: https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
#'
#' @param level Defaults to "county". Available options are: "cd116" (for congressional districts of the 116th Congress)
#' @param resolution Defaults to "500k", max available resolution. Available options are: "5m" and "20m"
#' @param year Defaults to 2018
#'
#' @return
#' @export
#'
#' @examples
#' ll_get_nuts_us(level = "county",resolution = "500k",year = 2018)
#' 
ll_get_nuts_us <- function(level = "county",
                      resolution = "500k",
                      year = 2018) {
  usethis::ui_info("Source: https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html")
  ll_create_folders(geo = "us",
                    level = level,
                    resolution = resolution,
                    year = year)

  rds_file <- ll_find_file(geo = "us",
                           level = level,
                           resolution = resolution,
                           year = year,
                           name = "abl",
                           file_type = "rds")
  
  if (fs::file_exists(rds_file)) {
    return(readr::read_rds(file = rds_file))
  }
  
  shp_folder <- ll_find_file(geo = "us",
                             level = level,
                             resolution = resolution,
                             year = year,
                             name = "abl",
                             file_type = "shp")
  
    zip_file <- ll_find_file(geo = "us",
                             level = level,
                             resolution = resolution,
                             year = year,
                             name = "abl",
                             file_type = "zip")
    source_url <- paste0("https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_", year, "_us_", level, "_", resolution, ".zip")
    
    if (fs::file_exists(zip_file)==FALSE) {
      download.file(url = source_url,
                    destfile = zip_file)
    }
    unzip(zipfile = zip_file,
          exdir = shp_folder)
  sf <- sf::read_sf(shp_folder)
  readr::write_rds(x = sf,
                   file = rds_file)
  return(sf)
}