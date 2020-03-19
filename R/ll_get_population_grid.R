#' Get EU 1km population grid
#' 
#' Source: https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/population-distribution-demography/geostat
#' More details: https://ec.europa.eu/eurostat/statistics-explained/index.php/Population_grids
#'
#' @return An sf object with the population grid.
#' @export
#'
#' @examples
ll_get_population_grid <- function(year = 2011) {
  usethis::ui_info(x = "Source: https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/population-distribution-demography/geostat")
  ll_create_folders(geo = "eu",
                    level = "eu",
                    resolution = "1km",
                    year = year)
  
  rds_file <- ll_find_file(geo = "eu",
                           level = "eu",
                           resolution = "1km",
                           year = year,
                           name = "population_grid",
                           file_type = "rds")
  
  if (fs::file_exists(rds_file)) {
    return(readr::read_rds(path = rds_file))
  }
  
  shp_folder <- ll_find_file(geo = "eu",
                             level = "eu",
                             resolution = "1km",
                             year = year,
                             name = "population_grid",
                             file_type = "shp")
  
  if (fs::file_exists(path = shp_folder) == FALSE) {
    zip_file <- ll_find_file(geo = "eu",
                             level = "eu",
                             resolution = "1km",
                             year = year,
                             name = "population_grid",
                             file_type = "zip")
    source_url <- paste0("https://ec.europa.eu/eurostat/cache/GISCO/geodatafiles/GEOSTAT-grid-POP-1K-", year, "-V2-0-1.zip")
    
    if (fs::file_exists(zip_file)==FALSE) {
      download.file(url = source_url,
                    destfile = zip_file)
    }
    unzip(zipfile = zip_file,
          exdir = shp_folder)
  }
  sf <- sf::read_sf(fs::path(shp_folder, "Version 2_0_1", "GEOSTATReferenceGrid"))  %>%
    dplyr::right_join(read_csv(fs::path(shp_folder, "Version 2_0_1", "GEOSTAT_grid_POP_1K_2011_V2_0_1.csv")),
                      by = "GRD_ID") %>% 
    sf::st_transform(crs = 4326)
  readr::write_rds(x = sf,
                   path = rds_file)
  return(sf)
}