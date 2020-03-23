#' Get EU 1km population grid
#' 
#' Source: https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/population-distribution-demography/geostat
#' More details: https://ec.europa.eu/eurostat/statistics-explained/index.php/Population_grids
#'
#' @param year Defaults to 2011. Currently, the EU population grid is available only for the year 2011.
#' @param match_sf An sf object to me matched with the population grid. If not given, full grid is returned.
#' @param match_name A name to be used for local caching. It is the responsibility of the user to keept it consistent. If not given, data are not cached locally.
#' @param match_country Defaults to NULL. If given, used to speed up processing. 
#'
#' @return An sf object with the population grid.
#' @export
#'
#' @examples
ll_get_population_grid <- function(year = 2011,
                                   match_sf = NULL,
                                   match_name = NULL, 
                                   match_country = NULL,
                                   join = sf::st_intersects) {
  usethis::ui_info(x = "Source: https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/population-distribution-demography/geostat")
  
  if (is.null(match_country)==FALSE) {
    match_country <- stringr::str_to_upper(string = match_country)
  }
  
  if (is.null(match_name)==FALSE) {
    rds_file_location <- ll_find_file(geo = "eu",
                                      level = "eu",
                                      resolution = "1km",
                                      year = year,
                                      name = paste0(match_name, "-population_grid", "-", if(is.null(match_country)) "eu" else match_country),
                                      file_type = "rds")
    
    if (fs::file_exists(rds_file_location)) {
      return(readr::read_rds(rds_file_location))
    }
  }
  
  ll_create_folders(geo = "eu",
                    level = "eu",
                    resolution = "1km",
                    year = year)
  
  rds_file <- ll_find_file(geo = "eu",
                           level = "eu",
                           resolution = "1km",
                           year = year,
                           name = paste0("population_grid", "-", if(is.null(match_country)) "eu" else match_country),
                           file_type = "rds")
  
  if (fs::file_exists(rds_file)) {
    sf <- readr::read_rds(path = rds_file)
  } else {
    shp_folder <- ll_find_file(geo = "eu",
                               level = "eu",
                               resolution = "1km",
                               year = year,
                               name = "population_grid",
                               file_type = "shp")
    
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
    
    sf <- sf::read_sf(fs::path(shp_folder, "Version 2_0_1", "GEOSTATReferenceGrid"))  %>%
      dplyr::right_join(read_csv(fs::path(shp_folder, "Version 2_0_1", "GEOSTAT_grid_POP_1K_2011_V2_0_1.csv")),
                        by = "GRD_ID") %>% 
      sf::st_transform(crs = 4326)
    
    if (is.null(match_country)==FALSE) {
      sf <- sf %>%
        dplyr::filter(CNTR_CODE == match_country)
    }
    readr::write_rds(x = sf,
                     path = rds_file)
  }

  if (is.null(match_sf)==FALSE) {
    sf <- sf::st_filter(x = sf,
                        y = match_sf,
                        join = join)
  }
  
  if (is.null(match_name)==FALSE) {
    readr::write_rds(x = sf,
                     path = rds_file_location)
  }
  return(sf)
}