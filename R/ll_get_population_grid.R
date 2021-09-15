#' Get EU 1km population grid
#' 
#' Source: https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/population-distribution-demography/geostat
#' More details: https://ec.europa.eu/eurostat/statistics-explained/index.php/Population_grids
#'
#' @param year Defaults to 2018. Currently, the EU population grid is available only for the year 2006, 2011, and 2018.
#' @param match_sf An sf object to be matched with the population grid. If not given, full grid is returned.
#' @param match_name A name to be used for local caching. It is the responsibility of the user to keept it consistent. If not given, data are not cached locally.
#' @param match_country Defaults to NULL. If given, used to speed up processing. 
#' @param population_grid_sf Defaults to NULL. If given, it uses this one as population grid of reference. Useful to bulk process items, as it removes the need for re-loading the grid from local storage at each iteration.
#'
#' @return An sf object with the population grid.
#' @export
#'
#' @examples
ll_get_population_grid <- function(year = 2018,
                                   match_sf = NULL,
                                   match_name = NULL,
                                   match_country = NULL,
                                   join = sf::st_intersects,
                                   silent = FALSE,
                                   population_grid_sf = NULL) {
  if (silent==FALSE) {
    usethis::ui_info(x = "Data source population grid information: Eurostat, EFGS")
    usethis::ui_info(x = "Source: https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/population-distribution-demography/geostat")
  }
 
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
      return(readr::read_rds(file = rds_file_location))
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
  
  if (is.null(population_grid_sf)==FALSE) {
    sf <- population_grid_sf
  } else if (fs::file_exists(rds_file)) {
    sf <- readr::read_rds(file = rds_file)
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

    if (year == 2018) {
      source_url <- "https://ec.europa.eu/eurostat/cache/GISCO/geodatafiles/JRC_GRID_2018.zip"
    } else if (year == 2011) {
      source_url <- "https://ec.europa.eu/eurostat/cache/GISCO/geodatafiles/GEOSTAT-grid-POP-1K-2011-V2-0-1.zip"
    } else if (year == 2006 ) {
      source_url <- "https://ec.europa.eu/eurostat/cache/GISCO/geodatafiles/GEOSTAT_Grid_POP_2006_1K.zip"
    } else (
      usethis::ui_stop("Please provide a valid year.")
    )
    
    
    
    if (fs::file_exists(zip_file)==FALSE) {
      download.file(url = source_url,
                    destfile = zip_file)
    }
    unzip(zipfile = zip_file,
          exdir = shp_folder)
    
    if (year == 2018) {
      sf <- sf::read_sf(fs::path(shp_folder), layer = "JRC_POPULATION_2018") %>% 
        sf::st_transform(crs = 4326)
    } else if (year == 2011) {
      sf <- sf::read_sf(fs::path(shp_folder, "Version 2_0_1", "GEOSTATReferenceGrid"))  %>%
        dplyr::right_join(readr::read_csv(fs::path(shp_folder, "Version 2_0_1", "GEOSTAT_grid_POP_1K_2011_V2_0_1.csv")),
                          by = "GRD_ID") %>% 
        sf::st_transform(crs = 4326)
    } else if (year == 2006) {
      sf <- sf::read_sf(fs::path(shp_folder)) %>% 
        dplyr::rename(GRD_ID = .data$GRD_INSPIR) %>% 
        dplyr::right_join(readr::read_delim(file = fs::path(shp_folder, "GEOSTAT_grid_EU_POP_2006_1K_V1_1_1.csv"),
                                            delim = ";",
                                          col_names = c("GRD_ID", "POP_TOT", "YEAR", "METHD_CL", "CNTR_CODE", "DATA_SRC"),
                                          col_types = "c", ),
                          by = "GRD_ID") %>% 
        sf::st_transform(crs = 4326)
    }

    
    if (is.null(match_country)==FALSE) {
      if (year == 2018) {
        sf <- sf %>%
          dplyr::filter(stringr::str_detect(string = CNTR_ID, pattern = match_country))
      } else {
        sf <- sf %>%
          dplyr::filter(stringr::str_detect(string = CNTR_CODE, pattern = match_country))
      }
    }
    readr::write_rds(x = sf,
                     file = rds_file)
  }

  if (is.null(match_sf)==FALSE) {
    sf <- sf::st_filter(x = sf %>% sf::st_transform(crs = 3857),
                        y = match_sf %>% sf::st_transform(crs = 3857),
                        join = join) %>%
      sf::st_transform(crs = 4326)
  }
  
  if (is.null(match_name)==FALSE) {
    readr::write_rds(x = sf,
                     file = rds_file_location)
  }
  return(sf)
}