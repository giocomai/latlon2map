#' Gets NUTS as sf object from Eurostat's website 
#'
#' Source: https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/nuts#nuts16
#'
#' @param level Defaults to 3, corresponding to nuts3. Available values are: 0, 1, 2, and 3.
#' @param resolution Defaults to "60", for 1:60 Million. Available values: are 20, 10, 3, 1 (1 is highest quality available)-
#' @param year Defaults to 2016. Available values: 2016, 2013, 2010, 2006, 2001
#' @return NUTS in sf format
#' @export
#'
#' @examples
#' ll_get_lau_eu()
ll_get_nuts_eu <- function(name = NULL, 
                           level = 3,
                           resolution = 60,
                           year = 2016,
                           silent = FALSE) {
  
  resolution <- stringr::str_pad(string = resolution, width = 2, side = "left", pad = 0)
  
  if (silent==FALSE) {
    usethis::ui_info("© EuroGeographics for the administrative boundaries")
    usethis::ui_info("Source: https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/countries")
  }
  
  ll_create_folders(geo = "eu",
                    level = level,
                    resolution = resolution,
                    year = year)
  
  ll_create_folders(geo = "eu",
                    level = "all_levels",
                    resolution = resolution,
                    year = year)
  
  rds_file <- ll_find_file(geo = "eu",
                           level = level,
                           resolution = resolution,
                           year = year,
                           name = "abl",
                           file_type = "rds")
  
  if (is.null(name)==FALSE) {
    rds_file_location <- ll_find_file(geo = "eu",
                                      level = level,
                                      resolution = resolution,
                                      year = year,
                                      name = paste0(level, "-", stringr::str_replace(string = name, pattern = "[[:punct:]]", replacement = "_")),
                                      file_type = "rds")
    
    if (fs::file_exists(rds_file_location)) {
      return(readr::read_rds(rds_file_location))
    }
  }
  
  if (fs::file_exists(rds_file)) {
    sf <- readr::read_rds(path = rds_file)
  } else {
    
    shp_folder <- ll_find_file(geo = "eu",
                               level = "all_levels",
                               resolution = resolution,
                               year = year,
                               name = "abl",
                               file_type = "shp")
    
    shp_folder_level <- fs::path(shp_folder, paste0("NUTS_RG_", resolution, "M_", year, "_4326_LEVL_", level, ".shp"))
    
    if (fs::file_exists(shp_folder_level)==FALSE) {
      
      zip_file <- ll_find_file(geo = "eu",
                               level = "all_levels",
                               resolution = resolution,
                               year = year,
                               name = "abl",
                               file_type = "zip")
      source_url <- paste0("https://ec.europa.eu/eurostat/cache/GISCO/distribution/v2/nuts/download/ref-nuts-", year, "-", resolution, "m.shp.zip")
      
      if (fs::file_exists(zip_file)==FALSE) {
        download.file(url = source_url,
                      destfile = zip_file)
      }
      unzip(zipfile = zip_file,
            exdir = shp_folder)
      unzip(zipfile = paste0(shp_folder_level, ".zip"),
            exdir = fs::path(shp_folder, paste0("4326-nuts", level)))
      
    }
    sf <- sf::read_sf(fs::path(shp_folder, paste0("4326-nuts", level))) 
    readr::write_rds(x = sf,
                     path = rds_file)
  }
  if (is.null(name)==FALSE) {
    sf <- sf %>% 
      dplyr::filter(NUTS_NAME == name)
    readr::write_rds(x = sf,
                     path = rds_file_location)
  }
  return(sf)
}
