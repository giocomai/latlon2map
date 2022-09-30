#' Get administrative boundaries
#'
#' Source: https://gadm.org/
#'
#' @param geo Three letter country codes. If a two letter country code is given, it will tentatively be converted to a three-letter country code. Check consistency.
#' @param level Defaults to 0. Available labels, depending on data availability for the specific country, between 0 and 3. 
#' @param version Defaults to "4.0". Untested with others. 
#'
#' @return An `sf` object
#' @export
#'
#' @examples
#' ll_get_gadm(geo = "UKR", level = 2)
ll_get_gadm <- function(geo,
                        level = 0, 
                        version = "4.1") {
  usethis::ui_info("Source: https://gadm.org/")
  usethis::ui_info("The data are freely available for academic use and other non-commercial use. Redistribution, or commercial use, is not allowed without prior permission. Using the data to create maps for academic publishing is allowed.")
  
  if (nchar(geo)==2) {
    geo <- countrycode::countrycode(sourcevar = geo,
                                    origin = "iso2c",
                                    destination = "iso3c")
  }
  
  geo <- stringr::str_to_upper(geo)
  
  year <- stringr::str_replace(string = version,
                               pattern = stringr::fixed("."),
                               replacement = "_") # version
  resolution <- "NA"
  
  ll_create_folders(
    geo = geo,
    level = level,
    resolution = resolution,
    year = year,
    file_type = "rds"
  )
  

  rds_file <- ll_find_file(
    geo = geo,
    level = level,
    resolution = resolution,
    year = year,
    name = "abl",
    file_type = "rds"
  )

  
  if (fs::file_exists(rds_file)) {
    sf <- readr::read_rds(file = rds_file)
  } else {
    shp_folder <- ll_find_file(
      geo = geo,
      level = level,
      resolution = resolution,
      year = year,
      name = "abl",
      file_type = "shp"
    )
    
    source_url <- stringr::str_c("https://geodata.ucdavis.edu/gadm/gadm",
                                 version, 
                                 "/shp/gadm", 
                                 stringr::str_remove(version, stringr::fixed(".")),
                                 "_", 
                                 geo, 
                                 "_shp",
                                 ".zip")
    
    
    zip_file <- ll_find_file(
      geo = geo,
      level = level,
      resolution = resolution,
      year = year,
      name = "abl",
      file_type = "zip"
    )
    
    
    ll_create_folders(
      geo = geo,
      level = level,
      resolution = resolution,
      year = year,
      file_type = "zip"
    )
    
    if (fs::file_exists(zip_file) == FALSE) {
      download.file(
        url = source_url,
        destfile = zip_file
      )
    }
    
    zip_folder <- ll_find_file(
      geo = geo,
      level = level,
      resolution = resolution,
      year = year,
      name = "abl",
      file_type = "zip"
    ) %>%
      fs::path_dir()
    
    unzip(
      zipfile = zip_file,
      exdir = shp_folder
    )
    current_level_file <- fs::path(shp_folder,
                                   stringr::str_c("gadm",
                                                  stringr::str_remove(version, stringr::fixed(".")),
                                                  "_", 
                                                  geo,
                                                  "_",
                                                  level, 
                                                  ".shp"))
    
    sf <- sf::read_sf(current_level_file) %>%
      sf::st_transform(crs = 4326)
    
    
    saveRDS(
      object = sf,
      file = rds_file
    )
  }
  
  return(sf)
         
}
