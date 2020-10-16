#' Gets local administrative units from Eurostat's website 
#'
#' Source: https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/lau#lau18
#'
#' @param year Year of mapping, defaults to most recent (2018). Availalble values: 2017, 2018
#' @param lau_sf sf object, exactly such as the one that would be returned by `ll_get_lau_eu()`. Used to speed-up computation when bulk processing.
#'
#' @return European LAU in sf format
#' @export
#'
#' @examples
#' 
#' ll_set_folder("~/R/")
#' ll_get_lau_eu()
#' 
ll_get_lau_eu <- function(name = NULL,
                          year = 2018,
                          silent = FALSE, 
                          lau_sf = NULL) {
  if (silent==FALSE) {
    usethis::ui_info(x = "© EuroGeographics for the administrative boundaries")
  }
  ll_create_folders(geo = "eu",
                    level = "lau",
                    resolution = "1m",
                    year = year)
  rds_file <- ll_find_file(geo = "eu",
                           level = "lau",
                           resolution = "1m",
                           year = year,
                           name = "abl",
                           file_type = "rds")
  
  if (is.null(name)==FALSE) {
    rds_file_location <- ll_find_file(geo = "eu",
                                      level = "lau",
                                      resolution = "1m",
                                      year = year,
                                      name = paste0("lau-", stringr::str_replace(string = name, pattern = "[[:punct:]]", replacement = "_")),
                                      file_type = "rds")
    
    if (fs::file_exists(rds_file_location)) {
      return(readr::read_rds(file = rds_file_location))
    }
  }
  
  if (is.null(lau_sf)==FALSE) {
    sf <- lau_sf
  } else if (fs::file_exists(rds_file)) {
    sf <- readr::read_rds(file = rds_file)
  } else {
    shp_folder <- ll_find_file(geo = "eu",
                               level = "lau",
                               resolution = "1m",
                               year = year,
                               name = "abl",
                               file_type = "shp")
    
    source_url <- paste0("https://ec.europa.eu/eurostat/cache/GISCO/geodatafiles/LAU-", year, "-01M-SH.zip")
    zip_file <- ll_find_file(geo = "eu",
                             level = "lau",
                             resolution = "1m",
                             year = year,
                             name = "abl",
                             file_type = "zip")
    
    if (fs::file_exists(zip_file)==FALSE) {
      download.file(url = source_url,
                    destfile = zip_file)
    }
    unzip(zipfile = zip_file,
          exdir = shp_folder)
    sf <- sf::read_sf(shp_folder) %>% 
      dplyr::mutate(CNTR_CODE = stringr::str_extract(string = GISCO_ID,
                                                     pattern = "[[A-Z]][[A-Z]]")) %>% 
      sf::st_transform(crs = 4326)
    readr::write_rds(x = sf,
                     file = rds_file)
    
  }
  
  if (is.null(name)==FALSE) {
    sf <- sf %>% 
      dplyr::filter(LAU_LABEL == name)
    readr::write_rds(x = sf,
                     file = rds_file_location)
  }
  
  return(sf)
}
