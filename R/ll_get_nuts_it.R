#' Regions and provinces in Italy (high detail, CC-BY Istat)
#' 
#' 2019 / WGS84 UTM32N
#' 
#' @param level Defaults to "2", i.e. regioni. Available: "3" (i.e. province), and "lau", local administrative units.
#' 
#' @return
#' @export
#'
#' @examples
#' ll_get_nuts_it()
ll_get_nuts_it <- function(level = 2,
                           year = 2019,
                           resolution = "low") {
  usethis::ui_info(x = "Source: https://www.istat.it/it/archivio/222527")
  usethis::ui_info(x = "Istat (CC-BY)")
  
  rds_file <- ll_find_file(geo = "it",
                           level = level,
                           resolution = resolution,
                           year = year,
                           name = "abl",
                           file_type = "rds")
  
  if (fs::file_exists(rds_file)) {
    return(readr::read_rds(path = rds_file))
  }
  
  ll_create_folders(geo = "it",
                    level = level,
                    resolution = "low",
                    year = year)
  ll_create_folders(geo = "it",
                    level = "all_levels",
                    resolution = "low",
                    year = year)
  
  shp_folder <- ll_find_file(geo = "it",
                             level = "all_levels",
                             resolution = resolution,
                             year = year,
                             name = "abl",
                             file_type = "shp")
  
  type <- dplyr::if_else(condition = resolution == "high",
                         true = "non_generalizzati",
                         false = "generalizzati",
                         missing = "non_generalizzati")
  
  g_name <- dplyr::if_else(condition = resolution == "high",
                           true = "",
                           false = "_g",
                           missing = "")
  
  source_url <- paste0("http://www.istat.it/storage/cartografia/confini_amministrativi/", type, "/Limiti0101", year, g_name, ".zip")
  
  zip_file <- ll_find_file(geo = "it",
                           level = "all_levels",
                           resolution = resolution,
                           year = year,
                           name = "abl",
                           file_type = "zip")
  
  
  if (fs::file_exists(zip_file)==FALSE) {
    download.file(url = source_url, destfile = zip_file)
  }
  
  unzip(zipfile = zip_file, exdir = shp_folder)
  
  
  if (level == "lau") {
    sf <- sf::read_sf(fs::path(shp_folder,
                               paste0("Limiti01012019", g_name),
                               paste0("Com01012019", g_name)))
  } else if (level == 1) {
    sf <- sf::read_sf(fs::path(shp_folder,
                               paste0("Limiti01012019", g_name),
                               paste0("RipGeo01012019", g_name)))
  } else if (level == 2) {
    sf <- sf::read_sf(fs::path(shp_folder,
                               paste0("Limiti01012019", g_name),
                               paste0("Reg01012019", g_name)))
  } else if (level == 3) {
    sf <- sf::read_sf(fs::path(shp_folder,
                               paste0("Limiti01012019", g_name),
                               paste0("ProvCM01012019", g_name)))
  }
  readr::write_rds(x = sf %>% 
                     sf::st_transform(crs = 4326), path = rds_file)
  return(sf %>% 
           sf::st_transform(crs = 4326))
}