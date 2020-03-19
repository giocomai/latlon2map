#' Gets local administrative units from Eurostat's website 
#'
#' @return LAU in sf format
#' @export
#'
#' @examples
#' ll_get_lau_eu()
ll_get_lau_eu <- function(year = 2018) {
  usethis::ui_info(x = "© EuroGeographics for the administrative boundaries")
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
  
  if (fs::file_exists(rds_file)) {
    return(readr::read_rds(rds_file))
  }
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
                   path = rds_file)
  return(sf)
}
