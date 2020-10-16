#' Get countries as an sf object
#'
#' @param resolution Defaults to "60", for 1:60 Million. Available values: are 20, 10, 3, 1 (1 is highest quality available)-
#' @param year Defaults to 2016. Available values: 2016, 2013, 2010, 2006, 2001
#'
#' @return
#' @export
#'
#' @examples
ll_get_world <- function(resolution = "60",
                         year = 2016, 
                         name = NULL) {
  resolution <- stringr::str_pad(string = resolution, width = 2, side = "left", pad = 0)
  usethis::ui_info("Source: https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/countries")
  usethis::ui_info("© EuroGeographics for the administrative boundaries")
  ll_create_folders(geo = "world",
                    level = "country",
                    resolution = resolution,
                    year = year)
  
  rds_file <- ll_find_file(geo = "world",
                           level = "country",
                           resolution = resolution,
                           year = year,
                           name = "abl",
                           file_type = "rds")
  
  if (fs::file_exists(rds_file)) {
    return(readr::read_rds(file = rds_file))
  }
  
  shp_folder <- ll_find_file(geo = "world",
                             level = "country",
                             resolution = resolution,
                             year = year,
                             name = "abl",
                             file_type = "shp")
  if (fs::file_exists(fs::path(shp_folder, paste0("CNTR_RG_", resolution, "M_", year, "_4326.shp")))==FALSE) {
    
    zip_file <- ll_find_file(geo = "world",
                             level = "country",
                             resolution = resolution,
                             year = year,
                             name = "abl",
                             file_type = "zip")
    source_url <- paste0("https://ec.europa.eu/eurostat/cache/GISCO/distribution/v2/countries/download/ref-countries-", year, "-", resolution, "m.shp.zip")
    
    if (fs::file_exists(zip_file)==FALSE) {
      download.file(url = source_url,
                    destfile = zip_file)
    }
    unzip(zipfile = zip_file,
          exdir = shp_folder)
    unzip(zipfile = fs::path(shp_folder, paste0("CNTR_RG_", resolution, "M_", year, "_4326.shp.zip")),
          exdir = shp_folder)
    
  }
  sf <- sf::read_sf(fs::path(shp_folder, paste0("CNTR_RG_", resolution, "M_", year, "_4326.shp"))) %>% 
    sf::st_transform(crs = 4326)
  readr::write_rds(x = sf,
                   file = rds_file)
  return(sf)
}