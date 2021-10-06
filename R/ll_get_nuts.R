#' Get administrative boundaries
#'
#' Source: https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
#'
#' @param geo Three letter country codes 
#' @param level Defaults to 0. Available labels, depending on data availability for the specific country, between 0 and 3. 
#' @param year Defaults to "".
#'
#' @return
#' @export
#'
#' @examples
#' ll_get_nuts(geo = "BIH", level = 0)
ll_get_gadm <- function(geo,
                        level = 0) {
  usethis::ui_info("Source_ https://gadm.org/")
  usethis::ui_info("The data are freely available for academic use and other non-commercial use. Redistribution, or commercial use, is not allowed without prior permission. Using the data to create maps for academic publishing is allowed.")
  
  geo <- stringr::str_to_upper(geo)
  
  year <- "3_6"# version
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
  
  source_url <- stringr::str_c("https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_", 
                               geo, 
                               "_",
                               level, 
                               "_sf.rds")
  
  if (fs::file_exists(rds_file)==FALSE) {
    download.file(
      url = source_url,
      destfile = rds_file
    )   
  }
  sf <- readr::read_rds(file = rds_file) %>% 
    sf::st_as_sf() %>% 
    sf::st_transform(crs = 4326) 
    
  saveRDS(object = sf, file = rds_file)
  
  return(sf)
         
}
