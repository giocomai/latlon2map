#' Gets local administrative units from Eurostat's website 
#'
#' Source: https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/lau#lau18
#'
#' @param name Name of the local administrative unit in the local language. Use gisco_id whenever possible, as names of local administrative units are not unique, e.g. there are 11 "Neuenkirchen" in the dataset. If both `name` and `gisco_id` are NULL, then it returns all municipalities.
#' @param gisco_id Gisco identifier of the relevant administrative unit. If given, takes precedence over name. 
#' @param year Year of mapping, defaults to most recent (2019). Availalble values: 2016, 2017, 2018, 2019.
#' @param lau_sf sf object, exactly such as the one that would be returned by `ll_get_lau_eu()`. Used to speed-up computation when bulk processing.
#' @param silent Defaults to FALSE. If TRUE, hides copyright notice. Useful e.g. when using this in reports or in loops. The copyright notice must still be shown where the final output is used.
#'
#' @return European LAU in sf format
#' @export
#'
#' @examples
#' 
#' ll_set_folder("~/R/")
#' ll_get_lau_eu()
#' 
ll_get_lau_eu <- function(gisco_id = NULL, 
                          name = NULL,
                          year = 2019,
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
  
  if (is.null(gisco_id)==FALSE) {
    rds_file_location <- ll_find_file(geo = "eu",
                                      level = "lau",
                                      resolution = "1m",
                                      year = year,
                                      name = gisco_id,
                                      file_type = "rds")
    
    if (fs::file_exists(rds_file_location)) {
      return(readr::read_rds(file = rds_file_location))
    }
  } else if (is.null(name)==FALSE) {
    rds_file_location <- ll_find_file(geo = "eu",
                                      level = "lau",
                                      resolution = "1m",
                                      year = year,
                                      name = paste0("lau-",
                                                    stringr::str_replace(string = name,
                                                                         pattern = "[[:punct:]]",
                                                                         replacement = "_")),
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
    source_url <- paste0("https://gisco-services.ec.europa.eu/distribution/v2/lau/download/ref-lau-", year, "-01m.shp.zip")
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
    zip_folder <- ll_find_file(geo = "eu",
                               level = "lau",
                               resolution = "1m",
                               year = year,
                               name = "abl",
                               file_type = "zip") %>% 
      fs::path_dir()
    
    unzip(zipfile = zip_file,
          exdir = zip_folder)
    
    unzip(zipfile = fs::path(zip_folder, paste0("LAU_RG_01M_", year, "_4326.shp.zip")),
          exdir = shp_folder)
    
    
    sf <- sf::read_sf(shp_folder) %>% 
      dplyr::mutate(CNTR_CODE = stringr::str_extract(string = GISCO_ID,
                                                     pattern = "[[A-Z]][[A-Z]]")) %>% 
      sf::st_transform(crs = 4326)
    saveRDS(object = sf,
            file = rds_file)
    
  }
  
  if (is.null(gisco_id)==FALSE) {
    sf <- sf %>% 
      dplyr::filter(GISCO_ID == gisco_id)
    saveRDS(object = sf,
            file = rds_file_location)
  } else if (is.null(name)==FALSE) {
    if (is.element("LAU_LABEL", colnames(sf))==TRUE) {
      sf <- sf %>% 
        dplyr::filter(LAU_LABEL == name)
    } else {
      sf <- sf %>% 
        dplyr::filter(LAU_NAME == name)
    }
    if (nrow(sf)>1) {
      usethis::ui_stop(x = "More than one local administrative unit with this name. Use gisco_id instead.")
    }
    saveRDS(object = sf,
            file = rds_file_location)
  }
  return(sf)
}


#' Get all streets available in OpenStreetMap located in given local administrative unit.
#' 
#' Relies on the output of `ll_get_lau_eu()` for the boundaries of local administrative units.
#' 
#' @param gisco_id Gisco identifier.
#' @param unnamed_streets Defaults to TRUE. If FALSE, it drops all streets with missing "name" or missing "fclass".
#'
#' @return
#' @export
#'
#' @examples
#' 
#' \dontrun{
#' ll_osm_lau_streets(gisco_id  = "IT_022205", unnamed_streets = FALSE)
#' }
ll_osm_lau_streets <- function(gisco_id,
                               unnamed_streets = TRUE,
                               year = 2019) {
  
  gisco_cc <- stringr::str_extract(string = gisco_id,
                                       pattern = "[A-Z][A-Z]") %>% 
    stringr::str_to_upper()
  
  city_code <- stringr::str_extract(string = gisco_id,
                                    pattern = "[[:digit:]]+")
  
  
  if (unnamed_streets==TRUE) {
    ll_create_folders(geo = "eu",
                      level = "lau_osm_streets",
                      resolution = "1m",
                      year = year)
    rds_file_location <- ll_find_file(geo = "eu",
                                      level = "lau_osm_streets",
                                      resolution = "1m",
                                      year = year,
                                      name = gisco_id,
                                      file_type = "rds")
  } else {
    ll_create_folders(geo = "eu",
                      level = "lau_osm_streets_no_NA",
                      resolution = "1m",
                      year = year)
    rds_file_location <- ll_find_file(geo = "eu",
                                      level = "lau_osm_streets_no_NA",
                                      resolution = "1m",
                                      year = year,
                                      name = gisco_id,
                                      file_type = "rds")
  }
  
  
  if (fs::file_exists(rds_file_location)) {
    return(readr::read_rds(file = rds_file_location))
  }
  # TODO country_full_name will not always match with OSM/Geofabrik
  if (is.element(gisco_cc,ll_osm_bboxes$country_code)) {
    country_full_name <- ll_osm_bboxes %>% 
      dplyr::filter(country_code == gisco_cc) %>% 
      dplyr::distinct(country) %>% 
      dplyr::pull(country)
    
    bboxes_available <- TRUE
  } else {
    country_full_name <- countrycode::codelist %>% 
      dplyr::filter(iso2c == gisco_cc) %>% 
      dplyr::pull(iso.name.en)
    bboxes_available <- FALSE
  }

  if (bboxes_available) {
    current_country_bboxes <- ll_osm_bboxes %>% 
      dplyr::filter(country_code==gisco_cc) 
    
    regions_to_load <- current_country_bboxes %>% 
      dplyr::filter(purrr::map_lgl(.x = current_country_bboxes$bbox,
                                       .f = function(current_bbox) {
                                         bbox_to_check$xmin < current_bbox$xmax & current_bbox$xmin < bbox_to_check$xmax & bbox_to_check$ymin <  current_bbox$ymax & current_bbox$ymin < bbox_to_check$ymax     
                                       }))
    ll_osm_extract_roads(countries = country_full_name)

    street_folders <- fs::path(latlon2map::ll_set_folder(),
               "osm_roads_shp",
               country_full_name,
               regions_to_load$region)
    city_roads_pre <- purrr::map_dfr(.x = street_folders,
                                     .f = function(x) sf::st_read(dsn = x))
  } else {
    city_roads_pre <- ll_osm_get_roads(country = country_full_name) 
  }
  
  if (unnamed_streets==TRUE) {
    city_roads <- city_roads_pre %>% 
      sf::st_intersection(ll_get_lau_eu(gisco_id = gisco_id,
                                        year = year))
  } else {
    city_roads <- city_roads_pre %>%
      dplyr::filter(is.na(name)==FALSE, is.na(fclass)==FALSE) %>% 
      sf::st_intersection(ll_get_lau_eu(gisco_id = gisco_id,
                                        year = year)) %>% 
      dplyr::group_by(name) %>%
      dplyr::summarise() %>% 
      dplyr::ungroup()
  }
  
  saveRDS(object = city_roads,
          file = rds_file_location)
  
 city_roads
  
}