#' Gets local administrative units from Eurostat's website
#'
#' Source: https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/lau#lau18
#'
#' @param name Name of the local administrative unit in the local language. Use gisco_id whenever possible, as names of local administrative units are not unique, e.g. there are 11 "Neuenkirchen" in the dataset. If both `name` and `gisco_id` are NULL, then it returns all municipalities.
#' @param gisco_id Gisco identifier of the relevant administrative unit. If given, takes precedence over name.
#' @param year Year of mapping, defaults to most recent (2020). Available starting with 2011.
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
ll_get_lau_eu <- function(gisco_id = NULL,
                          name = NULL,
                          year = 2020,
                          silent = FALSE,
                          lau_sf = NULL) {
  if (silent == FALSE) {
    usethis::ui_info(x = "© EuroGeographics for the administrative boundaries")
  }
  ll_create_folders(
    geo = "eu",
    level = "lau",
    resolution = "1m",
    year = year
  )

  rds_file <- ll_find_file(
    geo = "eu",
    level = "lau",
    resolution = "1m",
    year = year,
    name = "abl",
    file_type = "rds"
  )

  if (is.null(gisco_id) == FALSE) {
    rds_file_location <- ll_find_file(
      geo = "eu",
      level = "lau",
      resolution = "1m",
      year = year,
      name = gisco_id,
      file_type = "rds"
    )

    if (fs::file_exists(rds_file_location)) {
      return(readr::read_rds(file = rds_file_location))
    }
  } else if (is.null(name) == FALSE) {
    rds_file_location <- ll_find_file(
      geo = "eu",
      level = "lau",
      resolution = "1m",
      year = year,
      name = paste0(
        "lau-",
        stringr::str_replace(
          string = name,
          pattern = "[[:punct:]]",
          replacement = "_"
        )
      ),
      file_type = "rds"
    )

    if (fs::file_exists(rds_file_location)) {
      return(readr::read_rds(file = rds_file_location))
    }
  }

  if (is.null(lau_sf) == FALSE) {
    sf <- lau_sf
  } else if (fs::file_exists(rds_file)) {
    sf <- readr::read_rds(file = rds_file)
  } else {
    shp_folder <- ll_find_file(
      geo = "eu",
      level = "lau",
      resolution = "1m",
      year = year,
      name = "abl",
      file_type = "shp"
    )
    source_url <- paste0("https://gisco-services.ec.europa.eu/distribution/v2/lau/download/ref-lau-", year, "-01m.shp.zip")
    zip_file <- ll_find_file(
      geo = "eu",
      level = "lau",
      resolution = "1m",
      year = year,
      name = "abl",
      file_type = "zip"
    )

    if (fs::file_exists(zip_file) == FALSE) {
      download.file(
        url = source_url,
        destfile = zip_file
      )
    }
    zip_folder <- ll_find_file(
      geo = "eu",
      level = "lau",
      resolution = "1m",
      year = year,
      name = "abl",
      file_type = "zip"
    ) %>%
      fs::path_dir()

    unzip(
      zipfile = zip_file,
      exdir = zip_folder
    )

    unzip(
      zipfile = fs::path(zip_folder, paste0("LAU_RG_01M_", year, "_4326.shp.zip")),
      exdir = shp_folder
    )


    sf <- sf::read_sf(shp_folder) %>%
      dplyr::mutate(CNTR_CODE = stringr::str_extract(
        string = GISCO_ID,
        pattern = "[[A-Z]][[A-Z]]"
      )) %>%
      sf::st_transform(crs = 4326)
    saveRDS(
      object = sf,
      file = rds_file
    )
  }

  if (is.null(gisco_id) == FALSE) {
    sf <- sf %>%
      dplyr::filter(GISCO_ID == gisco_id)
    saveRDS(
      object = sf,
      file = rds_file_location
    )
  } else if (is.null(name) == FALSE) {
    if (is.element("LAU_LABEL", colnames(sf)) == TRUE) {
      sf <- sf %>%
        dplyr::filter(LAU_LABEL == name)
    } else {
      sf <- sf %>%
        dplyr::filter(LAU_NAME == name)
    }
    if (nrow(sf) > 1) {
      usethis::ui_stop(x = "More than one local administrative unit with this name. Use gisco_id instead.")
    }
    saveRDS(
      object = sf,
      file = rds_file_location
    )
  }
  return(sf)
}


#' Get all streets available in OpenStreetMap located in given local
#' administrative unit.
#'
#' Relies on the output of `ll_get_lau_eu()` for the boundaries of local
#' administrative units.
#'
#' @param gisco_id Gisco identifier.
#' @param country Name of country as included in Geofabrik's datasets, does not
#'   always match common country names or geography. For details on available
#'   country names see the dataset included in this package: `ll_osm_countries`
#' @param unnamed_streets Defaults to TRUE. If FALSE, it drops all streets with
#'   missing "name" or missing "fclass".
#' @param lau_boundary_sf Defaults to NULL. If given, used to speed up
#'   processing. Must be an `sf` object such as the ones output by
#' @param streets_sf Defaults to NULL. If given, used to speed up processing.
#'   Must be an `sf` object such as the ones output by `ll_get_lau_eu()`.
#' @param country_code_type Defaults to "eurostat". An alternative common value
#'   is "iso2c". See `countrycode::codelist` for a list of available codes.
#' @param year Year of LAU boundaries, defaults to most recent (2020), passed to
#'   `ll_get_lau_eu()`. Available starting with 2011.
#'
#' @return An `sf` objects with all streets of a given LAU based on
#'   OpenStreetMap
#' @export
#'
#' @examples
#' \dontrun{
#' ll_osm_get_lau_streets(gisco_id = "IT_022205", unnamed_streets = FALSE)
#'
#' # or if country name does not match
#'
#' ll_osm_get_lau_streets(gisco_id = "EL_01020204", country = "greece")
#' }
ll_osm_get_lau_streets <- function(gisco_id,
                                   country = NULL, 
                                   unnamed_streets = TRUE,
                                   lau_boundary_sf = NULL, 
                                   streets_sf = NULL,
                                   country_code_type = "eurostat",
                                   year = 2020) {
  
  if (unnamed_streets == TRUE) {
    ll_create_folders(
      geo = "eu",
      level = "lau_osm_streets",
      resolution = "1m",
      year = year
    )
    rds_file_location <- ll_find_file(
      geo = "eu",
      level = "lau_osm_streets",
      resolution = "1m",
      year = year,
      name = gisco_id,
      file_type = "rds"
    )
  } else {
    ll_create_folders(
      geo = "eu",
      level = "lau_osm_streets_no_NA",
      resolution = "1m",
      year = year
    )
    rds_file_location <- ll_find_file(
      geo = "eu",
      level = "lau_osm_streets_no_NA",
      resolution = "1m",
      year = year,
      name = gisco_id,
      file_type = "rds"
    )
  }
  
  
  if (fs::file_exists(rds_file_location)) {
    return(readr::read_rds(file = rds_file_location))
  }
  
  
  gisco_cc <- stringr::str_extract(
    string = gisco_id,
    pattern = "[A-Z][A-Z]"
  ) %>%
    stringr::str_to_upper()
  
  city_code <- stringr::str_extract(
    string = gisco_id,
    pattern = "[[:digit:]]+"
  )
  
  if (is.null(lau_boundary_sf)==FALSE) {
    current_lau_boundary <- lau_boundary_sf
  } else {
    current_lau_boundary <- ll_get_lau_eu(
      gisco_id = gisco_id,
      year = year
    )  
  }
  
  current_lau_bbox <- sf::st_bbox(current_lau_boundary)
  
  if (is.null(streets_sf)==FALSE) {
    city_roads_pre <- streets_sf
  } else {
    # TODO country_full_name will not always match with OSM/Geofabrik
    
    if (is.element(gisco_cc, ll_osm_bboxes$country_code)) {
      if (is.null(country)==FALSE) {
        country_full_name <- country %>%
          stringr::str_to_lower(country)
        
        bbox_check_df <- ll_osm_bboxes %>%
          dplyr::filter(.data$country == country_full_name)
        
        if (nrow(bbox_check_df)>0) {
          bboxes_available <- TRUE
        } else {
          bboxes_available <- FALSE
        }
        
      } else {
        country_full_name <- ll_osm_bboxes %>%
          dplyr::filter(country_code == gisco_cc) %>%
          dplyr::distinct(country) %>%
          dplyr::pull(country) 
        
        bboxes_available <- TRUE
    }

    } else {
      if (is.null(country)==FALSE) {
        country_full_name <- country
      } else {
        country_full_name <- countrycode::countrycode(sourcevar = gisco_cc,
                                                      origin = country_code_type,
                                                      destination = "iso.name.en")
        
        if (country_full_name=="Czechia") {
          country_full_name <- "czech-republic"
        } else if (stringr::str_detect(string = country_full_name, pattern = " ")) {
          country_full_name <- stringr::str_replace_all(string = country_full_name,
                                                        pattern = " ",
                                                        replacement = "-")
        }
      }
      bboxes_available <- FALSE
    }
    
    if (bboxes_available) {
      current_country_bboxes <- ll_osm_bboxes %>%
        dplyr::filter(country_code == gisco_cc)
      
      regions_to_load <- current_country_bboxes %>%
        dplyr::filter(purrr::map_lgl(
          .x = current_country_bboxes$bbox,
          .f = function(current_bbox) {
            current_lau_bbox$xmin < current_bbox$xmax & current_bbox$xmin < current_lau_bbox$xmax & current_lau_bbox$ymin < current_bbox$ymax & current_bbox$ymin < current_lau_bbox$ymax
          }
        ))
      ll_osm_extract_roads(countries = country_full_name)
      
      street_folders <- fs::path(
        latlon2map::ll_set_folder(),
        "osm_roads_shp",
        country_full_name,
        regions_to_load$region
      )
      city_roads_pre <- purrr::map_dfr(
        .x = street_folders,
        .f = function(x) sf::st_read(dsn = x)
      )
    } else {
      city_roads_pre <- ll_osm_get_roads(country = country_full_name)
    }
  }
  
  if (unnamed_streets == TRUE) {
    city_roads <- city_roads_pre %>%
      sf::st_intersection(current_lau_boundary)
  } else {
    city_roads <- city_roads_pre %>%
      dplyr::filter(is.na(name) == FALSE, is.na(fclass) == FALSE) %>%
      sf::st_intersection(current_lau_boundary) %>%
      dplyr::group_by(name) %>%
      dplyr::summarise() %>%
      dplyr::ungroup()
  }
  
  saveRDS(
    object = city_roads,
    file = rds_file_location
  )
  
  city_roads
}
