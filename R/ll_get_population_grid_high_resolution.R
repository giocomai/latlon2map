#' Get High Resolution Population Density Maps + Demographic Estimates
#'
#' Source: https://data.humdata.org/organization/facebook
#' Details on methodology: https://dataforgood.fb.com/docs/methodology-high-resolution-population-density-maps-demographic-estimates/
#'
#' @param geo A twe letter country code, such as "IT" for Italy and "DE" for Germany
#' @param match_sf An sf object to me matched with the population grid. If not given, full grid is returned.
#' @param match_name A name to be used for local caching. It is the responsibility of the user to keept it consistent. If not given, data are not cached locally.
#' @param source_url A direct link to the zipped version of the csv file in the original database, if automatic download with the country code does not work. For example, for Italy this would be "https://data.humdata.org/dataset/0eb77b21-06be-42c8-9245-2edaff79952f/resource/1e96f272-7d86-4108-b4ca-5a951a8b11a0/download/population_ita_2019-07-01.csv.zip"
#' @param file_format Defaults to "CSV". Other available formats include "GeoTIFF", "JSON", "zip", "GDAL Virtual Format". Currently only CSV supported.
#' @param dataset Defaults to "population". Beginning of the name of the dataset. For alternatives, see e.g. `population_grid_hr_metadata %>% dplyr::filter(country_code=="IT") %>% dplyr::distinct(name)`. Currently only tested with default value.
#' @param join
#' @param silent
#'
#' @return
#' @export
#'
#' @examples
ll_get_population_grid_hr <- function(geo,
                                      match_sf = NULL,
                                      match_name = NULL,
                                      join = sf::st_intersects,
                                      file_format = "CSV",
                                      dataset = "population|general", 
                                      source_url = NULL,
                                      silent = FALSE) {
  if (silent == FALSE) {
    usethis::ui_info(x = paste("Facebook Connectivity Lab and Center for International Earth Science Information Network - CIESIN - Columbia University. 2016. High Resolution Settlement Layer (HRSL). Source imagery for HRSL © 2016 DigitalGlobe. Accessed", Sys.Date()))
    usethis::ui_info(x = "License: Creative Commons Attribution International")
    usethis::ui_info(x = "Source: https://data.humdata.org/organization/facebook")
  }

  if (is.null(geo) == FALSE) {
    geo <- stringr::str_to_upper(string = geo)
  }

  if (is.null(source_url)==FALSE) {
    source_url <- latlon2map::population_grid_hr_metadata %>% 
      dplyr::filter(.data$format == file_format, country_code==geo)  %>% 
      dplyr::filter(stringr::str_detect(string = name, pattern = dataset)) %>%
      dplyr::distinct() %>% 
      dplyr::pull(download_url)
  }
  
  if (is.null(match_name) == FALSE) {
    rds_file_location <- ll_find_file(
      geo = geo,
      level = 0,
      resolution = "hr",
      year = 2020,
      name = paste0(match_name, "-hr_population_grid", "-", "geo"),
      file_type = "rds"
    )

    if (fs::file_exists(rds_file_location)) {
      return(readr::read_rds(file = rds_file_location))
    }
  }

  ll_create_folders(
    geo = geo,
    level = 0,
    resolution = "hr",
    year = 2020,
    file_type = c("zip", "csv", "rds")
  )

  rds_file <- ll_find_file(
    geo = geo,
    level = 0,
    resolution = "hr",
    year = 2020,
    name = paste0("population_grid_hr", "-", geo),
    file_type = "rds"
  )

  if (fs::file_exists(rds_file) & is.null(match_sf)) {
    sf <- readr::read_rds(file = rds_file)
  } else {
    csv_folder <- ll_find_file(
      geo = geo,
      level = 0,
      resolution = "hr",
      year = 2020,
      name = paste0("population_grid", "-", geo),
      file_type = "csv"
    ) %>% fs::path_dir()

    zip_file <- ll_find_file(
      geo = geo,
      level = 0,
      resolution = "hr",
      year = 2020,
      name = paste0("population_grid", "-", geo),
      file_type = "zip"
    )

    if (fs::file_exists(zip_file) == FALSE) {
      download.file(
        url = source_url,
        destfile = zip_file
      )
    }

    file_name <- stringr::str_split(source_url, "/") %>%
      unlist() %>%
      dplyr::last() %>%
      stringr::str_replace("_csv\\.zip$|\\.csv\\.zip$", ".csv") %>%
      stringr::str_to_lower()

    if (fs::file_exists(fs::path(csv_folder, file_name)) == FALSE) {
      unzip(
        zipfile = zip_file,
        exdir = csv_folder
      )
      fs::dir_walk(
        path = csv_folder,
        fun = function(x) {
          fs::file_move(
            path = x,
            new_path = fs::path(
              fs::path_dir(x),
              stringr::str_to_lower(fs::path_file(x))
            )
          )
        }
      )
    }
    df <- readr::read_csv(
      file = fs::path(csv_folder, file_name),
      col_names = c("Lat", "Lon", "Population"),
      col_types = readr::cols(
        Lat = readr::col_double(),
        Lon = readr::col_double(),
        Population = readr::col_double()
      ), skip = 1
    ) %>% 
      dplyr::filter(is.na(.data$Lat)==FALSE, is.na(.data$Population)==FALSE)
      #dplyr::filter(.data$Population>0)
    
    if (is.null(match_sf) == FALSE) {
      bbox <- sf::st_bbox(match_sf)
      df <- df %>%
        dplyr::filter(Lat >= bbox$ymin, Lat <= bbox$ymax, Lon >= bbox$xmin, Lat <= bbox$ymax)

      sf <- df %>%
        sf::st_as_sf(coords = c("Lon", "Lat"), crs = 4326)

      sf <- sf::st_filter(
        x = sf %>% sf::st_transform(crs = 3857),
        y = match_sf %>% sf::st_transform(crs = 3857),
        join = join
      ) %>%
        sf::st_transform(crs = 4326)

      if (is.null(match_name) == FALSE) {
        saveRDS(
          object = sf,
          file = rds_file_location
        )
      }

      return(sf)
    }
    sf <- df %>%
      sf::st_as_sf(coords = c("Lon", "Lat"),
                   crs = 4326)

    saveRDS(
      object = sf,
      file = rds_file
    )
  }

  return(sf)
}
