#' Regions and provinces in Italy (high detail, CC-BY Istat)
#'
#' 2019 / WGS84 UTM32N
#'
#' @param level Defaults to "2", i.e. regioni. Available: "3" (i.e. province), and "lau", local administrative units.
#' @param year Defaults to 2022 (latest available).
#' @param no_check_certificate Logical, defaults to FALSE. Enable only if certificate issues, and if you are aware of the security implications.
#'
#' @return
#' @export
#'
#' @examples
#' ll_set_folder("~/R/")
#' ll_get_nuts_it()
#' ll_get_nuts_it(name = "Rimini")
ll_get_nuts_it <- function(name = NULL,
                           level = 2,
                           year = 2022,
                           resolution = "low",
                           silent = FALSE, 
                           no_check_certificate = FALSE) {
  if (silent == FALSE) {
    usethis::ui_info(x = "Source: https://www.istat.it/it/archivio/222527")
    usethis::ui_info(x = "Istat (CC-BY)")
  }

  if (is.null(name) == FALSE) {
    rds_file_location <- ll_find_file(
      geo = "it",
      level = level,
      resolution = resolution,
      year = year,
      name = paste0(level, "-", stringr::str_replace_all(string = name, pattern = "[[:punct:]]", replacement = "_")),
      file_type = "rds"
    )

    if (fs::file_exists(rds_file_location)) {
      return(readr::read_rds(file = rds_file_location))
    }
  }


  rds_file <- ll_find_file(
    geo = "it",
    level = level,
    resolution = resolution,
    year = year,
    name = "abl",
    file_type = "rds"
  )

  if (fs::file_exists(rds_file)) {
    sf <- readr::read_rds(file = rds_file)
  } else {
    ll_create_folders(
      geo = "it",
      level = level,
      resolution = resolution,
      year = year
    )
    ll_create_folders(
      geo = "it",
      level = "all_levels",
      resolution = resolution,
      year = year
    )

    shp_folder <- ll_find_file(
      geo = "it",
      level = "all_levels",
      resolution = resolution,
      year = year,
      name = "abl",
      file_type = "shp"
    )

    type <- dplyr::if_else(condition = resolution == "high",
      true = "non_generalizzati",
      false = "generalizzati",
      missing = "non_generalizzati"
    )

    g_name <- dplyr::if_else(condition = resolution == "high",
      true = "",
      false = "_g",
      missing = ""
    )

    source_url <- paste0("https://www.istat.it/storage/cartografia/confini_amministrativi/", type, "/Limiti0101", year, g_name, ".zip")

    zip_file <- ll_find_file(
      geo = "it",
      level = "all_levels",
      resolution = resolution,
      year = year,
      name = "abl",
      file_type = "zip"
    )


    if (fs::file_exists(zip_file) == FALSE) {
      if (isTRUE(no_check_certificate)) {
        download.file(url = source_url, destfile = zip_file, method = "wget", extra = "--no-check-certificate")
      } else {
        download.file(url = source_url, destfile = zip_file) 
      }
    }

    unzip(zipfile = zip_file, exdir = shp_folder)


    if (level == "lau") {
      sf <- sf::read_sf(fs::path(
        shp_folder,
        paste0("Limiti0101", year, g_name),
        paste0("Com0101", year, g_name)
      ))
    } else if (level == 1) {
      sf <- sf::read_sf(fs::path(
        shp_folder,
        paste0("Limiti0101", year, g_name),
        paste0("RipGeo0101", year, g_name)
      ))
    } else if (level == 2) {
      sf <- sf::read_sf(fs::path(
        shp_folder,
        paste0("Limiti0101", year, g_name),
        paste0("Reg0101", year, g_name)
      ))
    } else if (level == 3) {
      sf <- sf::read_sf(fs::path(
        shp_folder,
        paste0("Limiti0101", year, g_name),
        paste0("ProvCM0101", year, g_name)
      ))
    }
    sf <- sf %>%
      sf::st_transform(crs = 4326)

    readr::write_rds(x = sf, file = rds_file)
  }

  if (is.null(name) == FALSE) {
    if (level == "lau") {
      sf <- sf %>%
        dplyr::filter(COMUNE == name)
    } else if (level == 1) {
      sf <- sf %>%
        dplyr::filter(DEN_RIP == name)
    } else if (level == 2) {
      sf <- sf %>%
        dplyr::filter(DEN_REG == name)
    } else if (level == 3) {
      sf <- sf %>%
        dplyr::filter(DEN_PROV == name)
    }

    readr::write_rds(
      x = sf,
      file = rds_file_location
    )
  }
  return(sf)
}
