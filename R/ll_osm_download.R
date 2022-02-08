#' Download OSM data for whole countries from Geofabrik.
#'
#' N.B. Names do not always correspond to official name of countries and may include different geographic entities.
#' For a full list of available "countries" as made available by Geofabrik, see the internal dataset `ll_osm_countries`.
#' Be considered in downloading files.
#'
#' @param countries One or more country names. For details on available country names see the dataset included in this package: `ll_osm_countries`
#' @param overwrite Logical, defaults to FALSE. If true, downloads new files even if already present.
#' @param wget Logical, defaults to FALSE. If TRUE, it downloads files with wget (if available), otherwise uses default method. Setting wget to TRUE may contribute to prevent download timeouts; notice that apparent freeze of the download progress in the console are common, and mostly the download is just continuing in the background (for reference, check file size in folder.)
#' @return Used only for its side effects (downloads osm data).
#' @examples
#' \dontrun{
#' ll_osm_download(countries = "Romania")
#' ll_osm_download(countries = c("chile", "colombia"))
#' }
#' @export
ll_osm_download <- function(countries,
                            overwrite = FALSE,
                            wget = FALSE) {
  countries_available_l <- is.element(stringr::str_to_lower(countries), ll_osm_countries$country)
  if (Reduce(x = countries_available_l, f = `&`) == FALSE) {
    missing_countries <- glue::glue_collapse(x = countries[!countries_available_l], sep = ", ", last = ", and ")
    usethis::ui_oops("The following countries are not available: {missing_countries}")
    usethis::ui_info("See the internal dataset `ll_osm_countries` for a list of available countries and geographic entities")
    usethis::ui_stop("Please input an accepted geographic entity name")
  }

  downloads_df <- tibble::tibble(country = tolower(countries)) %>%
    dplyr::left_join(y = ll_osm_countries, by = "country") %>%
    tidyr::unnest(link)

  base_folder <- fs::path(latlon2map::ll_set_folder(), "osm_countries_shp_zip")
  fs::dir_create(path = base_folder, recurse = TRUE)

  purrr::pwalk(
    .l = downloads_df,
    .f = function(country, continent, link) {
      country_folder <- fs::path(
        base_folder,
        country
      )
      local_file <- fs::path(country_folder, fs::path_file(link))
      if (fs::file_exists(local_file) == FALSE | overwrite == TRUE) {
        fs::dir_create(country_folder)
        usethis::ui_info(x = "If the download is not successful, please download manually - {usethis::ui_path(link)} - and store in this location: {usethis::ui_path(local_file)}")
        if (wget == TRUE) {
          download.file(url = link, destfile = local_file, method = "wget")
        } else {
          download.file(url = link, destfile = local_file)
        }
      }
    }
  )
}




#' Download OSM data in geopackage format for regions, provinces, and municipalities in Italy.
#'
#' See `ll_osm_it_gpkg` for all available files.
#'
#' @param level One of "regioni", "provincie", "comuni". Defaults to "comuni".
#' @param name Name of geographic entity. Check `ll_osm_it_gpkg` or `ll_get_nuts_it()` for valid names.
#' @param code Used in alternative to name. Check `ll_osm_it_gpkg` or `ll_get_nuts_it()` for valid values.
#' @param wget Logical, defaults to FALSE. If TRUE, it downloads files with wget (if available), otherwise uses default method. Setting wget to TRUE may contribute to prevent download timeouts; notice that apparent freeze of the download progress in the console are common, and mostly the download is just continuing in the background (for reference, check file size in folder.)
#' @param quiet Logical, defaults to FALSE. If TRUE no messages about download advancement are printed.
#' @return Used only for its side effects (downloads osm data).
#' @examples
#' \dontrun{
#' ll_osm_download_it(level = "comuni", name = "Trento")
#' }
#' @export
ll_osm_download_it <- function(level = "comuni",
                               name = NULL,
                               code = NULL,
                               overwrite = FALSE,
                               wget = FALSE,
                               quiet = FALSE) {
  if (is.null(name) == FALSE) {
    available_l <- is.element(stringr::str_to_lower(name), stringr::str_to_lower(ll_osm_it_gpkg[[level]]$name))
    if (Reduce(x = available_l, f = `&`) == FALSE) {
      missing_names <- glue::glue_collapse(
        x = name[!available_l],
        sep = ", ",
        last = ", and "
      )
      if (quiet == FALSE) {
        usethis::ui_oops("The following places are not available: {missing_names}")
        usethis::ui_info("See the internal dataset `ll_osm_it_gpkg` for a list of available places")
        usethis::ui_stop("Please input an accepted geographic entity name")
      }
    }

    downloads_df <- tibble::tibble(
      name = stringr::str_to_lower(name),
      level = stringr::str_to_lower(level)
    ) %>%
      dplyr::left_join(
        y = ll_osm_it_gpkg[[level]] %>%
          dplyr::mutate(name = stringr::str_to_lower(name)),
        by = "name"
      )
  } else if (is.null(code) == FALSE) {
    if (is.null(code) == FALSE) {
      available_l <- is.element(as.numeric(code), as.numeric(ll_osm_it_gpkg[[level]]$code))
      if (Reduce(x = available_l, f = `&`) == FALSE) {
        missing_codes <- glue::glue_collapse(
          x = code[!available_l],
          sep = ", ",
          last = ", and "
        )
        if (quiet == FALSE) {
          usethis::ui_oops("The following places are not available: {missing_codes}")
          usethis::ui_info("See the internal dataset `ll_osm_it_gpkg` for a list of available places")
          usethis::ui_stop("Please input an accepted geographic entity name")
        }
      }
      downloads_df <- tibble::tibble(
        code = code,
        level = stringr::str_to_lower(level)
      ) %>%
        dplyr::left_join(
          y = ll_osm_it_gpkg[[level]],
          by = "code"
        )
    }
  }



  base_folder <- fs::path(
    latlon2map::ll_set_folder(),
    "osm_it_gpkg",
    stringr::str_to_lower(level)
  )
  fs::dir_create(path = base_folder, recurse = TRUE)

  purrr::pwalk(
    .l = downloads_df,
    .f = function(name, code, level, link) {
      local_file <- fs::path(base_folder, fs::path_file(link))
      if (fs::file_exists(local_file) == FALSE | overwrite == TRUE) {
        fs::dir_create(base_folder)
        usethis::ui_info(x = "If the download is not successful, please download manually - {usethis::ui_path(link)} - and store in this location: {usethis::ui_path(local_file)}")
        if (wget == TRUE) {
          download.file(
            url = link,
            destfile = local_file,
            method = "wget",
            quiet = quiet
          )
        } else {
          download.file(
            url = link,
            destfile = local_file,
            quiet = quiet
          )
        }
      }
    }
  )
}


#' Extract OSM data for regions, provinces, and municipalities in Italy.
#'
#' See `ll_osm_it_gpkg` for all available files.
#'
#' @param level One of "regioni", "provincie", "comuni". Defaults to "comuni".
#' @param name Name of geographic entity. Check `ll_osm_it_gpkg` or `ll_get_nuts_it()` for valid names.
#' @param code Used in alternative to name. Check `ll_osm_it_gpkg` or `ll_get_nuts_it()` for valid values.
#' @param layer Defaults to "lines". Must be one of "points", "lines", "multilinestrings", "multipolygons", or "other_relations"
#' @param quiet Logical, defaults to FALSE. If TRUE, supresses messages generated when reading the geopackage file.
#'
#' @return An sf object.
#' @export
#'
#' @examples
#' \dontrun{
#' ll_osm_extract_it(level = "comuni", name = "Trento")
#' }
#'
ll_osm_extract_it <- function(level = "comuni",
                              name = NULL,
                              code = NULL,
                              layer = "lines",
                              quiet = FALSE) {
  ll_osm_download_it(
    level = level,
    name = name,
    code = code,
    quiet = quiet
  )

  base_folder <- fs::path(
    latlon2map::ll_set_folder(),
    "osm_it_gpkg",
    stringr::str_to_lower(level)
  )

  available_files <- fs::dir_ls(
    path = base_folder,
    recurse = FALSE,
    type = "file",
    glob = "*.gpkg"
  ) %>%
    tibble::enframe(
      name = NULL,
      value = "local_files"
    ) %>%
    dplyr::mutate(filename = fs::path_file(local_files)) %>%
    dplyr::mutate(code = stringr::str_extract(
      string = filename,
      pattern = "[[:digit:]]+"
    )) %>%
    dplyr::left_join(
      y = ll_osm_it_gpkg[[level]],
      by = "code"
    ) %>%
    dplyr::mutate(
      name = stringr::str_to_lower(name),
      code = as.numeric(code)
    )


  if (is.null(name) == FALSE) {
    selected_files <- tibble::tibble(name = stringr::str_to_lower(name)) %>%
      dplyr::left_join(y = available_files, by = "name")
  } else if (is.null(code) == FALSE) {
    selected_files <- tibble::tibble(code = as.numeric(code)) %>%
      dplyr::left_join(y = available_files, by = "code")
  }

  extracted_sf <- sf::st_read(
    dsn = selected_files[["local_files"]],
    layer = layer,
    quiet = quiet
  )

  extracted_sf
}
