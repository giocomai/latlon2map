#' Extract from zip shape files of roads from previously downloaded
#'
#' @param countries The name of one or more geographic entities from files typically previously downloaded with `ll_osm_download()`
#' @param download_if_missing Logical, defaults to TRUE. If TRUE, downloads country files with `ll_osm_download()` if they are not available locally.
#' @param overwrite Logical, defaults to FALSE. If TRUE, extracts files from zip even if folder already existing.
#' @return Nothing, used for its side effects (extracts shapefiles from country-level zip files)
#' @examples
#' \dontrun{
#' ll_extract_roads(countries = "Romania")
#' }
#'
#' @export
#'

ll_osm_extract_roads <- function(countries,
                                 download_if_missing = TRUE,
                                 overwrite = FALSE) {
  base_folder <- fs::path(
    latlon2map::ll_set_folder(),
    "osm_roads_shp"
  )

  fs::dir_create(path = base_folder, recurse = TRUE)

  fs::dir_create(path = fs::path(
    latlon2map::ll_set_folder(),
    "osm_roads_shp"
  ))

  purrr::walk(
    .x = tolower(countries),
    .f = function(current_country) {
      current_country_zip_folder <- fs::path(
        latlon2map::ll_set_folder(),
        "osm_countries_shp_zip",
        current_country
      )
      if (fs::file_exists(current_country_zip_folder) == FALSE) {
        if (download_if_missing == TRUE) {
          usethis::ui_info(glue::glue("'{current_country}' is not available locally. It will now be downloaded."))
          ll_osm_download(countries = current_country)
          ll_osm_extract_roads(
            countries = current_country,
            download_if_missing = FALSE,
            overwrite = FALSE
          )
        } else {
          usethis::ui_info(glue::glue("'{current_country}' is not available locally. You can download it with 'll_osm_download('{current_country}')'."))
          usethis::ui_stop(glue::glue("{current_country} not available."))
        }
      } else {
        local_files <- fs::dir_ls(
          path = current_country_zip_folder,
          recurse = FALSE,
          type = "file",
          glob = "*.shp.zip"
        )



        purrr::walk(
          .x = local_files,
          .f = function(current_zip_file) {
            files_to_extract <- unzip(
              zipfile = current_zip_file,
              list = TRUE
            ) %>%
              tibble::as_tibble() %>%
              dplyr::filter(stringr::str_detect(string = Name, pattern = "roads")) %>%
              dplyr::pull(Name)

            current_street_shp_folder <-
              fs::path(
                latlon2map::ll_set_folder(),
                "osm_roads_shp",
                current_country,
                current_zip_file %>%
                  fs::path_file() %>%
                  stringr::str_remove(pattern = "-latest-free.shp.zip")
              )

            if (fs::file_exists(path = current_street_shp_folder) == FALSE | overwrite == TRUE) {
              unzip(
                zipfile = current_zip_file,
                files = files_to_extract,
                exdir = current_street_shp_folder
              )
            }
          }
        )
      }
    }
  )
}

#' Extract shape files of roads from previously downloaded
#'
#' @param country The name of one or more geographic entities from files typically previously downloaded with `ll_osm_download()`
#' @param silent Defaults to FALSE. If TRUE, hides copyright notice. Useful e.g. when using this in reports or in loops. The copyright notice must still be shown where the final output is used.
#' @return All roads in a country by OpenStreetMap.
#' @examples
#' \dontrun{
#' ll_osm_get_roads(country = "Romania")
#' }
#'
#' @export
#'

ll_osm_get_roads <- function(country,
                             silent = FALSE) {
  if (silent == FALSE) {
    usethis::ui_info(x = "© OpenStreetMap contributors")
  }
  country <- stringr::str_to_lower(country)

  country_street_shp_folder <-
    fs::path(
      latlon2map::ll_set_folder(),
      "osm_roads_shp",
      country
    )

  if (fs::file_exists(country_street_shp_folder) == FALSE) {
    ll_osm_extract_roads(countries = country)
  }

  street_folders <- fs::dir_ls(
    path = country_street_shp_folder,
    type = "directory",
    recurse = FALSE
  )

  purrr::map_dfr(
    .x = street_folders,
    .f = function(x) sf::st_read(dsn = x)
  )
}
