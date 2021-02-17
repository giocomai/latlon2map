#' Countries and geographic entities for which shapefiles are made availabile by Geofabrik
#'
#' A dataset with all names of countries, continents, as included in the Geofabrik database.
#' They are used to download files with `ll_osm_download()`
#'
#' Links to shapefiles are stored as tibbles. Unnest to see them, e.g.
#' `ll_osm_countries %>% tidyr::unnest(link)`
#' or for a single country:
#' `ll_osm_countries %>% dplyr::filter(country == "italy") %>% tidyr::unnest(link)`
#'
#' @format A tibble
#' \describe{
#'   \item{continent}{Name of the continent}
#'   \item{country}{Name of the country}
#'   \item{link}{Link to shapefiles in a tibble}
#' }
#' @source \url{http://download.geofabrik.de/}
"ll_osm_countries"
