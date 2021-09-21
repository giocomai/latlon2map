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




#' Geographic entities in Italy for which geopackage files are availabile
#'
#' A dataset with all names of geographic entities available for direct download as geopackage files
#' They are used to download files with `ll_osm_download_it()`
#'
#' @format A list of tibbles
#' @source \url{https://osmit-estratti.wmcloud.org/}
"ll_osm_it_gpkg"


#' A data frame with links to High Resolution Population Density Maps distributed by Facebook on HDX
#'
#' It is used to download files with `ll_get_population_grid_hr()`
#'
#'
#' @format A tibble
#' \describe{
#'   \item{country}{Name of the country in English}
#'   \item{country_code}{Two letter code as used by eurostat, see also `countrycode::codelist$eurostat`}
#'   \item{download_ulr}{Link to zipped dataset}
#'   \item{url}{Link to page describing the dataset}
#' }
#' @source \url{https://data.humdata.org/}
"population_grid_hr_metadata"
