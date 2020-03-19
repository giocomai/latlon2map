#' Matches a data frame with longitude and latitude to an sf object
#'
#' @param data A data frame or tibble with a column for longitude and one for latitude or an onject of the sf class. If an sf object is given, the longitude and latitude parameters are ignored.
#' @param longitude The exact column name or the column index (e.g. 1 if first column) for longitude. Defaults to 1.
#' @param latitude The exact column name or the column index (e.g. 1 if first column) for latitude. Defaults to 2.
#' @param join A function of the sf class determining the type of join. Defaults to `sf::st_intersects`. Check `?sf::st_join` for alternatives.
#' @param sample Defaults to NULL. If given, it runs the matching with only a subset of the original dataframe. Suggested for testing in particular when working with big datasets.
#' @param match An sf object to be matched with the given dataframe, defaults to `longlat2map::ll_get_world()`. This package facilitate obtaining alternative reference maps with functions such as `longlat2map::ll_get_nuts_eu()` and `longlat2map::ll_get_nuts_us()`
#'
#' @return An sf object with CRS 4326.
#' @export
#'
#' @examples
ll_match_geo <- function(data,
                         longitude = 1,
                         latitude = 2,
                         join = sf::st_intersects,
                         sample = NULL,
                         match = longlat2map::ll_get_world()) {
  if (is.null(sample)==FALSE) {
    data <- data %>% dplyr::sample_n(size = sample)
  }
  
  if (is.element("sf", class(data))==FALSE) {
    sf_data <- data %>% 
      sf::st_as_sf(coords = c(longitude,latitude), crs = 4326)
  } else {
    sf_data <- data
  }

  sf::st_join(sf_data %>% sf::st_transform(crs = 4326),
              match %>% sf::st_transform(crs = 4326),
              join = join)
}