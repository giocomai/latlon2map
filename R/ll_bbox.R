#' Provide a bounding box with a consistent, user given ratio
#'
#' This is useful in particular to make geom_sf()-based ggplots with consistent aspect ratio.
#'
#' @param sf An sf object.
#' @param ratio Defaults to "4:3". A chacters string, in the form of e.g. "4:3" or "16:9" or "1:1" (other values possible)
#'
#' @return A bounding box vector, same as with `sf::st_bbox()`, but with the given ratio set and compatible with crs 4326.
#' @export
#'
#' @examples
#' \dontrun{
#' # The following two graphs will have same 4:3 aspect ratio
#' ll_set_folder("~/R/")
#' library("ggspatial")
#'
#' sf_location <- ll_get_nuts_it(name = "Palmanova", level = "lau", resolution = "low")
#'
#' ggplot() +
#'   annotation_map_tile(type = "osm", zoomin = -1, cachedir = fs::path(ll_set_folder(), "ll_data")) +
#'   geom_sf(data = sf::st_as_sfc(ll_bbox(sf_location)), fill = NA, color = NA) +
#'   geom_sf(
#'     data = sf_location,
#'     colour = "darkred",
#'     size = 2,
#'     fill = NA,
#'     alpha = 0.8
#'   )
#'
#'
#' sf_location <- ll_get_nuts_it(name = "Pinzolo", level = "lau", resolution = "low")
#'
#' ggplot() +
#'   annotation_map_tile(type = "osm", zoomin = -1, cachedir = fs::path(ll_set_folder(), "ll_data")) +
#'   geom_sf(data = sf::st_as_sfc(ll_bbox(sf_location)), fill = NA, color = NA) +
#'   geom_sf(
#'     data = sf_location,
#'     colour = "darkred",
#'     size = 2,
#'     fill = NA,
#'     alpha = 0.8
#'   )
#' }
#'
ll_bbox <- function(sf,
                    ratio = "4:3") {
  bbox <- sf::st_bbox(sf %>% sf::st_transform(crs = 3857))

  horizontal_original <- as.numeric(bbox[3] - bbox[1])
  vertical_original <- as.numeric(bbox[4] - bbox[2])

  original_ratio_n <- horizontal_original / vertical_original

  desired_ratio_m <- stringr::str_split(
    string = ratio,
    pattern = ":",
    n = 2,
    simplify = TRUE
  ) %>%
    as.numeric()

  desired_ratio_n <- desired_ratio_m[1] / desired_ratio_m[2]

  if (desired_ratio_n == original_ratio_n) {
    # do nothing
  } else if (desired_ratio_n < original_ratio_n) {
    vertical_fixed <- horizontal_original / desired_ratio_n
    change <- (vertical_fixed - vertical_original) / 2
    bbox[2] <- bbox[2] - change
    bbox[4] <- bbox[4] + change
  } else if (desired_ratio_n > original_ratio_n) {
    horizontal_fixed <- vertical_original * desired_ratio_n
    change <- (horizontal_fixed - horizontal_original) / 2


    bbox[1] <- bbox[1] - change
    bbox[3] <- bbox[3] + change
  }
  return(bbox %>% sf::st_as_sfc() %>%
    sf::st_transform(crs = 4326) %>%
    sf::st_bbox())
}
