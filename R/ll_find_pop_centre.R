#' Find the population-weighted centre of a municipality
#'
#' @param sf_location 
#' @param sf_population_grid 
#' @param power Defaults to 2. To give more weight to cells with higher population density, raise the number of residents by the power of.
#' @param join Defaults to sf::st_intersects.
#'
#' @return
#' @export
#'
#' @examples
#' 
#' name = "Pinzolo"
#' sf_location <- ll_get_nuts_it(name = name, level = "lau", resolution = "high")
#' 
#' lau_grid_name_temp <- stringr::str_c(name, "_lau_high-st_intersects")
#' 
#' sf_location_grid <- ll_get_population_grid(match_sf = sf_location,
#'                                           match_name = lau_grid_name_temp,
#'                                           match_country = "IT",
#'                                           join = sf::st_intersects)
#'
#'
#' pop_centre <- ll_find_pop_centre(sf_location = sf_location,
#'                                  sf_population_grid = sf_location_grid,
#'                                  power = 2)

ll_find_pop_centre <- function(sf_location,
                               sf_population_grid,
                               power = 2,
                               join = sf::st_intersects) {
  
  sf_polygon <- sf_location %>% 
    sf::st_cast(to = "POLYGON") %>% 
    dplyr::mutate(id = dplyr::row_number()) %>% 
    dplyr::select(id)
  
  if (nrow(sf_polygon)>1) {
    df_pop_by_polygon <- purrr::map_dfr(.x = seq_along(along.with = 1:nrow(sf_polygon)),
                                        .f = function(x) {
                                          temp <- sf::st_filter(x = sf_population_grid,
                                                                y = sf_polygon %>% dplyr::slice(x),
                                                                join = join) %>% 
                                            sf::st_drop_geometry() 
                                          
                                          if (nrow(temp)==0) {
                                            tibble::tibble(TOT_P = 0, id = x)
                                          } else {
                                            temp %>% 
                                              dplyr::summarise(TOT_P = (sum(TOT_P)), id = x)
                                          }
                                        })
    
    sf_location <- sf_polygon %>% dplyr::slice(which.max(df_pop_by_polygon$TOT_P))
    
  }
  
  sf_location_grid <- sf::st_filter(x = sf_population_grid %>% sf::st_transform(crs = 3857),
                                    y = sf_location %>% sf::st_transform(crs = 3857),
                                    join = join) %>% 
    sf::st_transform(crs = 4326)
  
  sf_pop_centre <- dplyr::bind_cols(sf_location_grid %>% 
                                      sf::st_drop_geometry() %>% 
                                      select(TOT_P),
                                    sf_location_grid %>% 
                                      sf::st_transform(crs = 3857) %>% 
                                      sf::st_centroid() %>% 
                                      sf::st_transform(crs = 4326) %>% 
                                      sf::st_coordinates() %>% 
                                      tibble::as_tibble()) %>% 
    summarise(x = weighted.mean(x = X, w = TOT_P^power), 
              y = weighted.mean(x = Y, w = TOT_P^power)) %>% 
    sf::st_as_sf(coords = c("x", "y"), crs = 4326)
  
  # sf_location_grid %>% 
  #   dplyr::filter(TOT_P>=median(TOT_P))
  
  if (sf::st_intersects(x = sf_location, y = sf_pop_centre, sparse = FALSE)==FALSE) {
    sf_cell <- sf::st_filter(x = sf_location_grid %>% sf::st_transform(crs = 3857),
                             sf_pop_centre %>% sf::st_transform(crs = 3857),
                             join = sf::st_nearest_feature)
    sf_cell_intersection <- sf::st_intersection(sf_cell,
                                                sf_location %>%
                                                  sf::st_transform(crs = 3857))
    sf_pop_centre <- sf::st_centroid(sf_cell_intersection %>%
                                       sf::st_transform(crs = 3857)) %>%
      sf::st_transform(crs = 4326)
  }
  sf_pop_centre 
}