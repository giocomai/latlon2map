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
    dplyr::select(geometry) %>% 
    sf::st_cast(to = "POLYGON") %>% 
    dplyr::mutate(id = dplyr::row_number())
  
  if (nrow(sf_polygon)>1) {
    df_pop_by_polygon <- purrr::map_dfr(.x = seq_along(along.with = 1:nrow(sf_polygon)),
                                        .f = function(x) {
                                          temp <- sf::st_filter(x = sf_population_grid %>% sf::st_transform(crs = 3857),
                                                                y = sf_polygon %>% dplyr::slice(x) %>% sf::st_transform(crs = 3857),
                                                                join = join) %>% 
                                            sf::st_drop_geometry() 
                                          
                                          if (nrow(temp)==0) {
                                            tibble::tibble(Population = 0, id = x)
                                          } else {
                                            temp %>% 
                                              dplyr::summarise(Population = (sum(Population)), id = x)
                                          }
                                        })
    
    sf_location <- sf_polygon %>% dplyr::slice(which.max(df_pop_by_polygon$Population))
    
  }
  
  sf_location_grid <- sf::st_filter(x = sf_population_grid %>% sf::st_transform(crs = 3857),
                                    y = sf_location %>% sf::st_transform(crs = 3857),
                                    join = join) %>% 
    sf::st_transform(crs = 4326)
  if (is.element("TOT_P", colnames(sf_location_grid))) {
    sf_location_grid  <- sf_location_grid %>% 
      dplyr::rename(Population = TOT_P)
  }
  sf_pop_centre <- dplyr::bind_cols(sf_location_grid %>% 
                                      sf::st_drop_geometry() %>% 
                                      dplyr::select(Population),
                                    sf_location_grid %>% 
                                      sf::st_transform(crs = 3857) %>% 
                                      sf::st_centroid() %>% 
                                      sf::st_transform(crs = 4326) %>% 
                                      sf::st_coordinates() %>% 
                                      tibble::as_tibble()) %>% 
    dplyr::summarise(x = weighted.mean(x = X, w = Population^power), 
                     y = weighted.mean(x = Y, w = Population^power)) %>% 
    sf::st_as_sf(coords = c("x", "y"), crs = 4326)
  
  # sf_location_grid %>% 
  #   dplyr::filter(Population>=median(Population))
  
  if (sf::st_intersects(x = sf_location %>% sf::st_transform(crs = 3857), y = sf_pop_centre %>% sf::st_transform(crs = 3857), sparse = FALSE)==FALSE) {
    sf_cell <- sf_location_grid %>% sf::st_transform(crs = 3857) %>% 
      dplyr::slice(sf::st_nearest_feature(x = sf_pop_centre %>% sf::st_transform(crs = 3857),
                                           y =  sf_location_grid %>% sf::st_transform(crs = 3857)))
    
    sf_cell_intersection <- sf::st_intersection(sf_cell,
                                                sf_location %>%
                                                  sf::st_transform(crs = 3857))
    sf_pop_centre <- sf::st_centroid(sf_cell_intersection %>%
                                       sf::st_transform(crs = 3857)) %>%
      sf::st_transform(crs = 4326)
  }
  sf_pop_centre 
}