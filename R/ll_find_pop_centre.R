#' Find the population-weighted centre of a municipality
#'
#' @param sf_location
#' @param sf_population_grid
#' @param power Defaults to 2. To give more weight to cells with higher population density, raise the number of residents by the power of.
#' @param join Defaults to sf::st_intersects.
#' @param adjusted If adjusted is set to TRUE, join is ignored. The population of cells along the boundary line are weighted by the share of the cell included within the border.
#'
#' @return
#' @export
#'
#' @examples
#'
#' ll_set_folder("~/R/")
#' name <- "Pinzolo"
#' sf_location <- ll_get_nuts_it(name = name, level = "lau", resolution = "high")
#'
#' lau_grid_name_temp <- stringr::str_c(name, "_lau_high-st_intersects")
#'
#' sf_location_grid <- ll_get_population_grid(
#'   match_sf = sf_location,
#'   match_name = lau_grid_name_temp,
#'   match_country = "IT",
#'   join = sf::st_intersects
#' )
#'
#'
#' pop_centre <- ll_find_pop_centre(
#'   sf_location = sf_location,
#'   sf_population_grid = sf_location_grid,
#'   power = 2
#' )
ll_find_pop_centre <- function(sf_location,
                               sf_population_grid,
                               power = 2,
                               join = sf::st_intersects,
                               adjusted = FALSE) {
  if (is.element("TOT_P", colnames(sf_population_grid))) {
    sf_population_grid <- sf_population_grid %>%
      dplyr::rename(population = TOT_P)
  } else if (is.element("TOT_P_2018", colnames(sf_population_grid))) {
    sf_population_grid <- sf_population_grid %>%
      dplyr::rename(population = TOT_P_2018)
  } else if (is.element("POP_2020", colnames(sf_population_grid))) {
    sf_population_grid <- sf_population_grid %>%
      dplyr::rename(population = POP_2020)
  }


  if (adjusted == TRUE) {
    # adjust population for cells that intersect the boundary
    intersect_grid_sf <- sf::st_filter(
      x = sf_population_grid,
      y = sf_location,
      .predicate = sf::st_intersects
    )
    within_grid_sf <- sf::st_filter(
      x = sf_population_grid,
      y = sf_location,
      .predicate = sf::st_within
    )
    boundary_grid_sf <- dplyr::anti_join(intersect_grid_sf,
      within_grid_sf %>%
        sf::st_drop_geometry(),
      by = "GRD_ID"
    )
    boundary_grid_adjusted_df <- boundary_grid_sf %>%
      sf::st_drop_geometry()

    boundary_grid_adjusted_df$population_adjusted <- boundary_grid_sf$population * as.numeric(sf::st_area(sf::st_intersection(
      boundary_grid_sf,
      sf_location
    )) / sf::st_area(boundary_grid_sf))

    sf_location_grid <- intersect_grid_sf %>%
      dplyr::left_join(
        y = boundary_grid_adjusted_df %>%
          dplyr::select(GRD_ID, population_adjusted),
        by = "GRD_ID"
      ) %>%
      dplyr::mutate(population = dplyr::if_else(condition = is.na(population_adjusted),
        true = population,
        false = population_adjusted
      ))
  } else {
    sf_location_grid <- sf::st_filter(
      x = sf_population_grid,
      y = sf_location,
      .predicate = join
    )
  }


  sf_polygon <- sf_location %>%
    dplyr::select(geometry) %>%
    sf::st_cast(to = "POLYGON") %>%
    dplyr::mutate(id = dplyr::row_number())

  if (nrow(sf_polygon) > 1) {
    df_pop_by_polygon <- purrr::map_dfr(
      .x = seq_along(along.with = 1:nrow(sf_polygon)),
      .f = function(x) {
        temp <- sf::st_filter(
          x = sf_location_grid,
          y = sf_polygon %>% dplyr::slice(x),
          .predicate = join
        ) %>%
          sf::st_drop_geometry()

        if (nrow(temp) == 0) {
          tibble::tibble(population = 0, id = x)
        } else {
          temp %>%
            dplyr::summarise(
              population = (sum(population)),
              id = x
            )
        }
      }
    )

    sf_location <- sf_polygon %>%
      dplyr::slice(which.max(df_pop_by_polygon$population))

    sf_location_grid <- sf::st_filter(
      x = sf_location_grid,
      y = sf_location,
      .predicate = join
    )
  }

  sf_pop_centre <- dplyr::bind_cols(
    sf_location_grid %>%
      sf::st_drop_geometry() %>%
      dplyr::select(population),
    sf_location_grid %>%
      sf::st_centroid() %>%
      sf::st_transform(crs = 4326) %>%
      sf::st_coordinates() %>%
      tibble::as_tibble()
  ) %>%
    dplyr::summarise(
      x = weighted.mean(x = X, w = population^power),
      y = weighted.mean(x = Y, w = population^power)
    ) %>%
    sf::st_as_sf(coords = c("x", "y"), crs = 4326)

  # sf_location_grid %>%
  #   dplyr::filter(population>=median(population))


  # if the pop-weighted centre is out of the boundary,
  # take the closest closest cell, crop it with the boundary,
  # and use the centroid of the remaining part
  if (sf::st_intersects(
    x = sf_location,
    y = sf_pop_centre,
    sparse = FALSE
  ) == FALSE) {
    sf_cell <- sf_location_grid %>%
      dplyr::slice(sf::st_nearest_feature(
        x = sf_pop_centre,
        y = sf_location_grid
      ))

    sf_cell_intersection <- sf::st_intersection(
      sf_cell,
      sf_location
    )

    sf_pop_centre <- sf::st_centroid(sf_cell_intersection) %>%
      sf::st_transform(crs = 4326)
  }
  sf_pop_centre
}
