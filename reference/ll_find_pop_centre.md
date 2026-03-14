# Find the population-weighted centre of a municipality or other geographic entity

Find the population-weighted centre of a municipality or other
geographic entity

## Usage

``` r
ll_find_pop_centre(
  sf_location,
  sf_population_grid,
  power = 2,
  join = sf::st_intersects,
  adjusted = FALSE
)
```

## Arguments

- sf_location:

  Relevant gepgraphic entity as an `sf` object, typically retrieved with
  [`ll_get_lau_eu()`](https://giocomai.github.io/latlon2map/reference/ll_get_lau_eu.md)
  or similar.

- sf_population_grid:

  Relevant population grid, typically retrieved with
  [`ll_get_population_grid()`](https://giocomai.github.io/latlon2map/reference/ll_get_population_grid.md).

- power:

  Defaults to 2. To give more weight to cells with higher population
  density, raise the number of residents by the power of.

- join:

  Defaults to
  [`sf::st_intersects()`](https://r-spatial.github.io/sf/reference/geos_binary_pred.html).

- adjusted:

  If adjusted is set to `TRUE`, join is ignored. The population of cells
  along the boundary line are weighted by the share of the cell included
  within the border.

## Value

A point as an `sf` object. The population-weighted centre for the
relevant entity.

## Examples

``` r
if (FALSE) { # \dontrun{
  ll_set_folder("~/R/")
  name <- "Pinzolo"
  sf_location <- ll_get_nuts_it(name = name,
                                level = "lau",
                                resolution = "high")

  lau_grid_name_temp <- stringr::str_c(name, "_lau_high-st_intersects")

  sf_location_grid <- ll_get_population_grid(
    match_sf = sf_location,
    match_name = lau_grid_name_temp,
    match_country = "IT",
    join = sf::st_intersects
  )


  pop_centre <- ll_find_pop_centre(
    sf_location = sf_location,
    sf_population_grid = sf_location_grid,
    power = 2
  )
} # }
```
