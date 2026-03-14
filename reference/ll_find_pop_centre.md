# Find the population-weighted centre of a municipality

Find the population-weighted centre of a municipality

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

- power:

  Defaults to 2. To give more weight to cells with higher population
  density, raise the number of residents by the power of.

- join:

  Defaults to sf::st_intersects.

- adjusted:

  If adjusted is set to TRUE, join is ignored. The population of cells
  along the boundary line are weighted by the share of the cell included
  within the border.

## Examples

``` r
ll_set_folder("~/R/")
#> [1] "~/R/"
name <- "Pinzolo"
sf_location <- ll_get_nuts_it(name = name, level = "lau", resolution = "high")
#> ℹ Source: https://www.istat.it/it/archivio/222527
#> ℹ Istat (CC-BY)

lau_grid_name_temp <- stringr::str_c(name, "_lau_high-st_intersects")

sf_location_grid <- ll_get_population_grid(
  match_sf = sf_location,
  match_name = lau_grid_name_temp,
  match_country = "IT",
  join = sf::st_intersects
)
#> ℹ Data source population grid information: Eurostat, EFGS
#> ℹ Source: https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/population-distribution-demography/geostat


pop_centre <- ll_find_pop_centre(
  sf_location = sf_location,
  sf_population_grid = sf_location_grid,
  power = 2
)
#> Warning: st_centroid assumes attributes are constant over geometries
```
