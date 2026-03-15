# Get all streets available in OpenStreetMap located in given NUTS.

Relies on the output of
[`ll_get_nuts_eu()`](https://giocomai.github.io/latlon2map/reference/ll_get_nuts_eu.md)
for the boundaries of NUTS.

## Usage

``` r
ll_osm_get_nuts_streets(
  nuts_id,
  level = 3,
  resolution = 1,
  country = NULL,
  unnamed_streets = TRUE,
  nuts_boundary_sf = NULL,
  streets_sf = NULL,
  country_code_type = "eurostat",
  year = 2021
)
```

## Arguments

- nuts_id:

  NUTS region identifier.

- country:

  Name of country as included in Geofabrik's datasets, does not always
  match common country names or geography. For details on available
  country names see the dataset included in this package:
  `ll_osm_countries`

- unnamed_streets:

  Defaults to `TRUE`. If `FALSE`, it drops all streets with missing
  "name" or missing `fclass`.

- streets_sf:

  Defaults to `NULL`. If given, used to speed up processing. Must be an
  `sf` object such as the ones output by
  [`ll_osm_get_roads()`](https://giocomai.github.io/latlon2map/reference/ll_osm_get_roads.md).

- country_code_type:

  Defaults to "eurostat". An alternative common value is "iso2c". See
  [`countrycode::codelist`](https://vincentarelbundock.github.io/countrycode/man/codelist.html)
  for a list of available codes.

- year:

  Year of LAU boundaries, defaults to most recent (2021), passed to
  [`ll_get_lau_eu()`](https://giocomai.github.io/latlon2map/reference/ll_get_lau_eu.md).
  Available starting with 2011.

## Value

An `sf` objects with all streets of a given NUTS regions based on
OpenStreetMap

## Examples

``` r
if (FALSE) { # \dontrun{
ll_osm_get_nuts_streets(nuts_id = "PT16D", country = "portugal")
} # }
```
