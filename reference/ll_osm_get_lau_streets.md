# Get all streets available in OpenStreetMap located in given local administrative unit.

Relies on the output of
[`ll_get_lau_eu()`](https://giocomai.github.io/latlon2map/reference/ll_get_lau_eu.md)
for the boundaries of local administrative units.

## Usage

``` r
ll_osm_get_lau_streets(
  gisco_id,
  country = NULL,
  unnamed_streets = TRUE,
  lau_boundary_sf = NULL,
  streets_sf = NULL,
  country_code_type = "eurostat",
  year = 2021,
  fallback = TRUE
)
```

## Arguments

- gisco_id:

  Gisco identifier.

- country:

  Name of country as included in Geofabrik's datasets, does not always
  match common country names or geography. For details on available
  country names see the dataset included in this package:
  `ll_osm_countries`

- unnamed_streets:

  Defaults to TRUE. If FALSE, it drops all streets with missing "name"
  or missing "fclass".

- lau_boundary_sf:

  Defaults to NULL. If given, used to speed up processing. Must be an
  `sf` object such as the ones output by by
  [`ll_get_lau_eu()`](https://giocomai.github.io/latlon2map/reference/ll_get_lau_eu.md).

- streets_sf:

  Defaults to NULL. If given, used to speed up processing. Must be an
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

- fallback:

  Logical, defaults to TRUE. If a `gisco_id` does not match an entity in
  [`ll_get_lau_eu()`](https://giocomai.github.io/latlon2map/reference/ll_get_lau_eu.md),
  try alternatives for the boundaries based on the country code,
  including
  [`ll_get_nuts_eu()`](https://giocomai.github.io/latlon2map/reference/ll_get_nuts_eu.md),
  [`ll_get_gadm()`](https://giocomai.github.io/latlon2map/reference/ll_get_gadm.md),
  and
  [`ll_get_adm_ocha()`](https://giocomai.github.io/latlon2map/reference/ll_get_adm_ocha.md).

## Value

An `sf` objects with all streets of a given LAU based on OpenStreetMap

## Examples

``` r
if (FALSE) { # \dontrun{
ll_osm_get_lau_streets(gisco_id = "IT_022205", unnamed_streets = FALSE)

# or if country name does not match

ll_osm_get_lau_streets(gisco_id = "EL_01020204", country = "greece")
} # }
```
