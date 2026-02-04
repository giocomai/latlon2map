# Extract OSM data for regions, provinces, and municipalities in Italy.

See `ll_osm_it_gpkg` for all available files.

## Usage

``` r
ll_osm_extract_it(
  level = "comuni",
  name = NULL,
  code = NULL,
  layer = "lines",
  quiet = FALSE
)
```

## Arguments

- level:

  One of "regioni", "provincie", "comuni". Defaults to "comuni".

- name:

  Name of geographic entity. Check `ll_osm_it_gpkg` or
  [`ll_get_nuts_it()`](https://giocomai.github.io/latlon2map/reference/ll_get_nuts_it.md)
  for valid names.

- code:

  Used in alternative to name. Check `ll_osm_it_gpkg` or
  [`ll_get_nuts_it()`](https://giocomai.github.io/latlon2map/reference/ll_get_nuts_it.md)
  for valid values.

- layer:

  Defaults to "lines". Must be one of "points", "lines",
  "multilinestrings", "multipolygons", or "other_relations"

- quiet:

  Logical, defaults to FALSE. If TRUE, supresses messages generated when
  reading the geopackage file.

## Value

An sf object.

## Examples

``` r
if (FALSE) { # \dontrun{
ll_osm_extract_it(level = "comuni", name = "Trento")
} # }
```
