# Extract shape files of roads from previously downloaded

Extract shape files of roads from previously downloaded

## Usage

``` r
ll_osm_get_roads(country, silent = FALSE)
```

## Arguments

- country:

  The name of one or more geographic entities from files typically
  previously downloaded with
  [`ll_osm_download()`](https://giocomai.github.io/latlon2map/reference/ll_osm_download.md).

- silent:

  Defaults to `FALSE`. If `TRUE`, hides copyright notice. Useful e.g.
  when using this in reports or in loops. The copyright notice must
  still be shown where the final output is used.

## Value

All roads in a country by OpenStreetMap.

## Examples

``` r
if (FALSE) { # \dontrun{
ll_osm_get_roads(country = "Romania")
} # }
```
