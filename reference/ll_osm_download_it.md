# Download OSM data in geopackage format for regions, provinces, and municipalities in Italy.

See `ll_osm_it_gpkg` for all available files.

## Usage

``` r
ll_osm_download_it(
  level = "comuni",
  name = NULL,
  code = NULL,
  overwrite = FALSE,
  wget = FALSE,
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

- wget:

  Logical, defaults to FALSE. If TRUE, it downloads files with wget (if
  available), otherwise uses default method. Setting wget to TRUE may
  contribute to prevent download timeouts; notice that apparent freeze
  of the download progress in the console are common, and mostly the
  download is just continuing in the background (for reference, check
  file size in folder.)

- quiet:

  Logical, defaults to FALSE. If TRUE no messages about download
  advancement are printed.

## Value

Used only for its side effects (downloads osm data).

## Examples

``` r
if (FALSE) { # \dontrun{
ll_osm_download_it(level = "comuni", name = "Trento")
} # }
```
