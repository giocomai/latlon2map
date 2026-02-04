# Extract from zip shape files of roads from previously downloaded

Extract from zip shape files of roads from previously downloaded

## Usage

``` r
ll_osm_extract_roads(countries, download_if_missing = TRUE, overwrite = FALSE)
```

## Arguments

- countries:

  The name of one or more geographic entities from files typically
  previously downloaded with
  [`ll_osm_download()`](https://giocomai.github.io/latlon2map/reference/ll_osm_download.md)

- download_if_missing:

  Logical, defaults to TRUE. If TRUE, downloads country files with
  [`ll_osm_download()`](https://giocomai.github.io/latlon2map/reference/ll_osm_download.md)
  if they are not available locally.

- overwrite:

  Logical, defaults to FALSE. If TRUE, extracts files from zip even if
  folder already existing.

## Value

Nothing, used for its side effects (extracts shapefiles from
country-level zip files)

## Examples

``` r
if (FALSE) { # \dontrun{
ll_extract_roads(countries = "Romania")
} # }
```
