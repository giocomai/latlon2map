# Download OSM data for whole countries from Geofabrik.

N.B. Names do not always correspond to official name of countries and
may include different geographic entities. For a full list of available
"countries" as made available by Geofabrik, see the internal dataset
`ll_osm_countries`. Be considered in downloading files.

## Usage

``` r
ll_osm_download(countries, overwrite = FALSE, wget = FALSE)
```

## Arguments

- countries:

  One or more country names. For details on available country names see
  the dataset included in this package: `ll_osm_countries`

- overwrite:

  Logical, defaults to FALSE. If true, downloads new files even if
  already present.

- wget:

  Logical, defaults to FALSE. If TRUE, it downloads files with wget (if
  available), otherwise uses default method. Setting wget to TRUE may
  contribute to prevent download timeouts; notice that apparent freeze
  of the download progress in the console are common, and mostly the
  download is just continuing in the background (for reference, check
  file size in folder.)

## Value

Used only for its side effects (downloads osm data).

## Examples

``` r
if (FALSE) { # \dontrun{
ll_osm_download(countries = "Romania")
ll_osm_download(countries = c("chile", "colombia"))
} # }
```
