# Countries and geographic entities for which shapefiles are made availabile by Geofabrik

A dataset with all names of countries, continents, as included in the
Geofabrik database. They are used to download files with
[`ll_osm_download()`](https://giocomai.github.io/latlon2map/reference/ll_osm_download.md)

## Usage

``` r
ll_osm_countries
```

## Format

A tibble

- continent:

  Name of the continent

- country:

  Name of the country

- link:

  Link to shapefiles in a tibble

## Source

<http://download.geofabrik.de/>

## Details

Links to shapefiles are stored as tibbles. Unnest to see them, e.g.
`ll_osm_countries %>% tidyr::unnest(link)` or for a single country:
`ll_osm_countries %>% dplyr::filter(country == "italy") %>% tidyr::unnest(link)`
