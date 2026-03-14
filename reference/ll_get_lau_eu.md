# Gets local administrative units from Eurostat's website

Source:
<https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/lau#lau18>

## Usage

``` r
ll_get_lau_eu(
  gisco_id = NULL,
  name = NULL,
  year = 2021,
  silent = FALSE,
  lau_sf = NULL,
  fallback = TRUE
)
```

## Arguments

- gisco_id:

  Gisco identifier of the relevant administrative unit. If given, takes
  precedence over name.

- name:

  Name of the local administrative unit in the local language. Use
  gisco_id whenever possible, as names of local administrative units are
  not unique, e.g. there are 11 "Neuenkirchen" in the dataset. If both
  `name` and `gisco_id` are `NULL`, then it returns all municipalities.

- year:

  Year of mapping, defaults to most recent (2021). Available starting
  with 2011.

- silent:

  Defaults to `FALSE`. If `TRUE`, hides copyright notice. Useful e.g.
  when using this in reports or in loops. The copyright notice must
  still be shown where the final output is used.

- lau_sf:

  sf object, exactly such as the one that would be returned by
  `ll_get_lau_eu()`. Used to speed-up computation when bulk processing.

## Value

European LAU in sf format

## Examples

``` r
ll_set_folder("~/R/")
#> [1] "~/R/"
ll_get_lau_eu()
#> ℹ © EuroGeographics for the administrative boundaries
#> Simple feature collection with 98188 features and 8 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -63.15082 ymin: -21.38881 xmax: 55.83573 ymax: 71.18196
#> Geodetic CRS:  WGS 84
#> # A tibble: 98,188 × 9
#>    GISCO_ID  CNTR_CODE LAU_ID LAU_NAME       POP_2021 POP_DENS_2 AREA_KM2  YEAR
#>  * <chr>     <chr>     <chr>  <chr>             <dbl>      <dbl>    <dbl> <int>
#>  1 CZ_578622 CZ        578622 Příluka             169         NA     3.77  2021
#>  2 CZ_578631 CZ        578631 Pustá Kamenice      317         NA    15.3   2021
#>  3 AT_40708  AT        40708  Gschwandt          2880         NA    16.7   2021
#>  4 CZ_578649 CZ        578649 Pustá Rybná         158         NA    14.0   2021
#>  5 CZ_578657 CZ        578657 Radiměř            1158         NA    28.6   2021
#>  6 CZ_578665 CZ        578665 Dražeň              155         NA     8.02  2021
#>  7 CZ_578673 CZ        578673 Rohozná             639         NA    11.6   2021
#>  8 CZ_578681 CZ        578681 Rozhraní            321         NA     4.09  2021
#>  9 CZ_578690 CZ        578690 Rozstání            237         NA     7.36  2021
#> 10 CZ_578703 CZ        578703 Rudná               153         NA     6.62  2021
#> # ℹ 98,178 more rows
#> # ℹ 1 more variable: geometry <MULTIPOLYGON [°]>
```
