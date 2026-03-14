# Get administrative boundaries

Source: https://gadm.org/

## Usage

``` r
ll_get_gadm(geo, level = 0, version = "4.1")
```

## Arguments

- geo:

  Three letter country codes. If a two letter country code is given, it
  will tentatively be converted to a three-letter country code. Check
  consistency.

- level:

  Defaults to 0. Available labels, depending on data availability for
  the specific country, between 0 and 3.

- version:

  Defaults to "4.0". Untested with others.

## Value

An `sf` object

## Examples

``` r
ll_get_gadm(geo = "UKR", level = 2)
#> ℹ Source: <https://gadm.org/>
#> ℹ The data are freely available for academic use and other non-commercial use. Redistribution, or commercial use, is not allowed without prior permission. Using the data to create maps for academic publishing is allowed.
#> Simple feature collection with 629 features and 13 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 22.14045 ymin: 44.38597 xmax: 40.21807 ymax: 52.37503
#> Geodetic CRS:  WGS 84
#> # A tibble: 629 × 14
#>    GID_2  GID_0 COUNTRY GID_1 NAME_1 NL_NAME_1 NAME_2 VARNAME_2 NL_NAME_2 TYPE_2
#>  * <chr>  <chr> <chr>   <chr> <chr>  <chr>     <chr>  <chr>     <chr>     <chr> 
#>  1 ?      UKR   Ukraine ?     ?      ?         ?      ?         NA        ?     
#>  2 UKR.1… UKR   Ukraine UKR.… Cherk… Черкаська Cherk… NA        NA        Mis'k…
#>  3 UKR.1… UKR   Ukraine UKR.… Cherk… Черкаська Cherk… NA        NA        Raion 
#>  4 UKR.1… UKR   Ukraine UKR.… Cherk… Черкаська Chorn… Chornoba… NA        Raion 
#>  5 UKR.1… UKR   Ukraine UKR.… Cherk… Черкаська Chyhy… NA        NA        Raion 
#>  6 UKR.1… UKR   Ukraine UKR.… Cherk… Черкаська Drabi… NA        NA        Raion 
#>  7 UKR.1… UKR   Ukraine UKR.… Cherk… Черкаська Horod… Gorodysc… NA        Raion 
#>  8 UKR.1… UKR   Ukraine UKR.… Cherk… Черкаська Kamia… NA        NA        Raion 
#>  9 UKR.1… UKR   Ukraine UKR.… Cherk… Черкаська Kaniv… NA        NA        Misto 
#> 10 UKR.1… UKR   Ukraine UKR.… Cherk… Черкаська Kaniv… NA        NA        Raion 
#> # ℹ 619 more rows
#> # ℹ 4 more variables: ENGTYPE_2 <chr>, CC_2 <chr>, HASC_2 <chr>,
#> #   geometry <MULTIPOLYGON [°]>
```
