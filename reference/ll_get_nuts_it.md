# Regions and provinces in Italy (high detail, CC-BY Istat)

2019 / WGS84 UTM32N

## Usage

``` r
ll_get_nuts_it(
  name = NULL,
  level = 2,
  year = 2023,
  resolution = "low",
  silent = FALSE,
  no_check_certificate = FALSE
)
```

## Arguments

- level:

  Defaults to "2", i.e. regioni. Available: "3" (i.e. province), and
  "lau", local administrative units.

- year:

  Defaults to 2023 (latest available).

- resolution:

  Defaults to "low". Valid values are either "low" or "high".

- no_check_certificate:

  Logical, defaults to FALSE. Enable only if certificate issues, and if
  you are aware of the security implications.

## Examples

``` r
ll_set_folder(fs::path(fs::path_home_r(), "R"))
#> /home/runner/R
ll_get_nuts_it()
#> ℹ Source: https://www.istat.it/it/archivio/222527
#> ℹ Istat (CC-BY)
#> Simple feature collection with 20 features and 5 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 6.626621 ymin: 35.49345 xmax: 18.52038 ymax: 47.09178
#> Geodetic CRS:  WGS 84
#> # A tibble: 20 × 6
#>    COD_RIP COD_REG DEN_REG       Shape_Leng Shape_Area                  geometry
#>  *   <dbl>   <dbl> <chr>              <dbl>      <dbl>        <MULTIPOLYGON [°]>
#>  1       1       1 Piemonte        1234644.    2.54e10 (((8.44976 46.46176, 8.4…
#>  2       1       2 Valle d'Aosta    311187.    3.26e 9 (((7.588572 45.97075, 7.…
#>  3       1       3 Lombardia       1411276.    2.39e10 (((8.816417 45.02231, 8.…
#>  4       2       4 Trentino-Alt…    800732.    1.36e10 (((12.20519 47.0865, 12.…
#>  5       2       5 Veneto          1056504.    1.84e10 (((12.50603 46.67803, 12…
#>  6       2       6 Friuli-Venez…    659341.    7.94e 9 (((13.76238 45.65511, 13…
#>  7       1       7 Liguria          821916.    5.42e 9 (((9.851328 44.02328, 9.…
#>  8       2       8 Emilia-Romag…   1176992.    2.25e10 (((10.4808 44.18949, 10.…
#>  9       3       9 Toscana         1304650.    2.30e10 (((11.11534 42.25869, 11…
#> 10       3      10 Umbria           619405.    8.46e 9 (((12.43119 43.59136, 12…
#> 11       3      11 Marche           618848.    9.34e 9 (((12.76911 43.96585, 12…
#> 12       3      12 Lazio           1056599.    1.72e10 (((13.45607 40.78707, 13…
#> 13       4      13 Abruzzo          613573.    1.08e10 (((13.92088 42.89333, 13…
#> 14       4      14 Molise           433631.    4.46e 9 (((14.84314 42.0408, 14.…
#> 15       4      15 Campania         890506.    1.37e10 (((15.29487 40.02368, 15…
#> 16       4      16 Puglia          1182953.    1.95e10 (((18.12246 39.8799, 18.…
#> 17       4      17 Basilicata       613038.    1.01e10 (((15.71509 39.96661, 15…
#> 18       4      18 Calabria         847416.    1.52e10 (((15.80124 39.69743, 15…
#> 19       5      19 Sicilia         1344688.    2.58e10 (((12.56001 35.50886, 12…
#> 20       5      20 Sardegna        1449231.    2.41e10 (((8.410014 38.86321, 8.…
ll_get_nuts_it(name = "Rimini", level = 3)
#> ℹ Source: https://www.istat.it/it/archivio/222527
#> ℹ Istat (CC-BY)
#> Simple feature collection with 1 feature and 12 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 12.09618 ymin: 43.73192 xmax: 12.75564 ymax: 44.16247
#> Geodetic CRS:  WGS 84
#> # A tibble: 1 × 13
#>   COD_RIP COD_REG COD_PROV COD_CM COD_UTS DEN_PROV DEN_CM DEN_UTS SIGLA TIPO_UTS
#> *   <dbl>   <dbl>    <dbl>  <dbl>   <dbl> <chr>    <chr>  <chr>   <chr> <chr>   
#> 1       2       8       99      0      99 Rimini   -      Rimini  RN    Provinc…
#> # ℹ 3 more variables: Shape_Leng <dbl>, Shape_Area <dbl>,
#> #   geometry <MULTIPOLYGON [°]>
```
