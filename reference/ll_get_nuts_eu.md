# Gets NUTS as sf object from Eurostat's website

Source:
https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/nuts#nuts16

## Usage

``` r
ll_get_nuts_eu(
  nuts_id = NULL,
  nuts_name = NULL,
  level = 3,
  resolution = 60,
  year = 2021,
  silent = FALSE
)
```

## Arguments

- nuts_id:

  NUTS id. Must correspond to the level given. e.g. "DE149" for the
  NUTS3 regionion of Sigmaringen in Germany.

- nuts_name:

  Name of the NUTS region. Run `ll_get_nuts_eu()` to check valid values
  in `NUTS_NAME` column.

- level:

  Defaults to 3, corresponding to nuts3. Available values are: 0, 1, 2,
  and 3.

- resolution:

  Defaults to "60", for 1:60 Million. Available values: are 20, 10, 3, 1
  (1 is highest quality available).

- year:

  Defaults to 2021 Available values: 2021, 2016, 2013, 2010, 2006, 2003

## Value

NUTS in sf format

## Examples

``` r
ll_get_nuts_eu()
#> ℹ © EuroGeographics for the administrative boundaries
#> ℹ Source: https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/countries
#> Simple feature collection with 1514 features and 8 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -61.84073 ymin: -21.37565 xmax: 55.84983 ymax: 80.79911
#> Geodetic CRS:  WGS 84
#> # A tibble: 1,514 × 9
#>    NUTS_ID LEVL_CODE CNTR_CODE NAME_LATN          NUTS_NAME MOUNT_TYPE URBN_TYPE
#>    <chr>       <int> <chr>     <chr>              <chr>          <int>     <int>
#>  1 DE149           3 DE        Sigmaringen        Sigmarin…          4         3
#>  2 DE211           3 DE        Ingolstadt, Kreis… Ingolsta…          4         2
#>  3 DE212           3 DE        München, Kreisfre… München,…          4         1
#>  4 DE213           3 DE        Rosenheim, Kreisf… Rosenhei…          4         2
#>  5 DE214           3 DE        Altötting          Altötting          4         2
#>  6 DE215           3 DE        Berchtesgadener L… Berchtes…          3         2
#>  7 DE216           3 DE        Bad Tölz-Wolfrats… Bad Tölz…          2         2
#>  8 DE217           3 DE        Dachau             Dachau             4         2
#>  9 DE218           3 DE        Ebersberg          Ebersberg          4         2
#> 10 DE219           3 DE        Eichstätt          Eichstätt          4         2
#> # ℹ 1,504 more rows
#> # ℹ 2 more variables: COAST_TYPE <int>, geometry <MULTIPOLYGON [°]>
```
