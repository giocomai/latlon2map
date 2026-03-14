# Get US counties

Source:
https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html

## Usage

``` r
ll_get_nuts_us(level = "county", resolution = "500k", year = 2018)
```

## Arguments

- level:

  Defaults to "county". Available options are: "cd116" (for
  congressional districts of the 116th Congress)

- resolution:

  Defaults to "500k", max available resolution. Available options are:
  "5m" and "20m"

- year:

  Defaults to 2018

## Examples

``` r
ll_get_nuts_us(level = "county", resolution = "500k", year = 2018)
#> ℹ Source: https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
#> Simple feature collection with 3233 features and 9 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -179.1489 ymin: -14.5487 xmax: 179.7785 ymax: 71.36516
#> Geodetic CRS:  NAD83
#> # A tibble: 3,233 × 10
#>    STATEFP COUNTYFP COUNTYNS AFFGEOID       GEOID NAME    LSAD      ALAND AWATER
#>    <chr>   <chr>    <chr>    <chr>          <chr> <chr>   <chr>     <dbl>  <dbl>
#>  1 21      007      00516850 0500000US21007 21007 Ballard 06       6.39e8 6.95e7
#>  2 21      017      00516855 0500000US21017 21017 Bourbon 06       7.50e8 4.83e6
#>  3 21      031      00516862 0500000US21031 21031 Butler  06       1.10e9 1.39e7
#>  4 21      065      00516879 0500000US21065 21065 Estill  06       6.56e8 6.52e6
#>  5 21      069      00516881 0500000US21069 21069 Fleming 06       9.03e8 7.18e6
#>  6 21      093      00516893 0500000US21093 21093 Hardin  06       1.61e9 1.75e7
#>  7 21      099      00516896 0500000US21099 21099 Hart    06       1.07e9 1.37e7
#>  8 21      131      00516912 0500000US21131 21131 Leslie  06       1.04e9 9.19e6
#>  9 21      151      00516919 0500000US21151 21151 Madison 06       1.13e9 1.53e7
#> 10 21      155      00516921 0500000US21155 21155 Marion  06       8.88e8 9.89e6
#> # ℹ 3,223 more rows
#> # ℹ 1 more variable: geometry <MULTIPOLYGON [°]>
```
