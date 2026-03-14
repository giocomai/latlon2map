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
#> Error in loadNamespace(x): there is no package called ‘stringr’
```
