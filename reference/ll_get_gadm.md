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
#> Error in loadNamespace(x): there is no package called ‘usethis’
```
