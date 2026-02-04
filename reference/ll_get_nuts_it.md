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
#> Error in loadNamespace(x): there is no package called ‘usethis’
ll_get_nuts_it(name = "Rimini", level = 3)
#> Error in loadNamespace(x): there is no package called ‘usethis’
```
