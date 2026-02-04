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
#> Error in loadNamespace(x): there is no package called ‘usethis’
```
