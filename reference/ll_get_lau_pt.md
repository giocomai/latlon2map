# Regions and provinces in Italy (high detail, CC-BY Istat)

Source: https://dados.gov.pt/pt/datasets/freguesias-de-portugal/

## Usage

``` r
ll_get_lau_pt(
  id = NULL,
  name = NULL,
  year = 2017,
  level = "concelho",
  silent = FALSE
)
```

## Arguments

- id:

  A character vector composed of six digits. Corresponds to "dicofre".

- year:

  Defaults to 2017 (latest and currently only available).

- level:

  Defaults to "freguesia". Valid value include "freguesia", "concelho",
  "distrito", "des_simpli".

## Examples

``` r
ll_set_folder(fs::path(fs::path_home_r(), "R"))
#> /home/runner/R
ll_get_lau_pt()
#> Error in loadNamespace(x): there is no package called ‘usethis’
ll_get_lau_pt(name = "Porto")
#> Error in loadNamespace(x): there is no package called ‘usethis’
```
