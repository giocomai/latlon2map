# Gets correspondence tables between local administrative units and nuts from Eurostat's website

Source:
https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/lau

## Usage

``` r
ll_get_lau_nuts_concordance(lau_year = 2019, nuts_year = 2016, silent = FALSE)
```

## Arguments

- lau_year:

  Defaults to 2019. See `ll_lau_nuts_concordance_links` for details on
  available combinations.

- nuts_year:

  Defaults to 2016. See `ll_lau_nuts_concordance_links` for details on
  available combinations.

- silent:

  Defaults to FALSE. If TRUE, hides copyright notice. Useful e.g. when
  using this in reports or in loops. The copyright notice must still be
  shown where the final output is used.

## Value

A tibble with a correspondence table.

## Details

Warning: due to issues in the original data, nuts may not always
correspond to the given year for all countries, e.g. in files with nuts
2016 one may find nuts 2013 for single country, e.g. Italy. Do check the
sources for details and ensure complete matching.

## Examples

``` r
if (FALSE) { # \dontrun{
ll_set_folder("~/R/")
ll_get_lau_nuts_concordance()
lau_with_nuts_df <- ll_get_lau_eu(year = 2018) %>%
  sf::st_drop_geometry() %>%
  filter(is.na(LAU_NAME) == FALSE) %>%
  dplyr::rename(gisco_id = GISCO_ID) %>%
  dplyr::left_join(
    y = ll_get_lau_nuts_concordance(
      lau_year = 2018,
      nuts_year = 2016
    ),
    by = "gisco_id"
  )
} # }
```
