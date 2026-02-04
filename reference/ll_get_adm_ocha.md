# Get administrative boundary lines from OCHA database

Source: https://data.humdata.org/

## Usage

``` r
ll_get_adm_ocha(
  geo,
  level = 0,
  match_name = NULL,
  source_url = NULL,
  silent = FALSE
)
```

## Arguments

- geo:

  A twe letter country code, such as "IT" for Italy and "DE" for Germany

- match_name:

  A name to be used for local caching if a subset of the grid is used.
  It is the responsibility of the user to keept it consistent. If not
  given, data are not cached locally.

- source_url:

  A direct link to the zipped version of the csv file in the original
  database, if automatic download with the country code does not work.
  For example, for Italy this would be
  "https://data.humdata.org/dataset/0eb77b21-06be-42c8-9245-2edaff79952f/resource/1e96f272-7d86-4108-b4ca-5a951a8b11a0/download/population_ita_2019-07-01.csv.zip"

- silent:

## Examples

``` r
if (interactive) {
  ll_get_adm_ocha(geo = "UA", level = 3)
}
#> Error in if (interactive) {    ll_get_adm_ocha(geo = "UA", level = 3)}: argument is not interpretable as logical
```
