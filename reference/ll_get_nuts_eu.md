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
#> Error in loadNamespace(x): there is no package called ‘stringr’
```
