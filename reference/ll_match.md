# Matches a data frame with longitude and latitude to an sf object

Matches a data frame with longitude and latitude to an sf object

## Usage

``` r
ll_match(
  data,
  longitude = 1,
  latitude = 2,
  join = sf::st_intersects,
  sample = NULL,
  match = longlat2map::ll_get_world()
)
```

## Arguments

- data:

  A data frame or tibble with a column for longitude and one for
  latitude or an onject of the sf class. If an sf object is given, the
  longitude and latitude parameters are ignored.

- longitude:

  The exact column name or the column index (e.g. 1 if first column) for
  longitude. Defaults to 1.

- latitude:

  The exact column name or the column index (e.g. 1 if first column) for
  latitude. Defaults to 2.

- join:

  A function of the sf class determining the type of join. Defaults to
  [`sf::st_intersects`](https://r-spatial.github.io/sf/reference/geos_binary_pred.html).
  Check
  [`?sf::st_join`](https://r-spatial.github.io/sf/reference/st_join.html)
  for alternatives.

- sample:

  Defaults to NULL. If given, it runs the matching with only a subset of
  the original dataframe. Suggested for testing in particular when
  working with big datasets.

- match:

  An sf object to be matched with the given dataframe, defaults to
  `longlat2map::ll_get_world()`. This package facilitate obtaining
  alternative reference maps with functions such as
  `longlat2map::ll_get_nuts_eu()` and `longlat2map::ll_get_nuts_us()`

## Value

An sf object with CRS 4326.
