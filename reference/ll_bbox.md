# Provide a bounding box with a consistent, user given ratio

This is useful in particular to make geom_sf()-based ggplots with
consistent aspect ratio.

## Usage

``` r
ll_bbox(sf, ratio = "4:3")
```

## Arguments

- sf:

  An sf object.

- ratio:

  Defaults to "4:3". A chacters string, in the form of e.g. "4:3" or
  "16:9" or "1:1" (other values possible)

## Value

A bounding box vector, same as with
[`sf::st_bbox()`](https://r-spatial.github.io/sf/reference/st_bbox.html),
but with the given ratio set and compatible with crs 4326.

## Examples

``` r
if (FALSE) { # \dontrun{
# The following two graphs will have same 4:3 aspect ratio
ll_set_folder("~/R/")
library("ggspatial")

sf_location <- ll_get_nuts_it(name = "Palmanova", level = "lau", resolution = "low")

ggplot() +
  annotation_map_tile(type = "osm", zoomin = -1, cachedir = fs::path(ll_set_folder(), "ll_data")) +
  geom_sf(data = sf::st_as_sfc(ll_bbox(sf_location)), fill = NA, color = NA) +
  geom_sf(
    data = sf_location,
    colour = "darkred",
    size = 2,
    fill = NA,
    alpha = 0.8
  )


sf_location <- ll_get_nuts_it(name = "Pinzolo", level = "lau", resolution = "low")

ggplot() +
  annotation_map_tile(type = "osm", zoomin = -1, cachedir = fs::path(ll_set_folder(), "ll_data")) +
  geom_sf(data = sf::st_as_sfc(ll_bbox(sf_location)), fill = NA, color = NA) +
  geom_sf(
    data = sf_location,
    colour = "darkred",
    size = 2,
    fill = NA,
    alpha = 0.8
  )
} # }
```
