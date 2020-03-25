
<!-- README.md is generated from README.Rmd. Please edit that file -->

# latlon2map

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of `latlon2map` is to make it simple to process spatial data,
and to match tabular data distributed in spreadsheets with
longitude/latitude columns with various geographical units based on
shapefile distributed by statistical offices such as Eurostat.

The package includes a number of convenience functions that download and
cache locally pre-processed files to reduce boilerplate code commonly
found in data projects, hopefully speeding up the data pipeline and
favouring reproducibility.

By dafault, `latlon2map` stores all downloaded files in the current
working directory. However, it makes is easy to keep them on a single
folder, preventing the common issue of having multiple copies of the
same geographic files for each data project on a computer. The code
remains transferrable and reproducible, as missing files are simply
downloaded on the fly if unavaiable locally.

## Installation

You can install `latlon2map` from GitHub with:

``` r
remotes::install_github("giocomai/latlon2map")
```

## Use

All `latlon2map` function starts with `ll_` to facilitate
auto-completion. By default, data are stored in a folder called
`ll_data` inside the working directory. However, I suggest caching data
in a separate folder for system-wide caching: you will not need to
re-download again geographic files for different projects, and you will
not unncessarily sync multiple copies of those files for each project
that needs them. You need to call e.g. `ll_set_folder("~/R/")` once per
session.

``` r
library("latlon2map")
ll_set_folder("~/R/")
#> [1] "~/R/"
```

There are currently a number of functions facilitating downloads of
geographic datasets, mostly from Eurostat’s website. They all return
`sf` objects, all of them transformed to crs 4326, all of them keeping
all of the columns present in the original dataset. Future versions will
likely add standard-named columns to facilitate matching data between
different datasets.

For reference all of the original data are kept in the `ll_data/shp/`
folder, so it is possible to check metadata about each dataset.
Information on copyright is printed to the console each time a given
data source is used.

Functions calls such as `ll_get_world(resolution = 60)` can safely be
used where you would usually place your `sf` object/data frame. They
will download items when they are not locally available, and will simply
load pre-processed data on the following calls.

Check the package vignette for more details, examples, and use cases.

## Shiny app

The package includes a shiny app that facilitates matching
latitude/longitude data frames wtih geographic units.

``` r
ll_app()
```
