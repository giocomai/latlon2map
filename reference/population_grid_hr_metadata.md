# A data frame with links to High Resolution Population Density Maps distributed by Facebook on HDX

It is used to download files with
[`ll_get_population_grid_hr()`](https://giocomai.github.io/latlon2map/reference/ll_get_population_grid_hr.md)

## Usage

``` r
population_grid_hr_metadata
```

## Format

A tibble

- country:

  Name of the country in English

- country_code:

  Two letter code as used by eurostat, see also
  `countrycode::codelist$eurostat`

- download_ulr:

  Link to zipped dataset

- url:

  Link to page describing the dataset

## Source

<https://data.humdata.org/>
