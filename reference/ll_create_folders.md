# Create folders to store geographic data

Create folders to store geographic data

## Usage

``` r
ll_create_folders(
  geo,
  level,
  resolution,
  year,
  file_type = c("shp", "zip", "rds")
)
```

## Arguments

- geo:

  The geographic unit of reference as a two-letter code

- level:

  E.g. NUTS0, NUTS1, or county, state, ecc.

- resolution:

  Either resolution level as given by the data distributor, or generic
  such as "high", "low", or "default

- file_type:

  By defaults, it creates folder for zip, shp, and rds files.
