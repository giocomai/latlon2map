# Run the Shiny Application

Run the Shiny Application

## Usage

``` r
ll_app(max_file_size = 100, ll_folder_path = NULL, ...)
```

## Arguments

- max_file_size:

  Maximum file size to accept for upload expressed in MB, defaults to
  100.

- ll_folder_path:

  If given, sets the folder to use for caching, corresponds to
  [`ll_set_folder()`](https://giocomai.github.io/latlon2map/reference/ll_set_folder.md).
  Useful e.g. for Docker deployments. Defaults to NULL.

- ...:

  A series of options to be used inside the app.
