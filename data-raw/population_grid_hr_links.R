## code to prepare `population_grid_hr_links` dataset goes here

# remotes::install_gitlab("dickoa/rhdx")
# https://gitlab.com/dickoa/rhdx
library("rhdx")
set_rhdx_config(hdx_site = "prod")
get_rhdx_config()

library("tidyverse")
hr1 <- search_datasets(
  query = "High Resolution Population Density Maps",
  rows = 1000
)

hr2 <- search_datasets(
  query = "High Resolution Population Density Maps",
  rows = 1000,
  start = 1000
)

hr <- c(hr1,hr2)

pb <- progress::progress_bar$new(total = length(hr))

all_available_df <- purrr::map_dfr(
  .x = hr,
  .f = function(x) {
    pb$tick()
    current_resources <- x %>% get_resources()

    # pb <- progress::progress_bar$new(total = length(current_resources))
    purrr::map_dfr(
      .x = current_resources,
      .f = function(current_resource) {
        #                 pb$tick()
        resource_list <- current_resource$as_list()

        resource_list[names(resource_list) == ""] <- NULL

        resource_df <- resource_list %>%
          tibble::enframe() %>%
          tidyr::pivot_wider() %>%
          tidyr::unnest(cols = dplyr::everything()) %>%
          mutate(across(everything(), as.character))

        # if (is.element("originalHash", names(resource_df))) {
        #   resource_df$originalHash <- as.character(resource_df$originalHash)
        # }
        # if (is.element("pii", names(resource_df))) {
        #   resource_df$pii <- as.character(resource_df$pii)
        # }
        resource_df
      }
    ) %>%
      dplyr::mutate(
        title = x$as_list()$title,
        country = list(x$as_list()$solr_additions %>% jsonlite::parse_json() %>% unlist()),
        dataset_source = x$as_list()$dataset_source,
        dataset_name = x$as_list()[["name"]]
      )
  }
)


population_grid_hr_metadata <- all_available_df %>%
  dplyr::filter(dataset_source == "Facebook") %>%
  dplyr::filter(url_type == "upload") %>%
  dplyr::select(
    package_id,
    id,
    size,
    metadata_modified,
    download_url,
    format,
    position,
    name,
    created,
    last_modified,
    title,
    country,
    dataset_name
  ) %>%
  tidyr::unnest(country) %>%
  dplyr::mutate(country_code = countrycode::countrycode(
    sourcevar = country,
    origin = "country.name.en",
    destination = "eurostat"
  )) %>%
  dplyr::mutate(url = stringr::str_c("https://data.humdata.org/dataset/", dataset_name)) %>%
  dplyr::select(
    country_code,
    country,
    format,
    download_url,
    name, title,
    dplyr::everything()
  ) %>%
  dplyr::group_by(country, name) %>%
  dplyr::slice_max(last_modified) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(country_code, country, format, name)


usethis::use_data(population_grid_hr_metadata,
  overwrite = TRUE
)
