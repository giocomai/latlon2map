## code to prepare `ll_osm_bboxes` dataset goes here

library("latlon2map")
options(timeout = 60000)
ll_set_folder(path = fs::path(
  fs::path_home_r(),
  "R",
  "ll_data"
))

# ll_get_lau_eu() %>%
#   dplyr::pull(GISCO_ID) %>%
#   stringr::str_extract(pattern = "[A-Z][A-Z]") %>%
#   unique()

countries_with_more <- ll_osm_countries %>%
  tidyr::unnest(link) %>%
  dplyr::group_by(continent, country) %>%
  dplyr::add_count(name = "n") %>%
  dplyr::ungroup() %>%
  dplyr::filter(n > 1) %>%
  dplyr::distinct(country) %>%
  dplyr::pull(country)



temp_bbox_folder <- fs::path(latlon2map::ll_set_folder(), "temp_bbox")
fs::dir_create(path = temp_bbox_folder)
ll_osm_bboxes <- ll_osm_bboxes %>% 
  filter(country!="spain")

ll_osm_bboxes_pre <- purrr::map_dfr(
  .x = countries_with_more,
  .f = function(current_country) {
    # current_country <- "spain"
    
    if (exists("ll_osm_bboxes")) {
      previous_df <- ll_osm_bboxes %>% 
        dplyr::filter(country==current_country)
      
      if (nrow(previous_df)>0) {
        return(
          previous_df %>% 
            dplyr::select(-country_code)
        )
      }
    }
    
    ll_osm_download(countries = current_country)
    ll_osm_extract_roads(countries = current_country)


    current_country_regions <- ll_osm_countries %>%
      dplyr::filter(country == current_country) %>%
      tidyr::unnest(link) %>%
      dplyr::pull(link) %>%
      fs::path_file() %>%
      fs::path_ext_remove() %>%
      stringr::str_remove(pattern = stringr::fixed("-latest-free.shp"))

    # all_regions <- fs::dir_ls(fs::path(latlon2map::ll_set_folder(),
    #                                    "osm_roads_shp",
    #                                    current_country))

    all_regions <- fs::path(
      latlon2map::ll_set_folder(),
      "osm_roads_shp",
      current_country,
      current_country_regions
    )

    purrr::map_dfr(
      .x = all_regions,
      .f = function(current_region) {
        current_region_name <- current_region %>% fs::path_file()
        current_region_file <- fs::path(
          temp_bbox_folder,
          paste0(
            current_country,
            "-",
            current_region_name,
            ".rds"
          )
        )

        if (fs::file_exists(current_region_file)) {
          readr::read_rds(file = current_region_file)
        } else {
          current_region_sf <- sf::st_read(current_region)

          current_bbox <- tibble::tibble(
            country = current_country,
            region = current_region_name,
            bbox = list(sf::st_bbox(current_region_sf))
          )
          readr::write_rds(x = current_bbox, file = current_region_file)
          current_bbox
        }
      }
    )
  }
)

# countrycode::codelist %>% dplyr::select(country.name.en, iso2) %>% View()
# ll_osm_bboxes %>%
#   dplyr::distinct(country) %>%
#   dplyr::pull(country) %>% dput()


cc <- tibble::tribble(
  ~country, ~country_code,
  "japan", "JP",
  "france", "FR",
  "germany", "DE",
  "great-britain", "UK",
  "italy", "IT",
  "netherlands", "NL",
  "poland", "PL",
  "spain", "ES",
  "russia", "RU",
  "canada", "CA",
  "us", "US",
  "brazil", "BR",
  "guatemala", "GT",
  "india", "IN"
)

## Add canary islands
# https://download.geofabrik.de/africa.html





ll_osm_bboxes <- ll_osm_bboxes_pre %>%
  dplyr::mutate(country_code = countrycode::countrycode(sourcevar = country,
                                                        origin = "country.name.en",
                                                        destination = "eurostat")) %>% 
  dplyr::select(country_code, country, region, bbox) %>% 
  dplyr::arrange(country_code)

usethis::use_data(ll_osm_bboxes, overwrite = TRUE)


### this useful only to update as new countries appear here
# 
# before_update_df <- ll_osm_bboxes
# 
# cc_new <- cc %>% 
#   dplyr::anti_join(y = ll_osm_bboxes, by = "country")
# 
# 
# 
# ll_osm_bboxes_new <- cc %>%
#   dplyr::anti_join(y = before_update_df, by = "country") 
# 
# ll_osm_bboxes <- dplyr::bind_rows(x = before_update_df,
#                                   y = ll_osm_bboxes_new) %>% 
#   dplyr::arrange(country)
# 
# usethis::use_data(ll_osm_bboxes, overwrite = TRUE)
