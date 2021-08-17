## code to prepare `ll_osm_bboxes` dataset goes here

library("latlon2map")
options(timeout = 60000)
ll_set_folder(path = fs::path(fs::path_home_r(),
                              "R",
                              "ll_data"))

# ll_get_lau_eu() %>% 
#   dplyr::pull(GISCO_ID) %>% 
#   stringr::str_extract(pattern = "[A-Z][A-Z]") %>% 
#   unique()

countries_with_more <- ll_osm_countries %>% 
  tidyr::unnest(link) %>% 
  dplyr::group_by(continent, country) %>% 
  dplyr::add_count(name = "n") %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(n>1) %>% 
  dplyr::distinct(country) %>% 
  dplyr::pull(country)
  
temp_bbox_folder <- fs::path(latlon2map::ll_set_folder(), "temp_bbox")
fs::dir_create(path = temp_bbox_folder)

ll_osm_bboxes_pre <- purrr::map_dfr(
  .x = countries_with_more,
  .f = function(current_country) {
    #current_country <- "us"
    #ll_osm_download(countries = current_country)
    # ll_osm_extract_roads(countries = current_country)
    
    
    current_country_regions <- ll_osm_countries %>% 
      dplyr::filter(country==current_country) %>% 
      tidyr::unnest(link) %>% 
      dplyr::pull(link) %>% 
      fs::path_file() %>% 
      fs::path_ext_remove() %>% 
      stringr::str_remove(pattern = stringr::fixed("-latest-free.shp"))
    
    # all_regions <- fs::dir_ls(fs::path(latlon2map::ll_set_folder(),
    #                                    "osm_roads_shp",
    #                                    current_country))
    
    all_regions <- fs::path(latlon2map::ll_set_folder(),
                                       "osm_roads_shp",
                                       current_country, 
                            current_country_regions
                            )
    
    purrr::map_dfr(.x = all_regions,
                   .f = function(current_region) {
                     
                     current_region_name <- current_region %>% fs::path_file()
                     current_region_file <- fs::path(temp_bbox_folder,
                                                     paste0(current_country,
                                                            "-",
                                                            current_region_name,".rds"))
                     
                     
                     
                   
                     if (fs::file_exists(current_region_file)) {
                       readr::read_rds(file = current_region_file)
                     } else {
                       current_region_sf <- sf::st_read(current_region)
                       
                       current_bbox <- tibble::tibble(country = current_country,
                                                      region = current_region_name, 
                                                      bbox = list(sf::st_bbox(current_region_sf)))
                       readr::write_rds(x = current_bbox, file = current_region_file)
                       current_bbox
                     }

                   })
    
  })

# countrycode::codelist %>% dplyr::select(country.name.en, iso2) %>% View()
# ll_osm_bboxes %>% 
#   dplyr::distinct(country) %>% 
#   dplyr::pull(country) %>% dput()


cc <- tibble::tribble(~country, ~country_code, 
                "japan", "JP",
                "france", "FR",
                "germany", "DE",
                "great-britain", "UK",
                "italy", "IT",
                "poland", "PL",
                "canada", "CA",
                "us", "US",
                "brazil", "BR",
                "guatemala", "GT")

ll_osm_bboxes <- ll_osm_bboxes_pre %>% 
  dplyr::left_join(y = cc, by = "country") %>% 
  dplyr::select(country_code, country, region, bbox)

usethis::use_data(ll_osm_bboxes, overwrite = TRUE)
