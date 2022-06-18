## code to prepare `ll_osm_countries` dataset goes here

library("dplyr")

continents <- c(
  "africa",
  "asia",
  "australia-oceania",
  "central-america",
  "europe",
  "north-america",
  "south-america",
  "russia"
)

ll_osm_countries <-
  purrr::map_dfr(
    .x = continents,
    .f = function(current_continent) {
      print(current_continent)
      current_continent_page <- xml2::read_html(x = glue::glue("http://download.geofabrik.de/{current_continent}/"))

      current_links <- current_continent_page %>%
        rvest::html_nodes(xpath = paste0("//a")) %>%
        xml2::xml_attr("href") %>%
        tibble::enframe(name = NULL, value = "links")

      current_countries_df <- current_links %>%
        dplyr::filter(stringr::str_detect(
          string = links,
          pattern = stringr::fixed(".html")
        )) %>%
        dplyr::transmute(country = fs::path_ext_remove(links))

      small_countries_df <-
        current_links %>%
        dplyr::filter(stringr::str_ends(
          string = links,
          pattern = "-latest-free.shp.zip"
        )) %>%
        dplyr::transmute(
          continent = current_continent,
          country = stringr::str_remove(links, "-latest-free.shp.zip"),
          link = glue::glue("http://download.geofabrik.de/{current_continent}/{links}")
        ) %>%
        tidyr::nest(link = c(link))


      big_countries_c <- dplyr::anti_join(
        x = current_countries_df,
        y = small_countries_df,
        by = "country"
      )


      big_countries_df <- purrr::map_dfr(
        .x = big_countries_c$country,
        .f = function(current_big_country) {
          current_big_country_links_pre <- xml2::read_html(x = paste0(
            "http://download.geofabrik.de/",
            current_continent,
            "/",
            current_big_country,
            ".html"
          )) %>%
            rvest::html_nodes(xpath = paste0("//a")) %>%
            xml2::xml_attr("href") 
          
          current_big_country_links <- current_big_country_links_pre %>% 
            stringr::str_subset(pattern = "-latest-free.shp.zip$")
          
          all_regions_html_links <- current_big_country_links_pre[stringr::str_starts(string = current_big_country_links_pre, current_big_country)&stringr::str_ends(string = current_big_country_links_pre, "html")]
          
          available_shp_regions_v <- stringr::str_remove(string = current_big_country_links, pattern = stringr::str_c(current_big_country, "/")) %>% 
            stringr::str_remove(pattern = "-latest-free.shp.zip")
          
          all_regions_v <- stringr::str_remove(string = all_regions_html_links, pattern = stringr::str_c(current_big_country, "/")) %>% 
            stringr::str_remove(pattern = ".html")
          
          big_regions_v <- all_regions_v[!(is.element(all_regions_v, available_shp_regions_v))]

          if (length(big_regions_v)>0) {
            
            big_regions_links <- purrr::map_dfr(.x = big_regions_v,
                           .f = function(current_region) {
                             xml2::read_html(x = paste0(
                               "http://download.geofabrik.de/",
                               current_continent,
                               "/",
                               current_big_country,
                               "/",
                               current_region,
                               ".html"
                             )) %>%
                               rvest::html_nodes(xpath = paste0("//a")) %>%
                               xml2::xml_attr("href")  %>%
                               tibble::enframe(name = NULL, value = "links") %>%
                               dplyr::filter(stringr::str_ends(
                                 string = links,
                                 pattern = "-latest-free.shp.zip"
                               ))
            })
            
            current_big_country_links <- c(current_big_country_links,
                                           stringr::str_c(current_big_country, "/", big_regions_links$links))

          }

          
          if (length(current_big_country_links) == 0) {
            return(NULL)
          } else {
            tibble::tibble(
              continent = current_continent,
              country = current_big_country,
              link = glue::glue("http://download.geofabrik.de/{current_continent}/{current_big_country_links}")
            ) %>%
              dplyr::group_by(continent, country) %>%
              tidyr::nest(link = link)
          }
        }
      )

      if (current_continent == "russia") {
        tibble::tibble(continent = "russia", 
                       country = "russia", 
                       link = list(small_countries_df %>% 
          tidyr::unnest(link) %>% 
          dplyr::select(link)))
      } else {
        dplyr::bind_rows(
          small_countries_df,
          big_countries_df
        )
      }
    }
  ) %>%
  arrange(continent, country)

# fix Canary Islands
ll_osm_countries[ll_osm_countries$country=="spain", "link"][[1]] <- list(dplyr::bind_rows(ll_osm_countries[ll_osm_countries$country=="spain", "link"][[1]],
          ll_osm_countries[ll_osm_countries$country=="canary-islands", "link"][[1]]) %>% 
            dplyr::distinct(.data$link))


usethis::use_data(ll_osm_countries , overwrite = TRUE)


temp <- sf::st_read("/home/g/Downloads/east-timor-latest.osm.pbf", layer = "lines")
temp %>% 
  filter(is.na(highway)==FALSE)

temp2 <- sf::st_read("/home/g/Downloads/east-timor-latest-free.shp/gis_osm_roads_free_1.shp")
