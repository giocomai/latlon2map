## code to prepare `ll_osm_countries` dataset goes here

library("dplyr")

continents <- c(
  "africa",
  "asia",
  "australia-oceania",
  "central-america",
  "europe",
  "north-america",
  "south-america"
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
          current_big_country_links <- xml2::read_html(x = paste0(
            "http://download.geofabrik.de/",
            current_continent,
            "/",
            current_big_country,
            ".html"
          )) %>%
            rvest::html_nodes(xpath = paste0("//a")) %>%
            xml2::xml_attr("href") %>%
            stringr::str_subset(pattern = "-latest-free.shp.zip$")
          
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
      
      dplyr::bind_rows(
        small_countries_df,
        big_countries_df
      )
    }
  )



usethis::use_data(ll_osm_countries, overwrite = TRUE)
