## code to prepare `ll_osm_it_gpkg` dataset goes here

# https://osmit-estratti.wmcloud.org/

ll_osm_it_gpkg <- list()

link_comuni_pre <- "https://osmit-estratti.wmcloud.org/dati/poly/comuni/geopackage/" %>%
  xml2::read_html() %>%
  rvest::html_nodes(xpath = paste0("//a")) %>%
  xml2::xml_attr("href") %>%
  tibble::enframe(name = NULL, value = "link") %>%
  dplyr::slice(-1)


library("latlon2map")

ll_set_folder(path = fs::path(fs::path_home_r(), "R", "ll_data"))

ll_osm_it_gpkg[["comuni"]] <- ll_get_nuts_it(level = "lau") %>%
  sf::st_drop_geometry() %>%
  dplyr::select(PRO_COM_T, COMUNE) %>%
  dplyr::left_join(
    y = link_comuni_pre %>%
      dplyr::mutate(PRO_COM_T = stringr::str_extract(
        string = link,
        pattern = "[[:digit:]]+"
      )),
    by = "PRO_COM_T"
  ) %>%
  dplyr::transmute(
    name = COMUNE,
    code = PRO_COM_T,
    link = paste0("https://osmit-estratti.wmcloud.org/dati/poly/comuni/geopackage/", link)
  ) %>%
  dplyr::filter(is.na(link) == FALSE)


## province

link_province_pre <- "https://osmit-estratti.wmcloud.org/dati/poly/province/geopackage/" %>%
  xml2::read_html() %>%
  rvest::html_nodes(xpath = paste0("//a")) %>%
  xml2::xml_attr("href") %>%
  tibble::enframe(name = NULL, value = "link") %>%
  dplyr::slice(-1)



ll_osm_it_gpkg[["province"]] <- ll_get_nuts_it(level = 3) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(COD_PROV, DEN_UTS) %>%
  dplyr::mutate(COD_PROV = stringr::str_pad(string = COD_PROV, width = 3, side = "left", pad = 0)) %>%
  dplyr::left_join(
    y = link_province_pre %>%
      dplyr::mutate(COD_PROV = stringr::str_extract(
        string = link,
        pattern = "[[:digit:]]+"
      )),
    by = "COD_PROV"
  ) %>%
  dplyr::transmute(
    name = DEN_UTS,
    code = COD_PROV,
    link = paste0("https://osmit-estratti.wmcloud.org/dati/poly/province/geopackage/", link)
  )


## regioni


link_regioni_pre <- "https://osmit-estratti.wmcloud.org/dati/poly/regioni/geopackage/" %>%
  xml2::read_html() %>%
  rvest::html_nodes(xpath = paste0("//a")) %>%
  xml2::xml_attr("href") %>%
  tibble::enframe(name = NULL, value = "link") %>%
  dplyr::slice(-1)



ll_osm_it_gpkg[["regioni"]] <- ll_get_nuts_it(level = 2) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(COD_REG, DEN_REG) %>%
  dplyr::mutate(COD_REG = stringr::str_pad(string = COD_REG, width = 2, side = "left", pad = 0)) %>%
  dplyr::left_join(
    y = link_regioni_pre %>%
      dplyr::mutate(COD_REG = stringr::str_extract(
        string = link,
        pattern = "[[:digit:]]+"
      )),
    by = "COD_REG"
  ) %>%
  dplyr::transmute(
    name = DEN_REG,
    code = COD_REG,
    link = paste0("https://osmit-estratti.wmcloud.org/dati/poly/regioni/geopackage/", link)
  )



usethis::use_data(ll_osm_it_gpkg, overwrite = TRUE)
