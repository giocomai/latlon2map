## code to prepare `ll_codes` dataset goes here

lau_df <- ll_get_lau_eu() %>% 
  sf::st_drop_geometry() %>% 
  dplyr::transmute(country_code = CNTR_CODE,
                   id = GISCO_ID, 
                   name = LAU_NAME,
                   source = "ll_get_lau_eu()")

nuts3_df <- ll_get_nuts_eu(level = 3) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::transmute(country_code = CNTR_CODE,
                   id = NUTS_ID, 
                   name = NAME_LATN,
                   source = "ll_get_nuts_eu(level = 3)")

ua2_df <- ll_get_gadm(geo = "UKR", level = 2) %>%
  sf::st_drop_geometry() %>% 
  dplyr::transmute(country_code = "UA",
                   id = stringr::str_c("UA_", GID_2), 
                   name = NAME_2,
                   source = "ll_get_gadm(geo = 'UKR', level = 2)")

ua1_df <- ll_get_gadm(geo = "UKR", level = 1) %>%
  sf::st_drop_geometry() %>% 
  dplyr::transmute(country_code = "UA",
                   id = stringr::str_c("UA_", GID_1), 
                   name = NAME_1,
                   source = "ll_get_gadm(geo = 'UKR', level = 1)")

# ua_df <- ll_get_adm_ocha(geo = "UA", level = 3) %>% 
#   sf::st_drop_geometry()

md1_df <- ll_get_adm_ocha(geo = "MD", level = 1) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::transmute(country_code = "MD",
                   id = ADM1_PCODE, 
                   name = ADM1_EN,
                   source = "ll_get_adm_ocha(geo = 'MD', level = 1)")

rs1_df <- ll_get_gadm(geo = "RS", level = 1) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::transmute(country_code = "RS",
                   id = stringr::str_c("RS_", GID_1), 
                   name = NAME_1,
                   source = "ll_get_gadm(geo = 'RS', level = 1)")

ba3_df <- ll_get_gadm(geo = "BIH", level = 3) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::transmute(country_code = "BA",
                   id = stringr::str_c("BA_", GID_3), 
                   name = NAME_3,
                   source = "ll_get_gadm(geo = 'BIH', level = 3)")

ba2_df <- ll_get_gadm(geo = "BIH", level = 2) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::transmute(country_code = "BA",
                   id = stringr::str_c("BA_", GID_2), 
                   name = NAME_2,
                   source = "ll_get_gadm(geo = 'BIH', level = 2)")

xk2_df <- ll_get_gadm(geo = "XKO", level = 2) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::transmute(country_code = "XK",
                   id = GID_2, 
                   name = NAME_2,
                   source = "ll_get_gadm(geo = 'XKO', level = 2)")


xk1_df <- ll_get_gadm(geo = "XKO", level = 1) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::transmute(country_code = "XK",
                   id = GID_1, 
                   name = NAME_1,
                   source = "ll_get_gadm(geo = 'XKO', level = 1)")

me1_df <- ll_get_gadm(geo = "MNE", level = 1) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::transmute(country_code = "ME",
                   id = stringr::str_c("ME_", GID_1), 
                   name = NAME_1,
                   source = "ll_get_gadm(geo = 'ME', level = 1)")

#tidywikidatar::tw_get_label(id = ll_lau_pt_id$qid, language = "pt")

pt_df <- ll_lau_pt_id %>% 
  dplyr::transmute(country_code = "PT",
                   id, 
                   name = Concelho,
                   source = "ll_get_lau_pt(level = 'concelho')")

ll_codes <- dplyr::bind_rows(lau_df,
                             nuts3_df,
                             ua1_df,
                             ua2_df,
                             md1_df,
                             rs1_df,
                             ba2_df,
                             ba3_df,
                             xk2_df,
                             xk1_df,
                             pt_df) %>% 
  dplyr::arrange(country_code, id, name)


usethis::use_data(ll_codes, overwrite = TRUE)
