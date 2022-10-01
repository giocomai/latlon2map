## code to prepare `ll_lau_pt_id` dataset goes here

library("latlon2map")
library("tidywikidatar")

freg_sf <- ll_get_lau_pt()

conc_df <- tw_query(query = list(
  c(p = "P31", q = "Q13217644")
), language = "pt")

## Lagoa municipality in Acores apparently not included
conc_df <- conc_df %>% 
  dplyr::filter(id != "Q564759")

# freg_sf %>% 
#   dplyr::filter(Concelho=="LAGOA")

# freg_sf %>%
#   sf::st_drop_geometry() %>% View()

ll_lau_pt_id_pre <- freg_sf %>% 
  sf::st_drop_geometry() %>% 
  dplyr::distinct(Concelho,Distrito) %>% 
  dplyr::left_join(y = conc_df %>% 
                     dplyr::mutate(Concelho = stringr::str_to_upper(label)) %>% 
                     dplyr::select(id, Concelho),
                   by = "Concelho")

#ll_lau_pt_id_pre$id[ll_lau_pt_id_pre$Concelho=="LISBOA"] <- "Q597"
ll_lau_pt_id_pre$id[ll_lau_pt_id_pre$Concelho=="CASTANHEIRA DE PÊRA"] <- "Q1013140"

ll_lau_pt_id_pre %>% 
  dplyr::filter(is.na(id)) 

ll_lau_pt_id <- ll_lau_pt_id_pre %>% 
  dplyr::rename(qid = id) %>% 
  dplyr::mutate(id = stringr::str_c("PT_", qid)) %>% 
  dplyr::mutate(population = tw_get_p1(id = qid, p = "P1082", language = "pt")) %>%
  dplyr::mutate(population = stringr::str_remove(string = population, pattern = stringr::fixed("+")) %>% as.numeric()) %>%
  dplyr::mutate(name = tw_get_label(id = qid, language = "pt")) %>% 
  dplyr::arrange(dplyr::desc(population))

ll_lau_pt_id 

# freg_sf %>% 
#   dplyr::filter(Concelho == "ALBUFEIRA") %>% 
#   dplyr::summarise()

usethis::use_data(ll_lau_pt_id, overwrite = TRUE)
