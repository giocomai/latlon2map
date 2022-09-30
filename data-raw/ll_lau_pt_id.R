## code to prepare `ll_lau_pt_id` dataset goes here

library("latlon2map")
library("tidywikidatar")

freg_sf <- ll_get_lau_pt()

conc_df <- tw_query(query = list(
  c(p = "P31", q = "Q13217644")
))


ll_lau_pt_id_pre <- freg_sf %>% 
  sf::st_drop_geometry() %>% 
  dplyr::distinct(Concelho) %>% 
  dplyr::left_join(y = conc_df %>% 
                     dplyr::mutate(Concelho = stringr::str_to_upper(label)) %>% 
                     dplyr::select(id, Concelho),
                   by = "Concelho") 

ll_lau_pt_id_pre$id[ll_lau_pt_id_pre$Concelho=="LISBOA"] <- "Q597"
ll_lau_pt_id_pre$id[ll_lau_pt_id_pre$Concelho=="CASTANHEIRA DE PÊRA"] <- "Q1013140"

ll_lau_pt_id_pre %>% 
  dplyr::filter(is.na(id)) 

ll_lau_pt_id <- ll_lau_pt_id_pre %>% 
  dplyr::rename(qid = id) %>% 
  dplyr::mutate(id = stringr::str_c("PT_", qid))

# freg_sf %>% 
#   dplyr::filter(Concelho == "ALBUFEIRA") %>% 
#   dplyr::summarise()

usethis::use_data(ll_lau_pt_id, overwrite = TRUE)
