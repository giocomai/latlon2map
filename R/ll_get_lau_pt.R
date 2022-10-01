#' Regions and provinces in Italy (high detail, CC-BY Istat)
#'
#' Source: https://dados.gov.pt/pt/datasets/freguesias-de-portugal/
#'
#' @param year Defaults to 2017 (latest and currently only available).
#' @param level Defaults to "freguesia". Valid value include "freguesia", "concelho", "distrito", "des_simpli".
#' @param id A character vector composed of six digits. Corresponds to "dicofre". 
#'
#' @return
#' @export
#'
#' @examples
#' ll_set_folder(fs::path(fs::path_home_r(), "R"))
#' ll_get_lau_pt()
#' ll_get_lau_pt(name = "Porto")
ll_get_lau_pt <- function(id = NULL, 
                          name = NULL,
                          year = 2017,
                          level = "concelho",
                          silent = FALSE) {
  if (silent == FALSE) {
    usethis::ui_info(x = "Source: https://dados.gov.pt/pt/datasets/freguesias-de-portugal/")
    usethis::ui_info(x = "dados.gov.pt (CC-BY)")
  }
  
  if (is.null(name) == FALSE) {
    name <- stringr::str_to_upper(name)
    rds_file_location <- ll_find_file(
      geo = "pt",
      level = level,
      resolution = "standard",
      year = year,
      fs::path_sanitize(paste0(level, "-", stringr::str_replace_all(string = name, pattern = "[[:punct:]]", replacement = "_"))),
      file_type = "rds"
    )
    
    if (fs::file_exists(rds_file_location)) {
      return(readr::read_rds(file = rds_file_location))
    }
  }
  
  if (is.null(id) == FALSE) {
    rds_file_location <- ll_find_file(
      geo = "pt",
      level = level,
      resolution = "standard",
      year = year,
      fs::path_sanitize(paste0(level, "-", stringr::str_replace_all(string = id, pattern = "[[:punct:]]", replacement = "_"))),
      file_type = "rds"
    )
    
    if (fs::file_exists(rds_file_location)) {
      return(readr::read_rds(file = rds_file_location))
    }
  }
  
  
  rds_file <- ll_find_file(
    geo = "pt",
    level = level,
    resolution = "standard",
    year = year,
    name = "abl",
    file_type = "rds"
  )
  
  if (fs::file_exists(rds_file)) {
    sf <- readr::read_rds(file = rds_file)
  } else {
    ll_create_folders(
      geo = "pt",
      level = level,
      resolution = "standard",
      year = year
    )
    ll_create_folders(
      geo = "pt",
      level = "all_levels",
      resolution = "standard",
      year = year
    )
    
    shp_folder <- ll_find_file(
      geo = "pt",
      level = "all_levels",
      resolution = "standard",
      year = year,
      name = "abl",
      file_type = "shp"
    )
   
    source_url <- paste0("https://dados.gov.pt/s/resources/freguesias-de-portugal/20181112-195834/cont-aad-caop2017.zip")
    
    zip_file <- ll_find_file(
      geo = "pt",
      level = "all_levels",
      resolution = "standard",
      year = year,
      name = "abl",
      file_type = "zip"
    )
    
    
    if (fs::file_exists(zip_file) == FALSE) {
        download.file(url = source_url, destfile = zip_file) 
    }
    
    unzip(zipfile = zip_file, exdir = shp_folder)
    
  
    sf <- sf::read_sf(shp_folder)
    
    sf <- sf %>%
      sf::st_transform(crs = 4326)
    
    saveRDS(object = sf, file = rds_file)
  }
  
  if (is.null(name) == FALSE) {
    if (level == "freguesia") {
      sf <- sf %>%
        dplyr::filter(Freguesia == name)
    } else if (level == "concelho") {
      sf <- sf %>%
        dplyr::filter(Concelho == name)
    } else if (level == "distrito") {
      sf <- sf %>%
        dplyr::filter(Distrito == name)
    } else if (level == "ses_simpli") {
      sf <- sf %>%
        dplyr::filter(Des_Simpli == name)
    }
    
    saveRDS(
      object = sf,
      file = rds_file_location
    )
  }
  
  if (is.null(id) == FALSE) {
    if (level == "concelho") {
      current_id <- id
      current_concelho <- ll_lau_pt_id %>% 
        dplyr::filter(id == current_id) %>% 
        dplyr::pull(Concelho)
      sf <- sf %>%
        dplyr::filter(Concelho == current_concelho) %>% 
        dplyr::group_by(Concelho) %>% 
        dplyr::summarise() %>% 
        dplyr::ungroup()
    } 
    
    saveRDS(
      object = sf,
      file = rds_file_location
    )
  }
  return(sf)
}
