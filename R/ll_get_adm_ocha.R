#' Get administrative boundary lines from OCHA database
#'
#' Source: https://data.humdata.org/
#'
#' @param geo A twe letter country code, such as "IT" for Italy and "DE" for Germany
#' @param match_name A name to be used for local caching if a subset of the grid is used. It is the responsibility of the user to keept it consistent. If not given, data are not cached locally.
#' @param source_url A direct link to the zipped version of the csv file in the original database, if automatic download with the country code does not work. For example, for Italy this would be "https://data.humdata.org/dataset/0eb77b21-06be-42c8-9245-2edaff79952f/resource/1e96f272-7d86-4108-b4ca-5a951a8b11a0/download/population_ita_2019-07-01.csv.zip"
#' @param silent
#'
#' @return
#' @export
#'
#' @examples
#' 
#' if (interactive) {
#'   ll_get_adm_ocha(geo = "UA", level = 3)
#' }
ll_get_adm_ocha <- function(geo,
                            level = 0,
                            match_name = NULL,
                            source_url = NULL,
                            silent = FALSE) {
  if (silent == FALSE) {
    
  }
  
  if (is.null(geo) == FALSE) {
    geo <- stringr::str_to_upper(string = geo)
  }
  
  current_slice <- latlon2map::ll_administrative_boundaries_ocha_metadata %>% 
    dplyr::filter(.data$country_code==geo)  %>% 
    dplyr::distinct()
  
  if (is.null(source_url)==TRUE) {
    source_url <- current_slice %>% 
      dplyr::pull(download_url)
  }
  
  
  year <- current_slice %>% 
    dplyr::pull(last_modified) %>% 
    stringr::str_extract(pattern = "[[:digit:]]{4}")
  
  if (is.null(match_name) == FALSE) {
    rds_file_location <- ll_find_file(
      geo = geo,
      level = level,
      resolution = "ocha",
      year = year,
      name = paste0(match_name),
      file_type = "rds"
    )
  } else {
    rds_file_location <- ll_find_file(
      geo = geo,
      level = level,
      resolution = "ocha",
      year = year,
      name = paste0("ocha_administrative"),
      file_type = "rds"
    )
  }
  
  if (fs::file_exists(rds_file_location)) {
    return(readr::read_rds(file = rds_file_location))
  }
  
  
  ll_create_folders(
    geo = geo,
    level = level,
    resolution = "ocha",
    year = year,
    file_type = c("zip", "shp", "rds")
  )
  
  
  if (fs::file_exists(rds_file_location)) {
    sf <- readr::read_rds(file = rds_file)
    return(sf)
  }
  
  shp_folder <- ll_find_file(
    geo = geo,
    level = level,
    resolution = "ocha",
    year = year,
    name =  paste0("ocha_administrative"),
    file_type = "shp"
  ) 
  
  zip_file <- ll_find_file(
    geo = geo,
    level = level,
    resolution = "ocha",
    year = year,
    name = paste0("ocha_administrative"),
    file_type = "zip"
  ) 
  
  if (fs::file_exists(zip_file) == FALSE) {
    download.file(
      url = source_url,
      destfile = zip_file
    )
  }
  
  file_name <- stringr::str_split(source_url, "/") %>%
    unlist() %>%
    dplyr::last() %>%
    stringr::str_replace("_csv\\.zip$|\\.csv\\.zip$", ".csv") %>%
    stringr::str_to_lower()
  
  if (fs::file_exists(fs::path(shp_folder, file_name)) == FALSE) {
    unzip(
      zipfile = zip_file,
      exdir = shp_folder,
      junkpaths = TRUE
    )
    fs::dir_walk(
      path = shp_folder,
      fun = function(x) {
        fs::file_move(
          path = x,
          new_path = fs::path(
            fs::path_dir(x),
            stringr::str_to_lower(fs::path_file(x))
          )
        )
      }
    )
  }
  
  all_shp_files_df <- tibble::tibble(file_location = fs::dir_ls(path = shp_folder, recurse = FALSE, type = "file", glob = "*.shp")) %>% 
    dplyr::mutate(file = fs::path_file(.data$file_location)) %>% 
    dplyr::mutate(level = stringr::str_extract(string = .data$file, pattern = "[[:digit:]]+"))
  
  selected_level <- level
  
  current_level_shp_file <- all_shp_files_df %>% 
    dplyr::filter(.data$level == as.character(selected_level)) %>% 
    dplyr::slice(1) %>% 
    dplyr::pull(file_location)
  
  current_sf <- sf::st_read(current_level_shp_file) %>% 
    sf::st_transform(crs = 4326)
  
  saveRDS(
    object = current_sf,
    file = rds_file_location
  )
  
  return(current_sf)
}

