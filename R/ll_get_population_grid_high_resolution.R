#' Get High Resolution Population Density Maps + Demographic Estimates
#'
#' Source: https://data.humdata.org/organization/facebook
#' Details on methodology: https://dataforgood.fb.com/docs/methodology-high-resolution-population-density-maps-demographic-estimates/
#' 
#' @param geo A twe letter country code, such as "IT" for Italy and "DE" for Germany
#' @param match_sf An sf object to me matched with the population grid. If not given, full grid is returned.
#' @param match_name A name to be used for local caching. It is the responsibility of the user to keept it consistent. If not given, data are not cached locally.
#' @param source_url A direct link to the zipped version of the csv file in the original database, if automatic download with the country code does not work. For example, for Italy this would be "https://data.humdata.org/dataset/0eb77b21-06be-42c8-9245-2edaff79952f/resource/1e96f272-7d86-4108-b4ca-5a951a8b11a0/download/population_ita_2019-07-01.csv.zip"
#' @param join 
#' @param silent 
#'
#' @return
#' @export
#'
#' @examples
ll_get_population_grid_hr <- function(geo,
                                      match_sf = NULL,
                                      match_name = NULL,
                                      join = sf::st_intersects,
                                      source_url = NULL,
                                      silent = FALSE) {
  if (silent==FALSE) {
    usethis::ui_info(x = paste("Facebook Connectivity Lab and Center for International Earth Science Information Network - CIESIN - Columbia University. 2016. High Resolution Settlement Layer (HRSL). Source imagery for HRSL © 2016 DigitalGlobe. Accessed", Sys.Date()))
    usethis::ui_info(x = "License: Creative Commons Attribution International")
    usethis::ui_info(x = "Source: https://data.humdata.org/organization/facebook")
  }
  
  if (is.null(geo)==FALSE) {
    geo <- stringr::str_to_upper(string = geo)
  }
  
  if (is.null(match_name)==FALSE) {
    rds_file_location <- ll_find_file(geo = geo,
                                      level = 0,
                                      resolution = "hr",
                                      year = 2020,
                                      name = paste0(match_name, "-hr_population_grid", "-", "geo"),
                                      file_type = "rds")
    
    if (fs::file_exists(rds_file_location)) {
      return(readr::read_rds(rds_file_location))
    }
  }
  
  ll_create_folders(geo = geo,
                    level = 0,
                    resolution = "hr",
                    year = 2020,
                    file_type = c("zip", "csv", "rds"))
  
  rds_file <- ll_find_file(geo = geo,
                           level = 0,
                           resolution = "hr",
                           year = 2020,
                           name = paste0("population_grid_hr", "-", geo),
                           file_type = "rds")
  
  if (fs::file_exists(rds_file)&is.null(match_sf)) {
    sf <- readr::read_rds(path = rds_file)
  } else {
    csv_folder <- ll_find_file(geo = geo,
                               level = 0,
                               resolution = "hr",
                               year = 2020,
                               name = paste0("population_grid", "-", geo),
                               file_type = "csv") %>% fs::path_dir()
    
    zip_file <- ll_find_file(geo = geo,
                             level = 0,
                             resolution = "hr",
                             year = 2020,
                             name = paste0("population_grid", "-", geo),
                             file_type = "zip")
    if (is.null(source_url)==FALSE) {
      #do nothing
    } else if (geo == "IT") {
      source_url <- "https://data.humdata.org/dataset/0eb77b21-06be-42c8-9245-2edaff79952f/resource/1e96f272-7d86-4108-b4ca-5a951a8b11a0/download/population_ita_2019-07-01.csv.zip"
    } else if (geo == "DE") {
      source_url <- "https://data.humdata.org/dataset/7d08e2b0-b43b-43fd-a6a6-a308f222cdb2/resource/77a44470-f80a-44be-9bb2-3e904dbbe9b1/download/population_deu_2019-07-01.csv.zip"
    } else if (geo == "UK") {
      source_url <- "https://data.humdata.org/dataset/b9a7b4a3-75a7-4de1-b741-27d78e8d0564/resource/674a0049-1a75-4f9a-a07b-654bda75456e/download/population_gbr_2019-07-01.csv.zip"
    } else if (geo == "PL") {
      source_url <- "https://data.humdata.org/dataset/d3bb62ab-b94d-4042-8e76-821fe17ce562/resource/9dc916a5-969c-4561-8c5c-23c96f9fedb0/download/population_pol_2019-07-01.csv.zip"
    } else if (geo == "ES") {
      source_url <- "https://data.humdata.org/dataset/80d0519e-0eaf-4c16-a16c-a10ca837a463/resource/f75d0b98-c2ca-4882-85b7-ab2c91ee78f4/download/population_esp_2019-07-01.csv.zip"
    } else {
      usethis::ui_todo("URL for this country not available. Provide direct link to zipped csv file as `source_url` parameter.")
      usethis::ui_todo("All datasets available from the following page: https://data.humdata.org/organization/facebook")
      usethis::ui_stop(paste("source_url needed when geo is", sQuote(geo)))
    }
    
    if (fs::file_exists(zip_file)==FALSE) {
      download.file(url = source_url,
                    destfile = zip_file)
    }
    
    if (fs::file_exists(fs::path(csv_folder, "population_ita_2019-07-01.csv"))==FALSE) {
      unzip(zipfile = zip_file,
            exdir = csv_folder)
    }
    df <- readr::read_csv(file = fs::path(csv_folder, "population_ita_2019-07-01.csv"),
                          col_types = readr::cols(
                            Lat = readr::col_double(),
                            Lon = readr::col_double(),
                            Population = readr::col_double()
                          ))
    if (is.null(match_sf)==FALSE) {
      
      bbox <- sf::st_bbox(match_sf)
      df <- df %>% 
        dplyr::filter(Lat>=bbox$ymin, Lat<=bbox$ymax, Lon>=bbox$xmin, Lat<=bbox$ymax)
      
      sf <- df %>% 
        sf::st_as_sf(coords = c("Lon", "Lat"), crs = 4326)
      
      sf <- sf::st_filter(x = sf %>% sf::st_transform(crs = 3857),
                          y = match_sf %>% sf::st_transform(crs = 3857),
                          join = join) %>%
        sf::st_transform(crs = 4326)
      
      if (is.null(match_name)==FALSE) {
        readr::write_rds(x = sf,
                         path = rds_file_location)
      }
      
      return(sf)
      
    }
    sf <- df %>% 
      sf::st_as_sf(coords = c("Lon", "Lat"), crs = 4326)
    
    readr::write_rds(x = sf,
                     path = rds_file)
  }

  return(sf)
  
}