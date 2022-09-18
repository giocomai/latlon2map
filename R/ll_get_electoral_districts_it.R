#' Get Italian electoral districts (CC-BY Istat)
#'
#' 2022 / WGS 84 / UTM zone 32N
#' 
#' Column names metadata:
#' 
##' \itemize{
##'  \item{COD_REG	Codice della regione/circoscrizione elettorale del Senato della Repubblica}
##'  \item{DEN_REG	Denominazione della regione amministrativa/circoscrizione elettorale Senato della Repubblica}
##'  \item{COD_PRO	Codice della provincia}
##'  \item{DEN_P_CM	Denominazione della provincia o città metropolitana}
##'  \item{COD_CM	Codice della città metropolitana}
##'  \item{PRO_COM	Codice del comune}
##'  \item{DEN_COM	Denominazione del comune}
##'  \item{CAP_DEN	Denominazione del capoluogo di provincia o città metropolitana}
##'  \item{POP_2011	Popolazione - Censimento 2011 }
##'  \item{ASC_COD	Codice concatenato comune e area sub-comunale}
##'  \item{ASC_COD1	Codice progressivo area sub-comunale}
##'  \item{ASC_COD2	Codice alfanumerico dell'area sub-comunale attribuito dal comune}
##'  \item{ASC_NOME	Denominazione dell'area sub-comunale}
##'  \item{ASC_TIPO	Tipologia di area-sub-comunale}
##'  \item{CIRC_COD	Codice della circoscrizione elettorale della Camera dei deputati}
##'  \item{CIRC_DEN	Denominazione della circoscrizione elettorale della Camera dei deputati}
##'  \item{CU20_COD	Codice del collegio elettorale uninominale della Camera dei deputati}
##'  \item{CP20_COD	Codice del collegio elettoraleplurinominale della Camera dei deputati}
##'  \item{SU20_COD	Codice del collegio elettorale uninominale del Senato della Repubblica}
##'  \item{SP20_COD	Codice del collegio elettorale plurinominale del Senato della Repubblica}
##'  \item{CU20_DEN	Denominazione del collegio elettorale uninominale della Camera dei deputati}
##'  \item{CP20_DEN	Denominazione del collegio elettorale plurinominale della Camera dei deputati}
##'  \item{SU20_DEN	Denominazione del collegio elettorale uninominale del Senato della Repubblica}
##'  \item{SP20_DEN	Denominazione del collegio elettorale plurinominale del Senato della Repubblica}
##'  \item{CU20_C1	Sigla del collegio elettorale uninominale della Camera dei deputati}
##'  \item{CP20_C1	Sigla del collegio elettorale plurinominale della Camera dei deputati}
##'  \item{SU20_C1	Sigla del collegio elettorale uninominale del Senato della Repubblica}
##'  \item{SP20_C1	Sigla del collegio elettorale plurinominale del Senato della Repubblica}
##' }
#'
#' @param level Defaults to "Circoscrizioni_Camera". Valid values:
##' \itemize{
##'  \item{"Circoscrizioni_Camera"}: Basi geografiche delle circoscrizioni elettorali - Camera dei deputati
##'  \item{"Regioni_Senato"}: Basi geografiche delle circoscrizioni elettorali - Senato della Repubblica
##'  \item{"CAMERA_CollegiPLURINOMINALI_2020"}: Basi geografiche dei collegi elettorali plurinominali - Camera dei deputati
##'  \item{"CAMERA_CollegiUNINOMINALI_2020"}: Basi geografiche dei collegi elettorali uninominali - Camera dei deputati
##'  \item{"SENATO_CollegiPLURINOMINALI_2020"}: Basi geografiche dei collegi elettorali plurinominali - Senato della Repubblica
##'  \item{"SENATO_CollegiUNINOMINALI_2020"}: Basi geografiche dei collegi elettorali uninominali - Senato della Repubblica
##'  \item{"UT_Collegi2020"}: Basi geografiche delle unità territoriali che formano i collegi elettorali (comuni e aree sub-comunali, limitatamente ai comuni di Torino, Genova, Milano, Roma, Napoli e Palermo con territorio ripsrtito su più di un collegio). Geografia comunale vigente alla data della pubblicazione
##' }
#' @param year Defaults to 2022 (latest available). Currently no other year accepted.
#' @param no_check_certificate Logical, defaults to TRUE. Enable only if certificate issues, and if you are aware of the security implications.
#'
#' @return
#' @export
#'
#' @examples
#' ll_set_folder(fs::path(fs::path_home_r(), "R"))
#' ll_get_electoral_districts_it()
#' ll_get_electoral_districts_it(name = "Lombardia 2")
#' ll_get_electoral_districts_it() %>% ggplot2::ggplot() + ggplot2::geom_sf() + ggplot2::labs(title = "Circoscrizioni Camera")
#' ll_get_electoral_districts_it(level = "SENATO_CollegiUNINOMINALI_2020") %>% ggplot2::ggplot() + ggplot2::geom_sf() + ggplot2::labs(title = "Collegi uninominali - Senato")
ll_get_electoral_districts_it <- function(name = NULL,
                           level = "Circoscrizioni_Camera",
                           year = 2022,
                           silent = FALSE, 
                           no_check_certificate = FALSE) {
  if (silent == FALSE) {
    usethis::ui_info(x = "https://www.istat.it/it/archivio/273443")
    usethis::ui_info(x = "Istat (CC-BY)")
  }
  
  if (year == 2020) {
    year <- 2022
  }
  
  resolution <- "standard"
  
  if (is.null(name) == FALSE) {
    rds_file_location <- ll_find_file(
      geo = "it_elections",
      level = level,
      resolution = resolution,
      year = year,
      name = paste0(level, "-", stringr::str_replace_all(string = name, pattern = "[[:punct:]]", replacement = "_")),
      file_type = "rds"
    )
    
    if (fs::file_exists(rds_file_location)) {
      return(readr::read_rds(file = rds_file_location))
    }
  }
  
  
  rds_file <- ll_find_file(
    geo = "it_elections",
    level = level,
    resolution = resolution,
    year = year,
    name = "electoral_districts",
    file_type = "rds"
  )
  
  if (fs::file_exists(rds_file)) {
    sf <- readr::read_rds(file = rds_file)
  } else {
    ll_create_folders(
      geo = "it_elections",
      level = level,
      resolution = resolution,
      year = year
    )
    ll_create_folders(
      geo = "it_elections",
      level = "all_levels",
      resolution = resolution,
      year = year
    )
    
    shp_folder <- ll_find_file(
      geo = "it_elections",
      level = "all_levels",
      resolution = resolution,
      year = year,
      name = "electoral_districts",
      file_type = "shp"
    )
    
    
    source_url <- "https://www.istat.it/storage/Basi%20Geografiche%202022/Collegi_Elettorali_BasiGeografiche.zip"
    
    zip_file <- ll_find_file(
      geo = "it_elections",
      level = "all_levels",
      resolution = resolution,
      year = year,
      name = "electoral_districts",
      file_type = "zip"
    )
    
    
    if (fs::file_exists(zip_file) == FALSE) {
      if (isTRUE(no_check_certificate)) {
        download.file(url = source_url, destfile = zip_file, method = "wget", extra = "--no-check-certificate")
      } else {
        download.file(url = source_url, destfile = zip_file) 
      }
    }
    
    unzip(zipfile = zip_file, exdir = shp_folder)
    
    sf <- sf::read_sf(fs::path(
      shp_folder,
      "Collegi_Elettorali_BasiGeografiche",
      level
    ))
    
    saveRDS(object = sf, file = rds_file)
  }
  
  if (is.null(name) == FALSE) {
    if (level == "Circoscrizioni_Camera") {
      sf <- sf %>%
        dplyr::filter(CIRC_DEN == name)
    } else if (level == "Regioni_Senato") {
      sf <- sf %>%
        dplyr::filter(DEN_REG == name)
    } else if (level == "CAMERA_CollegiPLURINOMINALI_2020") {
      sf <- sf %>%
        dplyr::filter(is.na(CP20_DEN)==FALSE) %>% 
        dplyr::filter(CP20_DEN == name)
    } else if (level == "CAMERA_CollegiUNINOMINALI_2020") {
      sf <- sf %>%
        dplyr::filter(CU20_DEN == name)
    } else if (level == "SENATO_CollegiPLURINOMINALI_2020") {
      sf <- sf %>%
        dplyr::filter(is.na(SP20_DEN)==FALSE) %>% 
        dplyr::filter(SP20_DEN == name)
    } else if (level == "SENATO_CollegiUNINOMINALI_2020") {
      sf <- sf %>%
        dplyr::filter(is.na(SU20_DEN)==FALSE) %>% 
        dplyr::filter(SU20_DEN == name)
    } else if (level == "SENATO_CollegiPLURINOMINALI_2020") {
      sf <- sf %>%
        dplyr::filter(is.na(SP20_DEN)==FALSE) %>% 
        dplyr::filter(SP20_DEN == name)
    } else if (level == "SENATO_CollegiUNINOMINALI_2020") {
      sf <- sf %>%
        dplyr::filter(is.na(SU20_DEN)==FALSE) %>% 
        dplyr::filter(SU20_DEN == name)
    } else if (level == "UT_Collegi2020") {
      sf <- sf %>%
        dplyr::filter(is.na(DEN_REG) == FALSE) %>% 
        dplyr::filter(DEN_REG == name)
    } 
    
    saveRDS(object = sf,
            file = rds_file_location)
  }
  return(sf)
}
