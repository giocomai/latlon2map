#' Gets correspondence tables between local administrative units and nuts from Eurostat's website
#'
#' Source: https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/lau
#'
#' Warning: due to issues in the original data, nuts may not always correspond to the given year for all countries, e.g. in files with nuts 2016 one may find nuts 2013 for single country, e.g. Italy.
#' Do check the sources for details and ensure complete matching.
#'
#' @param lau_year Defaults to 2019. See `ll_lau_nuts_concordance_links` for details on available combinations.
#' @param nuts_year Defaults to 2016. See `ll_lau_nuts_concordance_links` for details on available combinations.
#' @param silent Defaults to FALSE. If TRUE, hides copyright notice. Useful e.g. when using this in reports or in loops. The copyright notice must still be shown where the final output is used.
#'
#' @return A tibble with a correspondence table.
#' @export
#'
#' @examples
#'
#' ll_set_folder("~/R/")
#' ll_get_lau_nuts_concordance()
#' \dontrun{
#' lau_with_nuts_df <- ll_get_lau_eu(year = 2018) %>%
#'   sf::st_drop_geometry() %>%
#'   filter(is.na(LAU_NAME) == FALSE) %>%
#'   dplyr::rename(gisco_id = GISCO_ID) %>%
#'   dplyr::left_join(
#'     y = ll_get_lau_nuts_concordance(
#'       lau_year = 2018,
#'       nuts_year = 2016
#'     ),
#'     by = "gisco_id"
#'   )
#' }
ll_get_lau_nuts_concordance <- function(lau_year = 2019,
                                        nuts_year = 2016,
                                        silent = FALSE) {
  if (silent == FALSE) {
    usethis::ui_info(x = "For details, see: https://ec.europa.eu/eurostat/web/nuts/local-administrative-units")
  }
  ll_create_folders(
    geo = "eu",
    level = "lau_nuts",
    resolution = "correspondence",
    year = lau_year,
    file_type = c("rds", "xlsx")
  )

  rds_file <- ll_find_file(
    geo = "eu",
    level = "lau_nuts",
    resolution = "correspondence",
    year = lau_year,
    name = stringr::str_c("lau_year_", lau_year, "_nuts_year_", nuts_year),
    file_type = "rds"
  )


  if (fs::file_exists(rds_file)) {
    return(readr::read_rds(file = rds_file))
  }

  xlsx_file <- ll_find_file(
    geo = "eu",
    level = "lau_nuts",
    resolution = "correspondence",
    year = lau_year,
    name = stringr::str_c("lau_year_", lau_year, "_nuts_year_", nuts_year),
    file_type = "xlsx"
  )
  lau_year_filter <- lau_year
  nuts_year_filter <- nuts_year

  source_url <- ll_lau_nuts_concordance_links %>%
    dplyr::filter(lau_year == lau_year_filter, nuts_year == nuts_year_filter) %>%
    dplyr::pull(link)

  if (length(source_url) != 1) {
    usethis::ui_stop("See `ll_lau_nuts_concordance_links` for details on available combinations of lau_year and nuts_year or rely on defaults.")
  }

  if (fs::file_exists(xlsx_file) == FALSE) {
    download.file(
      url = source_url,
      destfile = xlsx_file
    )
  }

  country_sheets <- tibble::tibble(sheet = readxl::excel_sheets(xlsx_file)) %>%
    dplyr::filter(nchar(.data$sheet) == 2) %>%
    dplyr::pull(sheet)

  pb <- progress::progress_bar$new(total = length(country_sheets))
  corresondence_df <- purrr::map_dfr(
    .x = country_sheets,
    .f = function(current_sheet_name) {
      pb$tick()
      message(current_sheet_name)
      if (lau_year == 2020 & country_sheets=="IT") {
        if (nuts_year==2016) {
          current_sheet_name <- "IT NUTS 2016"
        } else if (nuts_year == 2021) {
          current_sheet_name <- "IT NUTS 2021"
        }
        current_sheet <- readxl::read_xlsx(
          path = xlsx_file,
          sheet = current_sheet_name,
          col_names = TRUE,
          col_types = "text",
          range = readxl::cell_cols("A:D")
        )
        
      } else if (lau_year == 2020 & current_sheet_name %in% readxl::excel_sheets(xlsx_file)) {
        current_sheet <- readxl::read_xlsx(
          path = xlsx_file,
          sheet = current_sheet_name,
          col_names = TRUE,
          col_types = "text",
          range = readxl::cell_cols("A:D")
        )
      } else if (lau_year == 2020 & nuts_year==2016) {
        current_sheet_name <- tibble::tibble(sheet = readxl::excel_sheets(xlsx_file)) %>% 
          dplyr::filter(stringr::str_starts(string = sheet, pattern = current_sheet_name)) %>% 
          dplyr::pull(sheet)
        
        current_sheet <- readxl::read_xlsx(
          path = xlsx_file,
          sheet = current_sheet_name,
          col_names = TRUE,
          col_types = "text",
          range = readxl::cell_cols("B:E")
        ) %>% 
          dplyr::rename(`NUTS 3 CODE` = `NUTS 3 CODE 2016`)
      } else if (lau_year == 2020 & nuts_year==2021) {
        current_sheet_name <- tibble::tibble(sheet = readxl::excel_sheets(xlsx_file)) %>% 
          dplyr::filter(stringr::str_starts(string = sheet, pattern = current_sheet_name)) %>% 
          dplyr::pull(sheet)
        
        current_sheet <- readxl::read_xlsx(
          path = xlsx_file,
          sheet = current_sheet_name,
          col_names = TRUE,
          col_types = "text",
          range = readxl::cell_cols(c(1, 3:5))
        ) %>% 
          dplyr::rename(`NUTS 3 CODE` = `NUTS 3 CODE 2021`)
        
      } else {
        current_sheet <- readxl::read_xlsx(
          path = xlsx_file,
          sheet = current_sheet_name,
          col_names = TRUE,
          col_types = "text",
          range = readxl::cell_cols("A:D")
        )
      }

      


      if (nrow(current_sheet) == 0) {
        return(NULL)
      }
      
      if (is.element("NUTS 3 CODE 2013", colnames(current_sheet))) {
        current_sheet <- current_sheet %>%
          dplyr::rename(`NUTS 3 CODE` = `NUTS 3 CODE 2013`)
      }

      if (is.element("LAU NAME alternative", colnames(current_sheet))) {
        current_sheet <- current_sheet %>%
          dplyr::rename(`LAU NAME LATIN` = `LAU NAME alternative`)
      }

      # manual fix
      if (current_sheet_name == "EE") {
        current_sheet$`LAU CODE` <- stringr::str_pad(string = current_sheet$`LAU CODE`,
                                                     width = 4, side = "left", pad = "0")
      }
      if (current_sheet_name == "SI") {
        current_sheet$`LAU CODE` <- stringr::str_pad(string = current_sheet$`LAU CODE`,
                                                     width = 3, side = "left", pad = "0")
      }
      

      current_sheet %>%
        dplyr::filter(is.na(.data$`LAU CODE`) == FALSE) %>%
        dplyr::transmute(
          country = current_sheet_name,
          nuts_2 = stringr::str_remove(string = `NUTS 3 CODE`, pattern = "[[:print:]]$"),
          nuts_3 = `NUTS 3 CODE`,
          lau_id = as.character(`LAU CODE`),
          gisco_id = stringr::str_c(current_sheet_name, "_", `LAU CODE`),
          lau_name_national = `LAU NAME NATIONAL`,
          lau_name_latin = `LAU NAME LATIN`
        )
    }
  )

  saveRDS(
    object = corresondence_df,
    file = rds_file
  )

  corresondence_df
}
