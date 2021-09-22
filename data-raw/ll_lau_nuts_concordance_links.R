## code to prepare `ll_lau_nuts_concordance_links` dataset goes here

# see:
# https://ec.europa.eu/eurostat/web/nuts/local-administrative-units

ll_lau_nuts_concordance_links <- tibble::tribble(~lau_year, ~nuts_year, ~status, ~link,
                2020,      2021,       "provisional", "https://ec.europa.eu/eurostat/documents/345175/501971/EU-27-LAU-2020-NUTS-2021-NUTS-2016.xlsx",
                2020,      2016,       "provisional", "https://ec.europa.eu/eurostat/documents/345175/501971/EU-27-LAU-2020-NUTS-2021-NUTS-2016.xlsx",
                2019,      2016,       "validated", "https://ec.europa.eu/eurostat/documents/345175/501971/EU-28-LAU-2019-NUTS-2016.xlsx",
                2018,      2016,       "validated", "https://ec.europa.eu/eurostat/documents/345175/501971/EU-28-LAU-2018-NUTS-2016.xlsx",
                2017,      2016,       "validated", "https://ec.europa.eu/eurostat/documents/345175/501971/EU-28_LAU_2017_NUTS_2016.xlsx",
                2017,      2013,       "validated", "https://ec.europa.eu/eurostat/documents/345175/501971/EU-28_LAU_2017_NUTS_2013.xlsx"
                )


usethis::use_data(ll_lau_nuts_concordance_links, overwrite = TRUE)
