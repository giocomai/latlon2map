# Get Italian electoral districts (CC-BY Istat)

2022 / WGS 84 / UTM zone 32N

## Usage

``` r
ll_get_electoral_districts_it(
  name = NULL,
  level = "Circoscrizioni_Camera",
  year = 2022,
  silent = FALSE,
  no_check_certificate = FALSE
)
```

## Arguments

- level:

  Defaults to "Circoscrizioni_Camera". Valid values:

  - "Circoscrizioni_Camera": Basi geografiche delle circoscrizioni
    elettorali - Camera dei deputati

  - "Regioni_Senato": Basi geografiche delle circoscrizioni elettorali -
    Senato della Repubblica

  - "CAMERA_CollegiPLURINOMINALI_2020": Basi geografiche dei collegi
    elettorali plurinominali - Camera dei deputati

  - "CAMERA_CollegiUNINOMINALI_2020": Basi geografiche dei collegi
    elettorali uninominali - Camera dei deputati

  - "SENATO_CollegiPLURINOMINALI_2020": Basi geografiche dei collegi
    elettorali plurinominali - Senato della Repubblica

  - "SENATO_CollegiUNINOMINALI_2020": Basi geografiche dei collegi
    elettorali uninominali - Senato della Repubblica

  - "UT_Collegi2020": Basi geografiche delle unità territoriali che
    formano i collegi elettorali (comuni e aree sub-comunali,
    limitatamente ai comuni di Torino, Genova, Milano, Roma, Napoli e
    Palermo con territorio ripsrtito su più di un collegio). Geografia
    comunale vigente alla data della pubblicazione

- year:

  Defaults to 2022 (latest available). Currently no other year accepted.

- no_check_certificate:

  Logical, defaults to TRUE. Enable only if certificate issues, and if
  you are aware of the security implications.

## Details

Column names metadata:

- COD_REG Codice della regione/circoscrizione elettorale del Senato
  della Repubblica

- DEN_REG Denominazione della regione amministrativa/circoscrizione
  elettorale Senato della Repubblica

- COD_PRO Codice della provincia

- DEN_P_CM Denominazione della provincia o città metropolitana

- COD_CM Codice della città metropolitana

- PRO_COM Codice del comune

- DEN_COM Denominazione del comune

- CAP_DEN Denominazione del capoluogo di provincia o città metropolitana

- POP_2011 Popolazione - Censimento 2011

- ASC_COD Codice concatenato comune e area sub-comunale

- ASC_COD1 Codice progressivo area sub-comunale

- ASC_COD2 Codice alfanumerico dell'area sub-comunale attribuito dal
  comune

- ASC_NOME Denominazione dell'area sub-comunale

- ASC_TIPO Tipologia di area-sub-comunale

- CIRC_COD Codice della circoscrizione elettorale della Camera dei
  deputati

- CIRC_DEN Denominazione della circoscrizione elettorale della Camera
  dei deputati

- CU20_COD Codice del collegio elettorale uninominale della Camera dei
  deputati

- CP20_COD Codice del collegio elettoraleplurinominale della Camera dei
  deputati

- SU20_COD Codice del collegio elettorale uninominale del Senato della
  Repubblica

- SP20_COD Codice del collegio elettorale plurinominale del Senato della
  Repubblica

- CU20_DEN Denominazione del collegio elettorale uninominale della
  Camera dei deputati

- CP20_DEN Denominazione del collegio elettorale plurinominale della
  Camera dei deputati

- SU20_DEN Denominazione del collegio elettorale uninominale del Senato
  della Repubblica

- SP20_DEN Denominazione del collegio elettorale plurinominale del
  Senato della Repubblica

- CU20_C1 Sigla del collegio elettorale uninominale della Camera dei
  deputati

- CP20_C1 Sigla del collegio elettorale plurinominale della Camera dei
  deputati

- SU20_C1 Sigla del collegio elettorale uninominale del Senato della
  Repubblica

- SP20_C1 Sigla del collegio elettorale plurinominale del Senato della
  Repubblica

## Examples

``` r
ll_set_folder(fs::path(fs::path_home_r(), "R"))
#> /home/runner/R
ll_get_electoral_districts_it()
#> https://www.istat.it/it/archivio/273443
#> ℹ Istat (CC-BY)
#> Error in loadNamespace(x): there is no package called ‘stringr’
ll_get_electoral_districts_it(name = "Lombardia 2")
#> https://www.istat.it/it/archivio/273443
#> ℹ Istat (CC-BY)
#> Error in loadNamespace(x): there is no package called ‘stringr’
ll_get_electoral_districts_it() %>% ggplot2::ggplot() + ggplot2::geom_sf() + ggplot2::labs(title = "Circoscrizioni Camera")
#> https://www.istat.it/it/archivio/273443
#> ℹ Istat (CC-BY)
#> Error in loadNamespace(x): there is no package called ‘stringr’
ll_get_electoral_districts_it(level = "SENATO_CollegiUNINOMINALI_2020") %>% ggplot2::ggplot() + ggplot2::geom_sf() + ggplot2::labs(title = "Collegi uninominali - Senato")
#> https://www.istat.it/it/archivio/273443
#> ℹ Istat (CC-BY)
#> Error in loadNamespace(x): there is no package called ‘stringr’
```
