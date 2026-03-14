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
#> Simple feature collection with 28 features and 6 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 313279.3 ymin: 3933683 xmax: 1312016 ymax: 5220292
#> Projected CRS: WGS 84 / UTM zone 32N
#> # A tibble: 28 × 7
#>    OBJECTID CIRC_COD CIRC_DEN              Shape_Leng   Shape_Area POP_2011
#>       <dbl>    <dbl> <chr>                      <dbl>        <dbl>    <dbl>
#>  1        1        1 Piemonte 1               593390.  6826908024.  2247780
#>  2        2        2 Piemonte 2              1392233. 18559788304.  2116136
#>  3        3        3 Lombardia 1              360012.  1873839345.  3805895
#>  4        4        4 Lombardia 2              920201.  6719744999.  2088579
#>  5        5        5 Lombardia 3              604436.  7105383618.  2175099
#>  6        6        6 Lombardia 4             1095404.  8164129493.  1634578
#>  7        7        7 Veneto 1                 885787.  8562659461.  1932447
#>  8        8        8 Veneto 2                 800405.  9782546922.  2923457
#>  9        9        9 Friuli-Venezia Giulia    767632.  7932519651.  1220291
#> 10       10       10 Liguria                 1079631.  5416137655.  1570694
#> # ℹ 18 more rows
#> # ℹ 1 more variable: geometry <MULTIPOLYGON [m]>
ll_get_electoral_districts_it(name = "Lombardia 2")
#> https://www.istat.it/it/archivio/273443
#> ℹ Istat (CC-BY)
#> Simple feature collection with 1 feature and 6 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 465199.5 ymin: 5045017 xmax: 625473.8 ymax: 5165371
#> Projected CRS: WGS 84 / UTM zone 32N
#> # A tibble: 1 × 7
#>   OBJECTID CIRC_COD CIRC_DEN    Shape_Leng  Shape_Area POP_2011
#> *    <dbl>    <dbl> <chr>            <dbl>       <dbl>    <dbl>
#> 1        4        4 Lombardia 2    920201. 6719744999.  2088579
#> # ℹ 1 more variable: geometry <MULTIPOLYGON [m]>
ll_get_electoral_districts_it() %>%
 ggplot2::ggplot() +
 ggplot2::geom_sf() +
 ggplot2::labs(title = "Circoscrizioni Camera")
#> https://www.istat.it/it/archivio/273443
#> ℹ Istat (CC-BY)


ll_get_electoral_districts_it(level = "SENATO_CollegiUNINOMINALI_2020") %>%
 ggplot2::ggplot() +
 ggplot2::geom_sf() +
 ggplot2::labs(title = "Collegi uninominali - Senato")
#> https://www.istat.it/it/archivio/273443
#> ℹ Istat (CC-BY)
```
