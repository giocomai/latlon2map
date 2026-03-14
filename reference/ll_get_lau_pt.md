# Regions and provinces in Italy (high detail, CC-BY Istat)

Source: https://dados.gov.pt/pt/datasets/freguesias-de-portugal/

## Usage

``` r
ll_get_lau_pt(
  id = NULL,
  name = NULL,
  year = 2017,
  level = "concelho",
  silent = FALSE
)
```

## Arguments

- id:

  A character vector composed of six digits. Corresponds to "dicofre".

- year:

  Defaults to 2017 (latest and currently only available).

- level:

  Defaults to "freguesia". Valid value include "freguesia", "concelho",
  "distrito", "des_simpli".

## Examples

``` r
ll_set_folder(fs::path(fs::path_home_r(), "R"))
#> /home/runner/R
ll_get_lau_pt()
#> ℹ Source: <https://dados.gov.pt/pt/datasets/freguesias-de-portugal/>
#> ℹ dados.gov.pt (CC-BY)
#> Simple feature collection with 3223 features and 8 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -9.517029 ymin: 36.96171 xmax: -6.189159 ymax: 42.15431
#> Geodetic CRS:  WGS 84
#> # A tibble: 3,223 × 9
#>    Dicofre Freguesia     Concelho Distrito TAA   AREA_EA_Ha AREA_T_Ha Des_Simpli
#>  * <chr>   <chr>         <chr>    <chr>    <chr>      <dbl>     <dbl> <chr>     
#>  1 080106  Albufeira e … ALBUFEI… FARO     ÁREA…       0.09     4118. Albufeira…
#>  2 081504  Sagres        VILA DO… FARO     ÁREA…    3432.       3437. Sagres    
#>  3 080106  Albufeira e … ALBUFEI… FARO     ÁREA…       0.01     4118. Albufeira…
#>  4 080106  Albufeira e … ALBUFEI… FARO     ÁREA…       0.01     4118. Albufeira…
#>  5 080106  Albufeira e … ALBUFEI… FARO     ÁREA…       0.01     4118. Albufeira…
#>  6 080106  Albufeira e … ALBUFEI… FARO     ÁREA…       0.01     4118. Albufeira…
#>  7 080106  Albufeira e … ALBUFEI… FARO     ÁREA…       0.01     4118. Albufeira…
#>  8 080106  Albufeira e … ALBUFEI… FARO     ÁREA…       0.01     4118. Albufeira…
#>  9 080106  Albufeira e … ALBUFEI… FARO     ÁREA…       0.02     4118. Albufeira…
#> 10 080106  Albufeira e … ALBUFEI… FARO     ÁREA…       0.01     4118. Albufeira…
#> # ℹ 3,213 more rows
#> # ℹ 1 more variable: geometry <POLYGON [°]>
ll_get_lau_pt(name = "Porto")
#> ℹ Source: <https://dados.gov.pt/pt/datasets/freguesias-de-portugal/>
#> ℹ dados.gov.pt (CC-BY)
#> Simple feature collection with 7 features and 8 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -8.691294 ymin: 41.13835 xmax: -8.552613 ymax: 41.18594
#> Geodetic CRS:  WGS 84
#> # A tibble: 7 × 9
#>   Dicofre Freguesia      Concelho Distrito TAA   AREA_EA_Ha AREA_T_Ha Des_Simpli
#> * <chr>   <chr>          <chr>    <chr>    <chr>      <dbl>     <dbl> <chr>     
#> 1 131218  União das fre… PORTO    PORTO    ÁREA…       559.      559. Lordelo d…
#> 2 131202  Bonfim         PORTO    PORTO    ÁREA…       310.      310. Bonfim    
#> 3 131217  União das fre… PORTO    PORTO    ÁREA…       543.      543. Cedofeita…
#> 4 131203  Campanhã       PORTO    PORTO    ÁREA…       804.      804. Campanhã  
#> 5 131216  União das fre… PORTO    PORTO    ÁREA…       627.      627. Aldoar, F…
#> 6 131211  Ramalde        PORTO    PORTO    ÁREA…       582.      582. Ramalde   
#> 7 131210  Paranhos       PORTO    PORTO    ÁREA…       717.      717. Paranhos  
#> # ℹ 1 more variable: geometry <POLYGON [°]>
```
