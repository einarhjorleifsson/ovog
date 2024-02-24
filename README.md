
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hafvog

<!-- badges: start -->
<!-- badges: end -->

The goal of hafvog is to read the hafvogs json files into R

## Installation

You can install the development version of hafvog from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("einarhjorleifsson/hafvog")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(hafvog)
zip_path <- system.file("extdata", "TESTO-2024.zip", package = "hafvog")
tmpdir <- tempdir()
unzip(zipfile = zip_path, exdir = tmpdir)
hv_cruise(tmpdir)
#> # A tibble: 1 × 7
#>   D1         D2         cclass sclass   gid   vid cruise    
#>   <date>     <date>      <int>  <int> <int> <int> <chr>     
#> 1 2024-02-15 2024-03-15      0     30    73  2350 TESTO-2024
hv_station(tmpdir) |> dplyr::glimpse()
#> Rows: 1
#> Columns: 23
#> $ lat2            <dbl> 66.40833
#> $ z1              <int> 100
#> $ veidarfaeri_id  <chr> "1"
#> $ landsyni        <lgl> FALSE
#> $ ssq             <int> 1
#> $ cruise          <chr> "TESTO-2024"
#> $ vid             <int> 2350
#> $ undirstod_heiti <chr> "Varpa"
#> $ sclass          <int> 30
#> $ medferd_afla    <int> 1
#> $ .sid            <int> 8721
#> $ gid             <int> 73
#> $ kastad_n_breidd <dbl> 66.345
#> $ device_id       <int> 6
#> $ date            <date> 2024-02-15
#> $ net_nr          <int> 1
#> $ sq              <int> 624
#> $ grandaralengd   <int> 35
#> $ hnattstada      <int> -1
#> $ lon2            <dbl> -24.73967
#> $ z2              <int> 130
#> $ stod            <int> 1
#> $ lon1            <dbl> -24.78667
hv_towstations(tmpdir) |> dplyr::glimpse()
#> Rows: 1
#> Columns: 12
#> $ vir_uti       <int> 130
#> $ dregid_fra    <chr> "A"
#> $ speed         <dbl> 4
#> $ heading       <int> 15
#> $ towlength     <dbl> 4
#> $ t1            <dttm> 2024-02-15 22:00:00
#> $ .sid          <int> 8721
#> $ towtime       <int> 60
#> $ t2            <dttm> 2024-02-15 23:00:00
#> $ tognumer      <int> 12
#> $ lodrett_opnun <dbl> 3
#> $ larett_opnun  <dbl> 90
hv_environment(tmpdir) |> dplyr::glimpse()
#> Rows: 1
#> Columns: 10
#> $ botnhiti      <dbl> 4
#> $ lofthiti      <dbl> 3
#> $ vindhradi     <int> 4
#> $ loftvog       <int> 1000
#> $ sky           <int> 5
#> $ yfirbordshiti <dbl> 3
#> $ .sid          <int> 8721
#> $ sjor          <int> 2
#> $ vedur         <int> 1
#> $ vindatt       <int> 5
hv_measures(tmpdir) |> dplyr::glimpse()
#> Rows: 142
#> Columns: 16
#> $ nr                    <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 1…
#> $ lengd                 <dbl> 55, 55, 44, 44, 55, 77, 47, 45, 41, 15, 45, 30, …
#> $ maeliadgerd           <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ tegund                <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ valkvorn              <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,…
#> $ tegund_as_faedutegund <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ fjoldi                <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ radnr                 <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 1…
#> $ synis_id              <int> 8721, 8721, 8721, 8721, 8721, 8721, 8721, 8721, …
#> $ kyn                   <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ magaastand            <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ kynthroski            <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ ranfiskurteg          <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ kvarnanr              <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ heildarthyngd         <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ oslaegt               <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> Rows: 142
#> Columns: 16
#> $ nr          <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,…
#> $ length      <dbl> 55, 55, 44, 44, 55, 77, 47, 45, 41, 15, 45, 30, 31, 32, 33…
#> $ maeliadgerd <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
#> $ sid         <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
#> $ valkvorn    <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FA…
#> $ sid_food    <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
#> $ n           <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
#> $ radnr       <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,…
#> $ .sid        <int> 8721, 8721, 8721, 8721, 8721, 8721, 8721, 8721, 8721, 8721…
#> $ sex         <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ stomach     <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ maturity    <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ sid_pred    <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ kvarnanr    <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ wt          <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ gwt         <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
```
