# Stadlar rallstodvar ----------------------------------------------------------
devtools::load_all()
library(tidyverse)
con <- "data-raw/stillingar/stillingar_SMB_rall_(botnfiskur).zip"
## tegundir ----
### json ---
stadlar.tegundir <-
  ovog:::js_stadla_tegund_smb() |>
  dplyr::filter(leidangur_id == lid) |>
  dplyr::arrange(tegund) |>
  tidyr::gather(variable, value, lifur_low:kynkirtlar_high) |>
  dplyr::mutate(value = value / 100) |>
  tidyr::spread(variable, value)




stadlar.rallstodvar <-
  ovog:::js_stadla_rallstodvar(con) |>
  # fix an error in hift_v for SMH, should be corrected in database
  dplyr::mutate(hift_v = ifelse(hift_v == -2444550, -244455, hift_v)) |>
  dplyr::mutate(hift_n = ifelse(hift_n ==  6537038,  653703, hift_n)) |>
  geo::geoconvert(col.names = c("kastad_v", "kastad_n")) |>
  geo::geoconvert(col.names = c("hift_v",   "hift_n"))

lid <- stadlar.rallstodvar$leidangur_id[[1]]



## length weight ----
### xe ----
### json ----
stadlar.lw <-
  ovog:::js_stadla_lw() |>
  dplyr::collect(n = Inf) |>
  dplyr::mutate(osl1 = osl * (1 - fravik),
                osl2 = osl * (1 + fravik),
                sl1 = sl * (1 - fravik),
                sl2 = sl * (1 + fravik)) |>
  dplyr::select(tegund, lengd, osl1:sl2)

## fishtegundir ----
### json ----
fisktegundir <-
  ovog:::js_fisktegundir() |> 
  dplyr::select(tegund = species_no, heiti = name) |>
  dplyr::arrange(tegund)
sid <-
  ovog:::js_fisktegundir() |> 
  dplyr::select(sid = species_no, tegund = name) |>
  dplyr::arrange(tegund)
prey_names <-
  ovog:::js_fisktegundir() |> 
  dplyr::select(faeduhopur = food_no, lat_heiti = sci_name,
                isl_heiti = name,
                tegund = species_no)

## Maeliatridi ----
### json ----
aid <-
  ovog:::js_maeliatridi() |> 
  dplyr::rename(aid = id, adgerd = heiti)



# Predator ----
## json ----
magaastand <- 
  ovog:::js_magaastand()

