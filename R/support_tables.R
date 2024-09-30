tbl_js <- function(zpth, tbl) {
  conz <- unzip(zpth, tbl)
  d <-
    jsonlite::read_json(conz, simplifyVector = TRUE)$values |> 
    janitor::clean_names() |> 
    tibble::as_tibble()
  #unlink(conz)
  return(d)
}

# # could read all in at once via:
# zpth <- "inst/extdata/stillingar_SMB_rall_(botnfiskur).zip"
# tables <- unzip(zpth, list = TRUE) |> pull(Name)
# res <- map2(zpth, tables, tbl_js)
# names(res) <- tables |> str_remove("hafvog.")

#' Title
#'
#' The current form is just mimicry
#'
#' @param con path to stillingar_SMB_rall
#'
#' @export
#'
js_stadla_rallstodvar <- function(con = "data-raw/stillingar_SMB_rall_(botnfiskur).zip") {
  
  tbl_js(con, "hafvog.sti_rallstodvar.txt") |>
    dplyr::left_join(tbl_js(con, "hafvog.sti_leidangrar.txt"), by = "leidangur_id") |>
    dplyr::mutate(kastad_v = -kastad_v,
                  hift_v = -hift_v,
                  index = reitur * 100 + tognumer)
  
}

#' Title
#'
#' bla, bla
#'
#' @param con XX
#'
#' @export
#'
js_stadla_tegund <- function(con = "data-raw/stillingar_SMB_rall_(botnfiskur).zip") {
  
  tbl_js(con, "hafvog.fiskteg_tegundir.txt") |>
    dplyr::rename(tegund = fisktegund_id)
  
}

#' Title
#'
#' bla, bla
#'
#' @param con XX
#'
#' @export
#'
js_stadla_lw <- function(con = "data-raw/stillingar_SMB_rall_(botnfiskur).zip") {
  
  d <-
    tbl_js(con, "hafvog.fiskteg_lengd_thyngd.txt") |>
    dplyr::rename(tegund = fisktegund_id) |>
    dplyr::mutate(fravik = fravik/100) |>
    dplyr::collect(n = Inf)
  x <-
    d |>
    dplyr::group_by(tegund) |>
    dplyr::summarise(l.max = max(lengd))
  
  expand.grid(tegund = unique(d$tegund),
              lengd = 1:1500) |>
    dplyr::as_tibble() |>
    dplyr::left_join(x, by = "tegund") |>
    dplyr::filter(lengd <= l.max) |>
    dplyr::select(-l.max) |>
    dplyr::left_join(d, by = c("tegund", "lengd")) |>
    dplyr::arrange(tegund, -lengd) |>
    dplyr::group_by(tegund) |>
    tidyr::fill(oslaegt_a:fravik) |>
    dplyr::ungroup() |>
    dplyr:: mutate(osl = oslaegt_a * lengd^oslaegt_b,
                   sl = slaegt_a * lengd^slaegt_b)
  
}


js_fisktegundir <- function(con = "data-raw/stodtoflur.zip") {
  tbl_js(con, "hafvog.species_v.txt")
}


js_maeliatridi <- function(con = "data-raw/stillingar_SMB_rall_(botnfiskur).zip") {
  tbl_js(con, "hafvog.maeliatridi.txt")
}


js_magaastand <- function(con = "data-raw/stillingar_SMB_rall_(botnfiskur).zip") {
  tbl_js(con, "hafvog.magaastand.txt")
}