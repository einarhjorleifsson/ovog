# NOTE: Should possible just extract station and measures, then generate
#       the downstream tables (nu, le and kv)

#' Reads muliple hafvogs files
#'
#' @param zip_file File name, including path
#'
#' @return a list
#' @export
#'
hv_read_zip_data <- function(zip_file) {
  
  res <- purrr::map(zip_file, hv_read_zip_file)
  names(res) <- basename(zip_file)
  
  return(list(ST = dplyr::bind_rows(sapply(res, "[", "ST")), 
              M  = dplyr::bind_rows(sapply(res, "[", "M")), 
              NU = dplyr::bind_rows(sapply(res, "[", "NU")), 
              LE = dplyr::bind_rows(sapply(res, "[", "LE")), 
              KV = dplyr::bind_rows(sapply(res, "[", "KV")),
              pred = dplyr::bind_rows(sapply(res, "[", "pred")),
              prey = dplyr::bind_rows(sapply(res, "[", "prey"))))
  
}




#' Reads a sinle hafvog zip file
#'
#' @param zip_file File name, including path
#'
#' @return a list
#' @export
hv_read_zip_file <- function(zip_file) {
  
  
  
  
  tmpdir <- tempdir()
  if(file.exists(paste0(tmpdir, "/hafvog.leidangrar.txt"))) file.remove(paste0(tmpdir, "/hafvog.leidangrar.txt"))
  if(file.exists(paste0(tmpdir, "/hafvog.skraning.txt"))) file.remove(paste0(tmpdir, "/hafvog.skraning.txt"))
  if(file.exists(paste0(tmpdir, "/hafvog.stodvar.txt"))) file.remove(paste0(tmpdir, "/hafvog.stodvar.txt"))
  if(file.exists(paste0(tmpdir, "/hafvog.togstodvar.txt"))) file.remove(paste0(tmpdir, "/hafvog.togstodvar.txt"))
  if(file.exists(paste0(tmpdir, "/hafvog.umhverfi.txt"))) file.remove(paste0(tmpdir, "/hafvog.umhverfi.txt"))
  if(file.exists(paste0(tmpdir, "/hafvog.skraning.txt"))) file.remove(paste0(tmpdir, "/hafvog.skraning.txt"))
  
  utils::unzip(zip_file, exdir = tmpdir)
  
  ## station -------------------------------------------------------------------
  ST <- 
    hafvog::hv_station(tmpdir, std = FALSE) |> 
    dplyr::rename(veidarfaeri = fishing_gear_no) |> 
    # sometimes people forget to put in gear - should really generate a warning
    dplyr::mutate(vf = dplyr::case_when(synaflokkur == 30 & is.na(veidarfaeri) ~ 73,
                                        .default = veidarfaeri),
                  index = (reitur * 100 + tognumer) * 100 + vf) |>
    dplyr::select(-vf) |> 
    dplyr::rename(lon1 = kastad_v_lengd,
                  lat1 = kastad_n_breidd,
                  lon2 = hift_v_lengd,
                  lat2 = hift_n_breidd) |> 
    dplyr::mutate(lon2 = ifelse(is.na(lon2), lon1, lon2),
                  lat2 = ifelse(is.na(lat2), lat1, lat2)) |> 
    dplyr::mutate(lon = (lon1 + lon2) / 2,
                  lat = (lat1 + lat2) / 2,
                  toglengd = ifelse(is.na(toglengd), 4, toglengd),
                  veidarfaeri = 73) |> 
    dplyr::mutate(ar = lubridate::year(dags))
  if(any(is.na(ST$index))) stop("Unexpected: Tow index (index) is na")
  ## Measures ------------------------------------------------------------------
  M <- 
    ST |> 
    dplyr::select(synis_id, ar, leidangur, index, stod) |> 
    dplyr::left_join(hv_measures(tmpdir, std = FALSE),
                     by = dplyr::join_by(synis_id)) |>
    dplyr::select(-synis_id) |> 
    # maeliadgerd 30 is tagging - would not normally expect that in SMX
    #   expect tagging to be in another synaflokkur than survey
    dplyr::mutate(m = dplyr::case_when(maeliadgerd %in% c(1:3, 9, 30) ~ "maelt",
                                       maeliadgerd %in% 10 ~ "talid",
                                       .default = "annað")) |> 
    dplyr::arrange(leidangur, stod, tegund, maeliadgerd, nr)
  ## Numer -----------------------------------------------------------------------
  NU <- 
    ST |> 
    dplyr::select(ar, index) |> 
    dplyr::left_join(M |> 
                       dplyr::filter(m %in% c("maelt", "talid")),
                     by = dplyr::join_by(ar, index)) |> 
    dplyr::group_by(ar, index, tegund, m) |> 
    dplyr::summarise(n = sum(fjoldi),
                     .groups = "drop") |> 
    tidyr::pivot_wider(names_from = m, values_from = n, values_fill = 0) |> 
    dplyr::mutate(alls = maelt + talid) |> 
    dplyr::rename(fj_maelt = maelt,
                  fj_talid = talid,
                  fj_alls = alls) |> 
    dplyr::mutate(r = dplyr::case_when(fj_maelt == 0 ~ 1,
                                       .default = fj_alls / fj_maelt))
  
  
  ## Length ----------------------------------------------------------------------
  LE <-
    ST |> 
    dplyr::select(ar, index) |> 
    dplyr::left_join(M |> 
                       dplyr::filter(m %in% "maelt") |> 
                       dplyr::group_by(ar, index, tegund, lengd) |> 
                       dplyr::summarise(fjoldi = sum(fjoldi, na.rm = TRUE),
                                        .groups = "drop"),
                     by = dplyr::join_by(ar, index)) |> 
    # skala með toldum
    dplyr::left_join(NU |> 
                       dplyr::select(ar, index, tegund, r),
                     by = dplyr::join_by(ar, index, tegund)) |> 
    dplyr::mutate(n = r * fjoldi,
                  b = (n * 0.01 * lengd^3) / 1e3)
  if(any(is.na(LE$r))) stop("Unexpected: Raising factor (r) is na")
  
  ## Kvarnir ---------------------------------------------------------------------
  KV <- 
    ST |> 
    dplyr::select(ar, index) |> 
    dplyr::left_join(M |> 
                       dplyr::filter(maeliadgerd == 3),
                     by = dplyr::join_by(ar, index)) |> 
    dplyr::select(ar, index, tegund, nr, lengd, kyn,
                  kynthroski, oslaegt, slaegt,
                  kynfaeri, lifur)
  
  ## Predators (not really needed) ---------------------------------------------
  pred <- 
    M |> 
    dplyr::filter(!is.na(magaastand)) |> 
    dplyr::select(ar, index, pred = tegund, nr, oslaegt, slaegt, 
                  astand = magaastand) 
  # # Need the support table upstream
  # dplyr::left_join(magaastand |> 
  #                    dplyr::select(astand, lysing_astands),
  #                  by = "astand") |> 
  # dplyr::select(-astand) |> 
  # dplyr::rename(astand = lysing_astands)
  ## Prey (not really needed) --------------------------------------------------
  prey <-
    M |> 
    dplyr::filter(maeliadgerd %in% c(20, 21, 22)) |> 
    dplyr::rename(prey = tegund,
                  pred = ranfiskurteg,
                  pnr = nr, 
                  nr = kvarnanr) |>
    dplyr::select(ar, index, pred, nr, prey, pnr, 
                  n = fjoldi, lengd, kyn, thyngd = heildarthyngd)
  
  # downstream just use ar index as a link
  ST <- ST |> dplyr::select(-synis_id)
  
  return(list(ST = ST, M = M, NU = NU, LE = LE, KV = KV, pred = pred, prey = prey))
}
