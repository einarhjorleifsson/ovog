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
  
  
  if(!file.exists(zip_file)) {
    stop("There is no file: ", zip_file, ". Check your path and spelling")
  }
  
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
    #dplyr::filter(synaflokkur %in% id) |>
    dplyr::mutate(index = reitur * 100 + tognumer) |> 
    dplyr::mutate(lon1 = kastad_v_lengd,
                  lat1 = kastad_n_breidd,
                  lon2 = hift_v_lengd,
                  lat2 = hift_n_breidd) |> 
    dplyr::mutate(lon = (lon1 + lon2) / 2,
                  lat = (lat1 + lat2) / 2,
                  toglengd = ifelse(is.na(toglengd), 4, toglengd),
                  veidarfaeri = 73) |> 
    dplyr::mutate(ar = lubridate::year(dags))
  ## Measures ------------------------------------------------------------------
  M <- 
    hv_measures(tmpdir, std = FALSE) |> 
    # maeliadgerd 30 is tagging - would not normally expect that in SMX
    #   expect tagging to be in another synaflokkur than survey
    dplyr::mutate(m = dplyr::case_when(maeliadgerd %in% c(1:3, 30) ~ "maelt",
                                       maeliadgerd %in% 10 ~ "talid",
                                       .default = "annað"))
  ## Numer -----------------------------------------------------------------------
  NU <- 
    ST |> 
    dplyr::select(synis_id, index) |> 
    dplyr::left_join(M |> dplyr::filter(m %in% c("maelt", "talid")),
                     by = dplyr::join_by(synis_id)) |> 
    dplyr::group_by(synis_id, index, tegund, m) |> 
    dplyr::summarise(n = sum(fjoldi),
                     .groups = "drop") |> 
    tidyr::pivot_wider(names_from = m, values_from = n, values_fill = 0) |> 
    dplyr::mutate(alls = maelt + talid) |> 
    dplyr::arrange(index, tegund) |> 
    dplyr::rename(fj_maelt = maelt,
                  fj_talid = talid,
                  fj_alls = alls)
  ## Length ----------------------------------------------------------------------
  LE <-
    ST |> 
    dplyr::select(synis_id, index) |> 
    dplyr::left_join(M |> 
                       dplyr::filter(m %in% "maelt") |> 
                       dplyr::group_by(synis_id, tegund, lengd) |> 
                       dplyr::summarise(fjoldi = sum(fjoldi, na.rm = TRUE),
                                        .groups = "drop"),
                     by = dplyr::join_by(synis_id))
  ## Kvarnir ---------------------------------------------------------------------
  KV <- 
    ST |> 
    dplyr::select(synis_id, index) |> 
    dplyr::left_join(M |> dplyr::filter(maeliadgerd == 3),
                     by = dplyr::join_by(synis_id)) |> 
    dplyr::select(synis_id, index, tegund, nr, lengd, kyn,
                  kynthroski, oslaegt, slaegt,
                  kynfaeri, lifur)
  
  ## Predators (not really needed) ---------------------------------------------
  pred <- 
    M |> 
    dplyr::filter(!is.na(magaastand)) |> 
    dplyr::select(synis_id, pred = tegund, nr, oslaegt, slaegt, 
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
    dplyr::filter(maeliadgerd %in% c(20, 21)) |> 
    dplyr::rename(prey = tegund,
                  pred = ranfiskurteg,
                  pnr = nr, 
                  nr = kvarnanr) |>
    dplyr::select(synis_id, pred, nr, prey, pnr, 
                  n = fjoldi, lengd, kyn, thyngd = heildarthyngd)
  
  return(list(ST = ST, M = M, NU = NU, LE = LE, KV = KV, pred = pred, prey = prey))
}
