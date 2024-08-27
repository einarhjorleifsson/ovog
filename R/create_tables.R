#' Create tables from hafvog "skraning"
#' 
#' Normally the input to this function is what is read in from hv_read_hafvog.
#' The outputs are tables like fiskar.lengdir, fiskar.kvarnir, fiskar.numer, ...
#' 
#' @param list A list object containing hafvog dataframes
#' 
#' @return a list
#' @export
#' 
hv_create_tables <- function(list) {
  
  
  ## station -------------------------------------------------------------------
  ST <- list$stodvar
  ## Measures ------------------------------------------------------------------
  M <- 
    list$skraning |> 
    # maeliadgerd 30 is tagging - would not normally expect that in SMX
    #   expect tagging to be in another synaflokkur than survey
    dplyr::mutate(m = dplyr::case_when(maeliadgerd %in% c(1:3, 9, 30) ~ "maelt",
                                       maeliadgerd %in% 10 ~ "talid",
                                       .default = "annad"))
  ## Numer -----------------------------------------------------------------------
  NU <- 
    M |> 
    dplyr::group_by(.file, synis_id, tegund, m) |> 
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
    M |> 
    dplyr::filter(m %in% "maelt") |> 
    dplyr::group_by(.file, synis_id, tegund, lengd) |> 
    dplyr::summarise(fjoldi = sum(fjoldi, na.rm = TRUE),
                     .groups = "drop") |> 
    # skala með toldum
    dplyr::left_join(NU |> 
                       dplyr::select(.file, synis_id, tegund, r),
                     by = dplyr::join_by(.file, synis_id, tegund)) |> 
    dplyr::mutate(n = r * fjoldi,
                  b = (n * 0.01 * lengd^3) / 1e3)
  if(any(is.na(LE$r))) stop("Unexpected: Raising factor (r) is na")
  
  ## Kvarnir ---------------------------------------------------------------------
  KV <- 
    M |> 
    dplyr::filter(maeliadgerd == 3) |> 
    dplyr::select(.file, synis_id, tegund, nr, lengd, kyn,
                  kynthroski, oslaegt, 
                  lifur, dplyr::everything())
  
  ## Predators (not really needed) ---------------------------------------------
  pred <- 
    M |> 
    dplyr::filter(!is.na(magaastand)) |> 
    dplyr::select(.file, synis_id, pred = tegund, nr, oslaegt, # slaegt, 
                  astand = magaastand, dplyr::everything()) 
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
    dplyr::select(.file, synis_id, pred, nr, prey, pnr, 
                  n = fjoldi, lengd, kyn, thyngd = heildarthyngd)
  
  return(list(ST = ST, M = M, NU = NU, LE = LE, KV = KV, pred = pred, prey = prey))
}
