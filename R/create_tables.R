# For testing
# devtools::load_all()
# res <-  hv_import_cruise(c("~/R/Pakkar2/osmx/data-raw/SMH/TB2-2024.zip", "~/R/Pakkar2/osmx/data-raw/SMH/TTH1-2024.zip"))
# res <- hv_create_tables(res)
#

hv_create_table_stodvar <- function(stodvar) {
  
  stodvar <- 
    stodvar
  
  return(stodvar)
  
}

hv_create_table_kvarnir <- function(skraning) {
  
  kvarnir <-
    skraning |> 
    dplyr::filter(maeliadgerd == 3) |> 
    dplyr::select(.file, synis_id,
                  tegund, nr, lengd, kyn,
                  kynthroski, oslaegt, slaegt,
                  lifur, kynfaeri,
                  magaastand)
  
  return(kvarnir)
}

hv_create_table_numer <- function(skraning) {
  
  numer  <- 
    skraning |> 
    dplyr::filter(maeliadgerd %in% c(1:3, 9, 10, 30)) |> 
    dplyr::mutate(m = dplyr::case_when(maeliadgerd %in% c(1:3, 9, 30) ~ "maelt",
                                       maeliadgerd %in% 10 ~ "talid",
                                       .default = "annad")) |> 
    dplyr::group_by(.file, synis_id, tegund, m) |> 
    dplyr::reframe(n = sum(fjoldi)) |> 
    tidyr::pivot_wider(names_from = m, values_from = n, values_fill = 0) |> 
    dplyr::mutate(alls = maelt + talid) |> 
    dplyr::rename(fj_maelt = maelt,
                  fj_talid = talid,
                  fj_alls = alls) |> 
    dplyr::mutate(r = dplyr::case_when(fj_maelt == 0 ~ 1,
                                       .default = fj_alls / fj_maelt))
  
  return(numer)
}

hv_create_table_lengdir <- function(skraning) {
  
  lengdir <- 
    skraning |> 
    dplyr::filter(maeliadgerd %in% c(1:3, 9, 30)) |> 
    dplyr::group_by(.file, synis_id, tegund, lengd) |> 
    dplyr::reframe(n = sum(fjoldi, na.rm = TRUE))
  
  return(lengdir)
}



hv_create_table_prey <- function(skraning) {
  
  ## Predators -----------------------------------------------------------------
  pred <- 
    skraning |> 
    dplyr::filter(!is.na(magaastand)) |> 
    dplyr::select(.file, synis_id,
                  tegund,
                  nr,
                  lengd = lengd,
                  astand = magaastand)
  
  ## Prey ----------------------------------------------------------------------
  prey <-
    skraning |> 
    dplyr::filter(maeliadgerd %in% c(20, 21, 22)) |> 
    dplyr::rename(prey_nr = nr) |> 
    dplyr::select(.file, synis_id,
                  tegund = ranfiskurteg,
                  nr = kvarnanr,              # A bit odd
                  prey_tegund = tegund,
                  prey_nr,
                  prey_nn = fjoldi,
                  prey_lengd = lengd,
                  prey_thyngd = oslaegt,
                  prey_kyn = kyn,
                  prey_heildarthyngd = heildarthyngd) 
  
  
  pp <- 
    pred |> 
    dplyr::left_join(prey,
                     by = dplyr::join_by(.file, synis_id, tegund, nr)) |> 
    dplyr::mutate(prey_heildarthyngd = tidyr::replace_na(prey_heildarthyngd, 0))
  
  return(pp)
  
}


#' Create tables from hafvog "skraning"
#' 
#' Normally the input to this function is what is read in from hv_read_hafvog.
#' The outputs are tables like fiskar.lengdir, fiskar.kvarnir, fiskar.numer, ...
#' 
#' @param list A list object containing hafvog dataframes
#' @param scale A boolean (default TRUE) will raise number of length measured
#' with that counted
#' 
#' @return a list
#' @export
#' 
hv_create_tables <- function(list, scale = TRUE) {
  
  ## station -------------------------------------------------------------------
  
  ## Numer ---------------------------------------------------------------------
  numer <- hv_create_table_numer(list$skraning)
  
  ## Length --------------------------------------------------------------------
  lengdir <- hv_create_table_lengdir(list$skraning)
  
  ## Kvarnir -------------------------------------------------------------------
  kvarnir <- hv_create_table_kvarnir(list$skraning)
  
  ## Predator-prey -------------------------------------------------------------
  pp <- hv_create_table_prey(list$skraning)
  
  res <- 
    list(stodvar = list$stodvar,
         skraning = list$skraning,
         numer = numer,
         lengdir = lengdir,
         kvarnir = kvarnir,
         pp = pp)
  
  return(res)
}
