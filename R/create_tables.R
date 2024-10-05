# For testing
# devtools::load_all()
# res <-  hv_read_cruise(c("~/R/Pakkar2/osmx/data-raw/SMH/TB2-2024.zip", "~/R/Pakkar2/osmx/data-raw/SMH/TTH1-2024.zip"))
# hv_create_table_kvarnir(res$skraning)



hv_create_table_kvarnir <- function(skraning) {
  
  message("Einar: Should potentially add stomach weight")
  kvarnir <-
    skraning |> 
    dplyr::filter(maeliadgerd == 3) |> 
    dplyr::select(.file:stod,
                  tegund, nr, lengd, kyn,
                  kynthroski, oslaegt, slaegt,
                  lifur, kynfaeri,
                  magaastand)
  
  return(kvarnir)
}

hv_create_table_numer <- function(skraning) {
  
  numer  <- 
    skraning |> 
    dplyr::group_by(.file, synis_id, leidangur, dags, index, stod, tegund, m) |> 
    dplyr::summarise(n = sum(fjoldi),
                     .groups = "drop") |> 
    tidyr::pivot_wider(names_from = m, values_from = n, values_fill = 0) |> 
    dplyr::mutate(alls = maelt + talid) |> 
    dplyr::rename(fj_maelt = maelt,
                  fj_talid = talid,
                  fj_alls = alls) |> 
    dplyr::mutate(r = dplyr::case_when(fj_maelt == 0 ~ 1,
                                       .default = fj_alls / fj_maelt))
  
  return(numer)
}

hv_create_table_lengdir <- function(skraning, scale_by_counted = TRUE) {
  
  if(scale_by_counted) {
    numer <- hv_create_table_numer(skraning)
  }
  
  lengdir <- 
    skraning |> 
    dplyr::filter(m %in% "maelt") |> 
    dplyr::group_by(.file, synis_id, leidangur, dags, index, stod, tegund, lengd) |> 
    dplyr::reframe(n = sum(fjoldi, na.rm = TRUE))
  
  if(scale_by_counted) {
    lengdir <- 
      lengdir |> 
      dplyr::left_join(numer |> 
                         dplyr::select(.file, synis_id, leidangur, dags, index, stod, tegund, r),
                       by = dplyr::join_by(.file, synis_id, leidangur, dags, index, stod, tegund)) |> 
      dplyr::mutate(n = r * n)
  }
  
  lengdir <- 
    lengdir |> 
    dplyr::mutate(b = (n * 0.01 * lengd^3) / 1e3)  # biomass in kg
  
  return(lengdir)
}



hv_create_table_prey <- function(skraning) {
  
  ## Predators -----------------------------------------------------------------
  pred <- 
    skraning |> 
    dplyr::filter(!is.na(magaastand)) |> 
    dplyr::select(.file:stod,
                  pred = tegund, 
                  pred_nr = nr, 
                  pred_lengd = lengd,
                  oslaegt,
                  slaegt,
                  astand = magaastand)
  
  ## Prey ----------------------------------------------------------------------
  prey <-
    skraning |> 
    dplyr::filter(maeliadgerd %in% c(20, 21, 22)) |> 
    dplyr::rename(prey_nr = nr) |> 
    dplyr::select(.file:stod,
                  pred = ranfiskurteg,      # will join by this
                  pred_nr = kvarnanr,       # will join by this
                  prey = tegund,
                  prey_nr,
                  n = fjoldi,
                  prey_lengd = lengd,
                  kyn,
                  heildarthyngd) 

  
  pp <- 
    pred |> 
    dplyr::left_join(prey,
                     by = dplyr::join_by(.file, synis_id, leidangur, dags, index, stod, pred, pred_nr)) |> 
    dplyr::mutate(heildarthyngd = tidyr::replace_na(heildarthyngd, 0))
  
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
         numer = numer,
         lengdir = lengdir,
         kvarnir = kvarnir,
         pred = pred,
         prey = prey,
         pp = pp)
  
  return(res)
}
