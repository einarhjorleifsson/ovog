#' Read and parse a json file
#'
#' @param file File name
#'
#' @return A tibble
#' 
hv_read_json <- function(file) {
  
  if(!file.exists(file)) stop(paste0("File ", file, " does not exist"))
  
  d <- 
    jsonlite::read_json(file, simplifyVector = TRUE)$values |> 
    tibble::as_tibble() |> 
    janitor::clean_names()
  
  return(d)
  
}

#' Read any hafvog zipfile
#'
#' The files to read from within the zipfile are all jsonfiles
#'
#' @param zipfile The pathname of the zip file
#'
#' @return A list of tibbles
#' 
#' @export
#'
hv_read_zipfile <- function(zipfile) {
  
  if(!file.exists(zipfile)) stop(paste0("File ", zipfile, " does not exist"))
  
  # files to read from the tmpdir (in case there are more)
  files <- unzip(zipfile, list = TRUE) |> dplyr::pull(Name)
  
  conz <- utils::unzip(zipfile, exdir = tempdir(), overwrite = TRUE)
  tables <- purrr::map(conz, hv_read_json)
  
  names(tables) <- 
    files |> 
    basename() |> 
    stringr::str_remove("hafvog.") |> 
    stringr::str_remove(".txt")
  
  return(tables)
  
}


#' Reads hafvog's 'stillingar'
#'
#'
#' @param zipfile The pathname of the zip file
#'
#' @return A list of tibbles
#' @export
#'
hv_read_stillingar <- function(zipfile) {
  hv_read_zipfile(zipfile)
}

#' Reads hafvog's 'stodtoflur'
#'
#' @param zipfile The pathname of the zip file
#'
#' @return A list of tibbles
#' @export
#'
hv_read_stodtoflur <- function(zipfile) {
  hv_read_zipfile(zipfile)
}

#' Reads hafvog's cruise data
#'
#' Can read in multiple files. Name of source file is stored in variable ".id"
#'
#' @param zipfiles File names, including path
#' @param collapse_station boolean (default TRUE), returns station, towstation  environment as a single table
#'
#' @return a list
#' @export
#'
hv_read_cruise <- function(zipfiles, collapse_station = TRUE) {
  
  res <- purrr::map(zipfiles, hv_read_zipfile)
  names(res) <- basename(zipfiles)
  # safest would be to check if list names exists
  res <- 
    list(leidangrar = purrr::map(res, "leidangrar") |> dplyr::bind_rows(.id = ".file"),
         stodvar = purrr::map(res, "stodvar") |> dplyr::bind_rows(.id = ".file"),
         togstodvar = purrr::map(res, "togstodvar") |> dplyr::bind_rows(.id = ".file"),
         umhverfi = purrr::map(res, "umhverfi") |> dplyr::bind_rows(.id = ".file"),
         skraning = purrr::map(res, "skraning") |> dplyr::bind_rows(.id = ".file"),
         drasl_skraning = purrr::map(res, "drasl_skraning") |> dplyr::bind_rows(.id = ".file"))
  
  res$stodvar <-    hv_order_stodvar(res$stodvar)
  res$togstodvar <- hv_order_togstodvar(res$togstodvar)
  res$umhverfi <-   hv_order_umhverfi(res$umhverfi)
  res$skraning <-   hv_order_skraning(res$skraning)
  
  if(collapse_station) {
    res <-
      list(stodvar = 
             res$stodvar |> 
             dplyr::left_join(res$togstodvar,
                              by = dplyr::join_by(.file, synis_id)) |> 
             dplyr::left_join(res$umhverfi,
                              by = dplyr::join_by(.file, synis_id)),
           skraning = res$skraning,
           leidangrar = res$leidangrar,
           drasl_skraning = res$drasl_skraning)
  }
  
  return(res)
  
}

hv_order_stodvar <- function(d) {
  
  tbl_colnames <- c(".file",
                    "synis_id",
                    "leidangur", "skip", "dags", "reitur", "smareitur", 
                    "kastad_v_lengd", "kastad_n_breidd", "hift_v_lengd", "hift_n_breidd",
                    "dypi_kastad", "dypi_hift", "stod", "tog_aths",
                    "synaflokkur", "fishing_gear_no", "grandaralengd")
  dummy <- readr::read_csv("\n", col_names = tbl_colnames, col_types = "ciciDiiiiiiddiciid")
  
  d <- 
    dplyr::bind_rows(dummy,
                     d |> 
                       dplyr::select(.file, 
                                     synis_id,
                                     leidangur,
                                     #  Note, no station id
                                     skip,
                                     dags,
                                     reitur,
                                     smareitur,
                                     kastad_v_lengd,
                                     kastad_n_breidd,
                                     hift_v_lengd,
                                     hift_n_breidd,
                                     dypi_kastad,
                                     dypi_hift,
                                     stod,
                                     tog_aths,
                                     # sample variables
                                     synaflokkur,
                                     fishing_gear_no,
                                     grandaralengd,
                                     dplyr::everything()))
  return(d)
}
hv_order_togstodvar <- function(d) {
  
  tbl_colnames <- c(".file",
                    "synis_id", "togbyrjun", "togendir", "togtimi", "toghradi",
                    "toglengd", "tognumer", "togstefna", "lodrett_opnun", "larett_opnun")
  dummy <- readr::read_csv("\n", col_names = tbl_colnames, col_types = "ciTTddddddd")
  d <- 
    dplyr::bind_rows(dummy,
                     d) |>
    dplyr::select(.file,
                  synis_id,
                  togbyrjun,
                  togendir,
                  togtimi,
                  toghradi,
                  toglengd,
                  tognumer,
                  togstefna,
                  lodrett_opnun,
                  larett_opnun,
                  dplyr::everything())
  return(d)
}

hv_order_umhverfi <- function(d) {
  
  tbl_colnames <- c(".file",
                    "synis_id", "yfirbordshiti", "botnhiti")
  dummy <- readr::read_csv("\n", col_names = tbl_colnames, col_types = "cidd")
  d <- 
    dplyr::bind_rows(dummy,
                     d) |> 
    dplyr::select(.file,
                  synis_id,
                  yfirbordshiti,
                  botnhiti,
                  dplyr::everything())
  return(d)
}

hv_order_skraning <- function(d) {
  
  tbl_colnames <- c(".file",
                    "s_synis_id", "s_maeliadgerd", "s_tegund", "s_lengd",
                    "s_fjoldi", "s_kyn", "s_kynthroski", "s_kvarnanr",
                    "s_nr", "s_oslaegt", "s_slaegt", "s_magaastand",
                    "s_lifur", "s_kynfaeri",
                    "s_tegund_as_faedutegund",
                    "s_radnr",
                    "s_ranfiskurteg",
                    "s_heildarthyngd")
  dummy <- readr::read_csv("\n", col_names = tbl_colnames, col_types = "ciiididdiidddddiiid")
  d <-
    dplyr::bind_rows(dummy, 
                     d) |> 
    # one could make this more simple by just dropping s_ from name
    dplyr::select(.file,
                  synis_id = s_synis_id,
                  maeliadgerd = s_maeliadgerd,
                  tegund = s_tegund,
                  lengd = s_lengd,
                  fjoldi = s_fjoldi,
                  kyn = s_kyn,
                  kynthroski = s_kynthroski,
                  kvarnanr = s_kvarnanr,
                  nr = s_nr,
                  oslaegt = s_oslaegt,
                  slaegt = s_slaegt,
                  magaastand = s_magaastand,
                  lifur = s_lifur,
                  kynfaeri = s_kynfaeri,
                  tegund_as_faedutegund = s_tegund_as_faedutegund,
                  radnr = s_radnr,
                  ranfiskurteg = s_ranfiskurteg, 
                  heildarthyngd = s_heildarthyngd,
                  dplyr::everything())
  return(d)
}

