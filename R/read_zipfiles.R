#' Read a single hafvog zipfile 
#' 
#'
#' @param zipfile The name, including path of the hafvog zipfile
#'
#' @return a list containing individual tables
#' @export
#'
hv_read_zipfile <- function(zipfile) {
  
  files <- utils::unzip(zipfile = zipfile, list = TRUE) |> dplyr::pull(Name)
  utils::unzip(zipfile, exdir = tempdir(), overwrite = TRUE)
  res <- purrr::map(paste0(tempdir(), "/", files), hv_tbl)
  names(res) <- files |> stringr::str_remove("hafvog.") |> stringr::str_remove(".txt")
  
  # typesetting
  res$stodvar <-
    res$stodvar |> 
    dplyr::mutate(kastad_v_lengd = -.geoconvert(kastad_v_lengd),
                  kastad_n_breidd = .geoconvert(kastad_n_breidd),
                  hift_v_lengd = -.geoconvert(hift_v_lengd),
                  hift_n_breidd = .geoconvert(hift_n_breidd),
                  dags = lubridate::ymd(dags))
  res$togstodvar <-
    res$togstodvar |> 
    dplyr::mutate(togbyrjun = lubridate::ymd_hms(togbyrjun),
                  togendir  = lubridate::ymd_hms(togendir))
  
  return(res)
  
}

#' Reads hafvog's zipfiles
#'
#' Can read in multiple files. Name of source file is stored in variable ".id"
#'
#' @param zipfiles File names, including path
#' @param collapse_station boolean (default TRUE), returns station, towstation and environment as a single table
#'
#' @return a list
#' @export
#'
hv_read_hafvog <- function(zipfiles, collapse_station = TRUE) {
  
  res <- purrr::map(zipfiles, hv_read_zipfile)
  names(res) <- basename(zipfiles)
  # safest wrould be to check if list names exists
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
           drasl_skraning = res$drasl_skraning)
    res$stodvar <-
      res$stodvar |> 
      # NOTE: MISSING gear-id should really generate and error
      dplyr::mutate(index = (reitur * 100 + tognumer) * 100 + ifelse(is.na(fishing_gear_no) & synaflokkur == 30,
                                                                     73,
                                                                     fishing_gear_no),
                    .after = leidangur)
  }
  
  return(res)
  
}

hv_order_stodvar <- function(d) {
  
  tbl_colnames <- c(".file",
                    "leidangur", "skip", "dags", "reitur", "smareitur", 
                    "kastad_v_lengd", "kastad_n_breidd", "hift_v_lengd", "hift_n_breidd",
                    "dypi_kastad", "dypi_hift", "stod", "tog_aths", "synis_id",
                    "synaflokkur", "fishing_gear_no", "grandaralengd")
  dummy <- readr::read_csv("\n", col_names = tbl_colnames, col_types = "cciDiiiiiiddiciiid")
  
  d <- 
    dplyr::bind_rows(dummy,
                     d |> 
                       dplyr::select(.file, 
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
                                     synis_id,
                                     synaflokkur,
                                     fishing_gear_no,
                                     grandaralengd,
                                     dplyr::everything()))
  return(d)
}
hv_order_togstodvar <- function(d) {
  
  tbl_colnames <- c(".file",
                    "synid_id", "togbyrjun", "togendir", "togtimi", "toghradi",
                    "toglengd", "tognumer", "togstefna", "lodrett_opnun", "larett_opnun")
  dummy <- readr::read_csv("\n", col_names = tbl_colnames, col_types = "ciTTddddddd")
  d <- 
    dplyr::bind_rows(dummy,
                     d |> 
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
                                     dplyr::everything()))
  return(d)
}
hv_order_umhverfi <- function(d) {
  
  tbl_colnames <- c(".file",
                    "synid_id", "yfirbordshiti", "botnhiti")
  dummy <- readr::read_csv("\n", col_names = tbl_colnames, col_types = "cidd")
  d <- 
    dplyr::bind_rows(dummy,
                     d |> 
                       dplyr::select(.file,
                                     synis_id,
                                     yfirbordshiti,
                                     botnhiti,
                                     dplyr::everything()))
  return(d)
}
hv_order_skraning <- function(d) {
  
  tbl_colnames <- c(".file",
                    "s_synid_id", "s_maeliadgerd", "s_tegund", "s_lengd",
                    "s_fjoldi", "s_kyn", "s_kynthroski", "s_kvarnanr",
                    "s_nr", "s_oslaegt", "s_slaegt", "s_magaastand",
                    "s_lifur", "s_kynfaeri",
                    "s_tegund_as_faedutegund")
  dummy <- readr::read_csv("\n", col_names = tbl_colnames, col_types = "ciiididdiidddddi")
  d <-
    dplyr::bind_rows(dummy, 
                     d |> 
                       dplyr::select(.file,
                                     s_synis_id,
                                     s_maeliadgerd,
                                     s_tegund,
                                     s_lengd,
                                     s_fjoldi,
                                     s_kyn,
                                     s_kynthroski,
                                     s_kvarnanr,
                                     s_nr,
                                     s_oslaegt,
                                     s_slaegt,
                                     s_magaastand,
                                     s_lifur,
                                     s_kynfaeri,
                                     s_tegund_as_faedutegund,
                                     dplyr::everything()))
  return(d)
}

