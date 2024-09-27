#' The base table function
#'
#' @param file File name
#'
#' @return A tibble
#' @export
hv_tbl <- function(file) {
  j <- jsonlite::read_json(file, simplifyVector = TRUE)
  v <- 
    j$v |> 
    janitor::clean_names() |> 
    tibble::as_tibble()
  return(v)
}


#' Read a single hafvog zipfile 
#' 
#'
#' @param zipfile The name, including path of the hafvog zipfile
#'
#' @return a list containing individual tables
#' @export
#'
hv_read_zipfile <- function(zipfile) {
  
  tmpdir <- tempdir()
  if(file.exists(paste0(tmpdir, "/hafvog.leidangrar.txt"))) file.remove(paste0(tmpdir, "/hafvog.leidangrar.txt"))
  if(file.exists(paste0(tmpdir, "/hafvog.skraning.txt"))) file.remove(paste0(tmpdir, "/hafvog.skraning.txt"))
  if(file.exists(paste0(tmpdir, "/hafvog.stodvar.txt"))) file.remove(paste0(tmpdir, "/hafvog.stodvar.txt"))
  if(file.exists(paste0(tmpdir, "/hafvog.togstodvar.txt"))) file.remove(paste0(tmpdir, "/hafvog.togstodvar.txt"))
  if(file.exists(paste0(tmpdir, "/hafvog.umhverfi.txt"))) file.remove(paste0(tmpdir, "/hafvog.umhverfi.txt"))
  
  files <- utils::unzip(zipfile = zipfile, list = TRUE) |> dplyr::pull(Name)
  utils::unzip(zipfile, exdir = tempdir(), overwrite = TRUE)
  res <- purrr::map(paste0(tempdir(), "/", files), hv_tbl)
  names(res) <- files |> stringr::str_remove("hafvog.") |> stringr::str_remove(".txt")
  
  # typesetting
  res$stodvar <-
    res$stodvar |> 
    # mutate only if column exist
    hv_ch2date(dags) |>  
    # Note: check how to deal with the negative
    hv_geoconvert(kastad_v_lengd) |> 
    hv_geoconvert(hift_v_lengd) |> 
    hv_geoconvert(kastad_n_breidd) |> 
    hv_geoconvert(hift_n_breidd)

  res$togstodvar <-
    res$togstodvar |> 
    # mutate only if column exist
    hv_ch2time(togbyrjun) |> 
    hv_ch2time(togendir)

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
hv_read_zips <- function(zipfiles, collapse_station = TRUE) {
  
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

