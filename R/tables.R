#' The cruise (leidangur) table
#'
#' @param pth A character string specifying the path to to the data
#' @param std Boolean (default TRUE) specifying if names should be standardized
#'
#' @return A tibble
#' @export
hv_cruise <- function(pth, std = TRUE) {
  d <- 
    hv_tbl(pth, "leidangrar") |> 
    dplyr::mutate(dags_byrjun = lubridate::ymd(dags_byrjun),
                  dags_endir = lubridate::ymd(dags_endir))
  if(std) {
    d <- 
      d |> 
      dplyr::rename(dplyr::any_of(vocabulary))
  }
  
  return(d)
  
}

#' The station (stodvar) table
#'
#' @param pth A character string specifying the path to to the data
#' @param std Boolean (default TRUE) specifying if names should be standardized
#' @param trim Boolean (default TRUE) to limit number of variables returned
#'
#' @return A tibble
#' @export
hv_station0 <- function(pth, std = TRUE, trim = TRUE) {
  
  d <- 
    hv_tbl(pth, "stodvar") |>
    dplyr::mutate(dags = lubridate::ymd(dags), 
                  hift_n_breidd =    geo::geoconvert.1(as.numeric(hift_n_breidd)),
                  kastad_n_breidd =  geo::geoconvert.1(as.numeric(kastad_n_breidd)),
                  hift_v_lengd =    -geo::geoconvert.1(as.numeric(hift_v_lengd)),
                  kastad_v_lengd =  -geo::geoconvert.1(as.numeric(kastad_v_lengd)))
  
  # Get the order right, first start with what is in station proper
  d <- 
    d |> 
    dplyr::select(
      # Cruise proper
      leidangur,
      # Station proper
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
      #aths,
      dplyr::everything()
    )
  
  
  
  if(std) {
    d <- 
      d |> 
      dplyr::rename(dplyr::any_of(vocabulary))
    
    if(trim) {
      d <-
        d |> 
        dplyr::select(cruise:gid)
    }
  }
  
  
  
  return(d)
  
}


#' The station (stodvar) table
#'
#' @param pth A character string specifying the path to to the data
#' @param std Boolean (default TRUE) specifying if names should be standardized
#' @param trim Boolean (default TRUE) to limit number of variables returned
#'
#' @return A tibble
#' @export
hv_station <- function(pth, std = TRUE, trim = TRUE) {
  
  d <- 
    hv_station0(pth, std = FALSE)
  
  # joins
  d <-
    d |> 
    dplyr::left_join(hv_towstations(pth, std = FALSE),
                     by = dplyr::join_by(synis_id)) |> 
    dplyr::left_join(hv_environment(pth, std = FALSE),
                     by = dplyr::join_by(synis_id))
  
  
  if(std) {
    d <- 
      d |> 
      dplyr::rename(dplyr::any_of(vocabulary))
  }
  
  return(d)
  
}


#' The towstation (togstodvar) table
#'
#' @param pth A character string specifying the path to to the data
#' @param std Boolean (default TRUE) specifying if names should be standardized
#' @param trim Boolean (default TRUE) to limit number of variables returned
#'
#' @return A tibble
#' @export
hv_towstations <- function(pth, std = TRUE, trim = TRUE) {
  d <- 
    hv_tbl(pth, "togstodvar") |> 
    dplyr::mutate(togbyrjun = lubridate::ymd_hms(togbyrjun),
                  togendir = lubridate::ymd_hms(togendir)
    ) |> 
    dplyr::select(synis_id,
                  togbyrjun,
                  togendir,
                  togtimi,
                  toghradi,
                  toglengd,
                  tognumer,
                  dplyr::everything())
  
  if(std) {
    d <- 
      d |> 
      dplyr::rename(dplyr::any_of(vocabulary))
    if(trim) {
      d <- 
        d |> 
        dplyr::select(.sid:townumber)
    }
    
  }
  
  return(d)
  
}

#' The environment (umhverfi) table
#'
#' @param pth A character string specifying the path to to the data
#' @param std Boolean (default TRUE) specifying if names should be standardized
#' @param trim Boolean (default TRUE) to limit number of variables returned
#' 
#' @return A tibble
#' @export
hv_environment <- function(pth, std = TRUE, trim = TRUE) {
  
  d <- 
    hv_tbl(pth, "umhverfi") |> 
    dplyr::select(synis_id,
                  yfirbordshiti,
                  botnhiti,
                  lofthiti,
                  dplyr::everything())
  
  if(std) {
    d <- 
      d |> 
      dplyr::rename(dplyr::any_of(vocabulary))
    if(trim) {
      d <- 
        d |> 
        dplyr::select(.sid, st, bt)
    }
    
  }
  
  return(d)
  
}


#' The measurement (skraning) table
#'
#' @param pth A character string specifying the path to to the data
#' @param std Boolean (default TRUE) specifying if names should be standardized
#' @param trim Boolean (default TRUE) to limit number of variables returned. Not active yet.
#' 
#' @return A tibble
#' @export
hv_measures <- function(pth, std = TRUE, trim = TRUE) {
  d <- 
    hv_tbl(pth, "skraning") |> 
    dplyr::rename_all(~stringr::str_replace(.,"^s_","")) |> 
    dplyr::select(synis_id,
                  maeliadgerd,
                  tegund,
                  lengd,
                  fjoldi,
                  kyn,
                  kynthroski,
                  oslaegt,
                  slaegt,
                  magaastand,
                  lifur,
                  kynfaeri,
                  kvarnanr,
                  dplyr::everything())
                 
  if(std) {
    d <- 
      d |> 
      dplyr::rename(dplyr::any_of(vocabulary))
    if(trim) {
      
    }
  }
  
  return(d)
  
}
