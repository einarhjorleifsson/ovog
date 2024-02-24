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
#'
#' @return A tibble
#' @export
hv_station <- function(pth, std = TRUE) {
  d <- 
    hv_tbl(pth, "stodvar") |> 
    dplyr::mutate(dags = lubridate::ymd(dags), 
                  hift_n_breidd =    geo::geoconvert.1(as.numeric(hift_n_breidd)),
                  kastad_n_breidd =  geo::geoconvert.1(as.numeric(kastad_n_breidd)),
                  hift_v_lengd =    -geo::geoconvert.1(as.numeric(hift_v_lengd)),
                  kastad_v_lengd =  -geo::geoconvert.1(as.numeric(kastad_v_lengd)))
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
#'
#' @return A tibble
#' @export
hv_towstations <- function(pth, std = TRUE) {
  d <- 
    hv_tbl(pth, "togstodvar") |> 
    dplyr::mutate(togbyrjun = lubridate::ymd_hms(togbyrjun),
                  togendir = lubridate::ymd_hms(togendir)
                  )
  if(std) {
    d <- 
      d |> 
      dplyr::rename(dplyr::any_of(vocabulary))
  }
  
  return(d)
  
}

#' The environment (umhverfi) table
#'
#' @param pth A character string specifying the path to to the data
#' @param std Boolean (default TRUE) specifying if names should be standardized
#'
#' @return A tibble
#' @export
hv_environment <- function(pth, std = TRUE) {
  d <- 
    hv_tbl(pth, "umhverfi")
  if(std) {
    d <- 
      d |> 
      dplyr::rename(dplyr::any_of(vocabulary))
  }
  
  return(d)
  
}


#' The measurement (skraning) table
#'
#' @param pth A character string specifying the path to to the data
#' @param std Boolean (default TRUE) specifying if names should be standardized
#'
#' @return A tibble
#' @export
hv_measures <- function(pth, std = TRUE) {
  d <- 
    hv_tbl(pth, "skraning") |> 
    dplyr::rename_all(~stringr::str_replace(.,"^s_","")) |> 
    dplyr::glimpse()
  if(std) {
    d <- 
      d |> 
      dplyr::rename(dplyr::any_of(vocabulary))
  }
  
  return(d)
  
}
