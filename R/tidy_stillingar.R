# QC functions
#   For testing:
#   devtools::load_all()
#   stillingar <- ovog::hv_read_stillingar("~/R/Pakkar2/osmx/data-raw/SMH/stillingar_SMH_rall_(haust).zip")


#' Create a tidy QC species min-max table
#' 
#' The resulting tibble contains the min and max lengd for a species and the 
#' ratio range of values relative to the ungutted species weight.
#'
#' The datasource is a list that contains object "fiskteg_tegundir". The list
#' is normally obtained using hv_read_stillingar.
#'
#' @param stillingar A tibble
#' @param long Boolean (default TRUE) returns a long table
#'
#' @return A tibble
#' 
#' @note The variable "maelibretti" is the odd man out
#' 
#' @export
#'
hv_tidy_range <- function(stillingar, long = TRUE) {
  
  tmp <- 
    stillingar$fiskteg_tegundir |> 
    dplyr::select(tegund = fisktegund_id,
                  leidangur_id,
                  maelibretti,
                  heildarthyngd_i_kg,
                  dplyr::everything()) |> 
    dplyr::arrange(tegund)
  
  # One should be able to use pivot_longer and do thing in one pipe flow
  low <- 
    tmp |> 
    dplyr::select(tegund:maelibretti, tidyselect::ends_with("_low")) |>  
    tidyr::gather(var, val, -c(tegund:maelibretti)) |> 
    dplyr::mutate(var = stringr::str_remove(var, "^oslaegt_")) |> 
    tidyr::separate(var, c("var", "range"), sep = "_") |> 
    dplyr::mutate(val = dplyr::case_when(var == "lengd" ~ val,
                                         .default = val / 100))
  
  hig <- 
    tmp |> 
    dplyr::select(tegund:maelibretti, tidyselect::ends_with("_high")) |>  
    tidyr::gather(var, val, -c(tegund:maelibretti)) |> 
    dplyr::mutate(var = stringr::str_remove(var, "^oslaegt_")) |> 
    tidyr::separate(var, c("var", "range"), sep = "_") |> 
    dplyr::mutate(val = dplyr::case_when(var == "lengd" ~ val,
                                         .default = val / 100))
  
  range <- 
    dplyr::bind_rows(low, hig) |> 
    dplyr::arrange(tegund, var, dplyr::desc(range))
  
  if(long) {
    range <- 
      range |> 
      tidyr::spread(range, val)
  } else {
    range <- 
      range |> 
      tidyr::unite("var", var, range) |> 
      tidyr::spread(var, val)
  }

  return(range)
  
}

#' Create a tidy QC species length-weight table
#' 
#' The resulting tibble contains ...
#'
#' The datasource is a list that contains object "fiskteg_lengd_thyngd". The list
#' is normally obtained using hv_read_stillingar.
#'
#' @param stillingar A tibble
#' @param adjust_lengths A boolean (default TRUE) where the max length for
#' the length-weight is potentially obtained from the "range" table. And
#' if only one value for length-weights, gets the minimum from the range table
#'
#' @return A tibble
#'
#' @export
#' 
hv_tidy_length_weights <- function(stillingar, adjust_lengths = TRUE) {
  
  # Ugly code, must be a better way
  d <-
    stillingar$fiskteg_lengd_thyngd |> 
    dplyr::select(tegund = fisktegund_id, lengd, fravik, dplyr::everything()) |> 
    dplyr::arrange(tegund, lengd)
  
  # We have two issues here:
  #  1. Maximum length is not the same in the range data and the lw-data
  #  2. Minimum length is not always reported in the lw-data
  if(adjust_lengths) {
    # 1. Adjust maximum length
    max.lengd <- 
      stillingar$fiskteg_tegundir |> 
      dplyr::select(tegund = fisktegund_id, lengd_high) |> 
      tidyr::drop_na() |> 
      dplyr::mutate(max = TRUE)
    # Adjust the max length
    d <- 
      d |> 
      dplyr::group_by(tegund) |> 
      dplyr::mutate(max = dplyr::case_when(lengd == max(lengd) ~ TRUE,
                                           .default = FALSE)) |> 
      dplyr::ungroup() |> 
      dplyr::left_join(max.lengd,
                       by = dplyr::join_by(tegund, max)) |> 
      dplyr::mutate(lengd = dplyr::case_when(max & lengd >= lengd_high ~ lengd,
                                             max & lengd <  lengd_high ~ lengd_high,
                                             .default = lengd)) |> 
      dplyr::select(-c(max, lengd_high))
    # 2. add the minimum length
    min.lengd <- 
      stillingar$fiskteg_tegundir |> 
      dplyr::select(tegund = fisktegund_id, lengd = lengd_low) |> 
      tidyr::drop_na() |>  
      dplyr::filter(tegund %in% d$tegund)
    d <- 
      d |> 
      dplyr::bind_rows(min.lengd) |> 
      dplyr::arrange(tegund, lengd) |> 
      dplyr::group_by(tegund) |> 
      tidyr::fill(fravik:slaegt_a, .direction = "updown") |> 
      dplyr::ungroup()
  }
  
  d <- 
    d |> 
    dplyr::group_by(tegund) |> 
    tidyr::complete(lengd = tidyr::full_seq(lengd, 1)) |> 
    tidyr::fill(fravik:slaegt_a) |> 
    dplyr::ungroup() |> 
    dplyr::arrange(tegund, lengd)
    
  
  # unsure if this is kosher
  if(FALSE) {
    d <- 
      d |> 
      tidyr::gather(var, val, oslaegt_b:slaegt_a) |> 
      tidyr::separate(var, c("var", "par"), sep = "_") 
    
    
    d <- 
      dplyr::bind_rows(d |> dplyr::filter(var == "oslaegt") |> dplyr::distinct() |> tidyr::spread(par, val),
                       d |> dplyr::filter(var == "slaegt") |> dplyr::distinct() |> tidyr::spread(par, val))
  }
  
  d <- 
    d |> 
    dplyr:: mutate(fravik = fravik / 100,
                   osl  = oslaegt_a * lengd^oslaegt_b,
                   sl   = slaegt_a  * lengd^slaegt_b,
                   osl1 = osl * (1 - fravik),
                   osl2 = osl * (1 + fravik),
                   sl1  = sl  * (1 - fravik),
                   sl2  = sl  * (1 + fravik))
  
  return(d)
  
}

