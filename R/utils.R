#' Standardize variable names
#'
#' @param d A dataframe or query
#'
#' @return A dataframe or query
#' @export
#'
hv_std <- function(d) {
  d |> 
    dplyr::rename(dplyr::any_of(vocabulary))
}


.geoconvert <- function (x) {
  if(is.character(x)) {
    x <- as.numeric(x)
  }
  i <- sign(x)
  x <- abs(x)
  x1 <- x%%10000
  k <- c(1:length(x1))
  k <- k[x1 > 5999 & !is.na(x1)]
  if (length(k) > 0) 
    print(paste("error > 60 min nr", k, x[k]))
  min <- (x/100) - trunc(x/10000) * 100
  return((i * (x + (200/3) * min))/10000)
}


# mutate only if column exist --------------------------------------------------
# see: https://forum.posit.co/t/mutate-only-if-a-column-exists-in-a-dataframe/100061/6
# would be nice if one could pass the function used so that one only has one
#  function
hv_ch2date <- function(df, col_name){
  col_name <- rlang::as_label(rlang::enquo(col_name))
  df |> dplyr::mutate(dplyr::across(tidyselect::any_of(col_name), lubridate::ymd))
}
hv_ch2time <- function(df, col_name){
  col_name <- rlang::as_label(rlang::enquo(col_name))
  df |> dplyr::mutate(dplyr::across(tidyselect::any_of( col_name ), lubridate::ymd_hms))
}
hv_geoconvert <- function(df, col_name){
  col_name <- rlang::as_label(rlang::enquo(col_name))
  df |>  dplyr::mutate(dplyr::across(tidyselect::any_of( col_name ), .geoconvert))
}

