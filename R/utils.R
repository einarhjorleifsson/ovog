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