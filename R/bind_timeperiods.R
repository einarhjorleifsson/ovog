#' Title
#'
#' @param current a list
#' @param historical a list
#'
#' @return a list
#' @export
#'
hv_bind_timeperiods <- function(current, historical) {
  
  iDONE <-
    current$ST |>
    dplyr::pull(index)
  
  st <- 
    dplyr::bind_rows(current$ST, 
                     historical$ST |> dplyr::filter(index %in% iDONE)) |> 
    dplyr::select(leidangur, ar, reitur, smareitur, tognumer, index, lon1:lat)
  le <- 
    dplyr::bind_rows(current$LE, 
                     historical$LE |> dplyr::filter(index %in% iDONE)) |> 
    dplyr::select(ar, index, tegund, lengd, n, b)
  kv <- 
    dplyr::bind_rows(current$KV, 
                     historical$KV |> dplyr::filter(index %in% iDONE))
  res <- list(st = st,
              le = le,
              kv = kv,
              M = current$M,
              pred = current$pred,
              prey = current$prey)
  return(res)
}