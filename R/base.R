#' The base table function
#'
#' @param pth A character string specifying the path to to the data
#' @param tbl The actual table name
#'
#' @return A tibble
#' @export
hv_tbl <- function(pth, tbl) {
  pth <- paste0(pth, "/hafvog.", tbl, ".txt")
  j <- jsonlite::read_json(pth, simplifyVector = TRUE)
  v <- 
    j$v |> 
    janitor::clean_names() |> 
    tibble::as_tibble()
  return(v)
}
