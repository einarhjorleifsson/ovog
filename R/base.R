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
