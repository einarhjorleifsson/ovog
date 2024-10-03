#' Read in a single json file from a zip-file
#'
#' @param zipfile The path and the name of the zip file
#' @param file The json file to read
#'
#' @return a tibble
#' 
tbl_json <- function(zipfile, file) {
  conz <- unzip(zipfile = zipfile, file = file, exdir = tempdir())
  d <-
    jsonlite::read_json(conz, simplifyVector = TRUE)$values |> 
    janitor::clean_names() |> 
    tibble::as_tibble()
  #unlink(conz)
  return(d)
}


#' Title
#'
#' The current form is just mimicry
#'
#' @param zipfile The pathname of the zip file
#'
#' @export
#'
hv_read_stillingar <- function(zipfile = "data-raw/stillingar/stillingar_SMB_rall_(botnfiskur).zip") {
  tables <- unzip(zipfile, list = TRUE) |> dplyr::pull(Name)
  stillingar <- purrr::map2(zipfile, tables, tbl_json)
  names(stillingar) <- 
    tables |> 
    stringr::str_remove("hafvog.") |> 
    stringr::str_remove(".txt")
  return(stillingar)
}


