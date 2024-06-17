#' Checks for station number overlap in a list of multiple hafvog survey files
#'
#' @param zip_file List of hafog inputs read infrom multiple survey zips
#'
#' @return logical, is there overlap in the 'stod'/stations numbers from different hafvog instance/surveys/vessels
#' @export

st_overlap <- function(surv_list) {
  surv_list |>
    purrr::map(\(l) l$ST$stod) |>
    unlist() |>
    table() -> n_st_occ
  any(n_st_occ>1)
}

#' Generates non-overlapping indices for a list of hafvog surveys
#'
#' @param surv_list List of Hafog data read infrom multiple survey zips
#'
#' @return list of sequential id-s one after the other for the multiple inputs
#' @export

new_stations <- function(surv_list,jump_by=100) {
  for(i in 1:length(surv_list)) {
    ST <- surv_list[[i]]$ST
    ST$stod <- (i-1)*jump_by+ST$stod
    surv_list[[i]]$ST <- ST
  }
  surv_list
}

#' Re-number stations with jump between surveys in alist of Hafvog data if necessary
#'
#' @param surv_list List of Hafog data read in from multiple survey zips
#'
#' @return Original list if no station nummber overlap, otherwise re-numbered stations version
#' @export

check_and_re_number_stations <- function(surv_list,jump_by=100) {
  if(!st_overlap(surv_list))
  {
    warning("No overlap, data passed trough!")
    surv_list
  } else
  {
    # create new station IDs for all surveys
    new_stations(surv_list,jump_by=jump_by)
  }
}


#' Concatenates a list of Hafvog data
#'
#' @param surv_list List of Hafog data read infrom multiple survey zips
#'
#' @return A single hafvog data list with concatenated
#' frames 'ST','M','KV','LE','pred','prey'.
#' @export

hv_concat <- function(surv_list) 
  purrr::pmap(surv_list,bind_rows)
