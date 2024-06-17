#' Checks for index overlap in a list of multiple hafvog survey files
#'
#' @param zip_file List of hafog inputs read infrom multiple survey zips
#'
#' @return logical, is there overlap in the 'synis_id'-s from different hafvog instances
#' @export

id_overlap <- function(surv_list) {
  surv_list |>
    purrr::map(\(l) l$ST$index) |>
    unlist() |>
    table() -> n_id_occ
  any(n_id_occ > 1)
}

#' Generates non-overlapping indices for a list of hafvog surveys
#'
#' @param surv_list List of Hafog data read infrom multiple survey zips
#'
#' @return list of sequential id-s one after the other for the multiple inputs
#' @export

new_index <- function(surv_list) {
  surv_list |>
    purrr::map_vec(\(l)
      nrow(l$ST)) |>
    cumsum() -> last

  first <- c(1,last[-length(last)]+1)
  tibble::tibble(first,last) -> ids

  purrr::map2(ids$first,ids$last,function(first,last) seq(first,last))
}

#' Re-indexes the second to last table of a survey input with a new index
#'
#' @param surv Hafog data read in from single survey
#'
#' @return survey list, column 'newid' added to all tables
#' @export

re_index <- function(surv)
  {
  ST <- surv$ST
  for(i in 2:length(surv)) {
    t <- surv[[i]]
    t |>
      dplyr::left_join(ST |> 
        dplyr::select(index,newid), by="index") -> t
    surv[[i]] <- t
  }
  surv
}

#' Re-indexes list of Hafvog data if necessary
#'
#' @param surv_list List of Hafog data read infrom multiple survey zips
#'
#' @return Original list if no overlap, otherwise re-indexed version
#' @export

check_and_re_index <- function(surv_list) {
  if(!id_overlap(surv_list))
  {
    warning("No overlap, data passed trough!")
    surv_list
  } else
  {
    # create new IDs for all surveys
    new_index(surv_list) -> new_ids 
    # add them to the ST-objects for each survey
    purrr::map2(surv_list,new_ids,
      function(surv_list,new_ids) {
        surv_list$ST$newid <- new_ids
        surv_list
      }) -> surv_list
   # join the newid-column to all objects within each survey
   surv_list |>
     purrr::map(surv_list, re_index)
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
