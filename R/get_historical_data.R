#' Read historical data
#'
#' @param AR The current data year, year less than that will be returned
#' @param SYNAFLOKKUR The survey sample class, normally either 30 or 35
#'
#' @return a list
#' @export
hv_read_historical <- function(AR, SYNAFLOKKUR) {
  
  # Main data from mar dump ----------------------------------------------------
  ST <-
    mardata::stod |>
    dplyr::filter(ar < AR) |>
    dplyr::left_join(mardata::syni,
                     by = "stod_id") |>
    dplyr::filter(synaflokkur_nr %in% SYNAFLOKKUR) |>
    dplyr::mutate(index = reitur * 100 + tog_nr) |>
    dplyr::select(synis_id,
                  leidangur,
                  dags,
                  skip = skip_nr,
                  stod = stod_nr,   # check
                  reitur,
                  smareitur,
                  kastad_n_breidd = kastad_breidd,
                  kastad_v_lengd = kastad_lengd,
                  hift_n_breidd = hift_breidd,
                  hift_v_lengd = hift_lengd,
                  dypi_kastad = botndypi_kastad,
                  dypi_hift = botndypi_hift,
                  veidarfaeri,
                  moskvastaerd,
                  grandaralengd,
                  #heildarafli,
                  synaflokkur = synaflokkur_nr,
                  ar,
                  togbyrjun,
                  togendir,
                  toghradi,
                  toglengd,
                  vir_uti,
                  lodrett_opnun,
                  tognumer = tog_nr,
                  togstefna,
                  larett_opnun,
                  togtimi = timi,
                  togdypi_kastad,
                  togdypi_hift,
                  togdypishiti,
                  # eykt,
                  vindhradi,
                  vindatt = vindatt_nr,
                  vedur = vedur_nr,
                  sky = sky_nr,
                  sjor = sjor_nr,
                  botnhiti,
                  yfirbordshiti,
                  hafis = hafis_nr,
                  straumstefna,
                  straumhradi,
                  sjondypi,
                  index
    ) |>
    dplyr::mutate(lon1 = kastad_v_lengd,
                  lat1 = kastad_n_breidd,
                  lon2 = hift_v_lengd,
                  lat2 = hift_n_breidd) |>
    #geo::geoconvert(col.names = c("lat1", "lon1")) |>
    #geo::geoconvert(col.names = c("lat2", "lon2")) |>
    dplyr::mutate(lon = (lon1 + lon2) / 2,
                  lat = (lat1 + lat2) / 2,
                  toglengd = ifelse(is.na(toglengd), 4, toglengd))
  
  NU <-
    mardata::skala |>
    dplyr::filter(synis_id %in% ST$synis_id) |>
    dplyr::select(synis_id,
                  tegund = tegund_nr,
                  fj_maelt = maeldir,
                  fj_talid = taldir) |>
    dplyr::mutate(fj_alls = fj_maelt + fj_talid)
  
  LE <-
    mardata::lengd |>
    dplyr::filter(synis_id %in%ST$synis_id) |>
    dplyr::select(synis_id,
                  tegund = tegund_nr,
                  lengd,
                  fjoldi)
  
  KV <-
    mardata::aldur |>
    dplyr::filter(synis_id %in%ST$synis_id) |>
    dplyr::select(synis_id,
                  tegund = tegund_nr,
                  nr = kvarna_nr,
                  lengd,
                  kyn = kyn_nr,
                  kynthroski_nr,
                  aldur,
                  oslaegt = thyngd,
                  slaegt,
                  kynfaeri,
                  lifur,
                  magi)
  
  return(list(ST = ST, NU = NU, LE = LE, KV = KV))
  
}