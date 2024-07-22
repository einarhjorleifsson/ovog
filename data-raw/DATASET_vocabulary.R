# NOTE: 
#  There is distinction between water depth and tow depth - needs double checking

# hafvog -----------------------------------------------------------------------
v01_hafvog_leidangur <-
  tibble::tribble(~from, ~to,
                  # hafvog leidangur
                  "dags_byrjun",   "D1",
                  "dags_endir",    "D2",
                  "leidangursteg", "cclass",
                  "synaflokkur",   "sclass",
                  "veidafaeri",    "gid",
                  # CHECK which one
                  "skip",          "vid",
                  #"skip_nr",       "vid",
                  "leidangur",     "cruise") |> 
  dplyr::mutate(table = "hafvog.leidangur")
v02_hafvog_stod <- 
  tibble::tribble(~from, ~to,
                  # hafvog stöð
                  "stod_id",          ".stid",
                  "dags",             "date",
                  
                  "kastad_v_lengd",   "lon1",
                  "kastad_n_breidd",  "lat1",
                  "hift_v_lengd",     "lon2",
                  "hift_n_breidd",    "lat2",
                  
                  "reitur",           "sq",
                  "smareitur",        "ssq",
                  
                  "dypi_kastad",      "z1",
                  "dypi_hift",        "z2",
                  
                  "fishing_gear_no", "gid") |> 
  dplyr::mutate(table = "hafvog.stod")
v03_hafvog_togstod <- 
  tibble::tribble(~from, ~to,
                  
                  # hafvog tögstod
                  "vir_uti", "wire",
                  "toghradi",  "speed",
                  "togstefna", "heading",
                  "toglengd",  "towlength",
                  "togbyrjun", "t1",
                  "togtimi",   "towtime",
                  "togendir",  "t2",
                  "tognumer",  "tid",        # Think so
                  "synis_id",  ".id",
                  "lodrett_opnun", "oh",
                  "larett_opnun", "ov") |> 
  dplyr::mutate(table = "hafvog.togstod")
v04_hafvog_umhverfi <- 
  tibble::tribble(~from, ~to,
                  # hafvog umhverfi
                  "botnhiti", "bt",
                  "yfirbordshiti", "st") |> 
  dplyr::mutate(table = "hafvog.umhverfi")
v05_hafvog_skraning <- 
  tibble::tribble(~from, ~to,
                  # hafvog skraning
                  "s_nr", "nr",            # sequential number within a mtype within a species
                  #  it otolith sample (mclass = 3) then this is the
                  #  same as kvarnanumer
                  "s_lengd", "length",
                  "s_maeliadgerd", "mtype",         # these are numerics
                  "s_tegund", "sid",
                  "valkvorn", "valkvorn_CHECK",
                  "s_tegund_as_faedutegund", "sid_prey",
                  "s_fjoldi", "n",
                  
                  "s_radnr", ".sid_nr",                  # order of the measurements
                  #  within a station (synis_id)
                  "s_synis_id", ".id",
                  "s_kyn", "sex",
                  "s_oslaegt", "wt",
                  "s_kynthroski", "maturity",
                  "s_slaegt", "gwt",
                  "s_lifur", "liver",
                  "s_kynfaeri", "gonads",
                  "s_magaastand", "stomach",
                  "s_ranfiskurteg", "sid_pred",
                  "s_kvarnanr", "sknr",
                  "s_heildarthyngd", "twt") |> 
  dplyr::mutate(table = "hafvog.skraning")

# channel ----------------------------------------------------------------------
v06_channel_station_v <- 
  tibble::tribble(~from, ~to,
                  "leidangur_id", ".cid",
                  "leidangur",    "cruise",
                  "stod_id",      ".stid",
                  "stod_teg",     NA,
                  "stod_nr",      "station_no",
                  "dags",         "date",
                  "ar",           "year",
                  "man",          "month",
                  "stod_timi",    NA,
                  "kastad_breidd","lat1",   
                  "kastad_lengd", "lon1",
                  "hift_breidd",  "lat2",
                  "hift_lengd",   "lon2",
                  "reitur",       "sq",
                  "smareitur",    "ssq",       
                  "skiki",        NA,
                  "fjardarreitur",NA,
                  "botndypi_kastad","z1",
                  "botndypi_hift",  "z2",
                  "skip_nr",        "vid",    # why vessel twice
                  "londunarhofn_nr","hid",
                  "vindhradi",      NA,
                  "vindatt_nr",     NA,
                  "vedur_nr",       NA,
                  "sky_nr",         NA,
                  "sjor_nr",        NA,
                  "hafis_nr",       NA,
                  "botnhiti",       "bt",
                  "yfirbordshiti",  "st",
                  "lofthiti",       "at",
                  "loftvog",        NA,
                  "straumstefna",   NA,
                  "straumhradi",    NA,
                  "sjondypi",      "secci",
                  "hnattstada",    NA,
                  "stada_stodvar_nr", NA) |> 
  dplyr::mutate(table = "channel.station_v",
                to = ifelse(is.na(to),from, to))

v07_channel_syni_v <- 
  tibble::tribble(~from, ~to,
                  "stod_id",           ".stid",
                  "synis_id",          ".id",
                  "veidarfaeri_nr",    NA,
                  "veidarfaeri_heiti", "veidarfaeri",
                  "synaflokkur_nr",    "sclass",
                  "landsyni",          NA,
                  "marktegund_nr",     NA,
                  "syni_nr",           NA,
                  "syni_heiti",        NA,
                  "syni_aths",         NA,
                  "moskvastaerd",      "mesh",
                  "grandaralengd",     "sweeps",
                  "hofudlinulengd",    NA,
                  "hlerathyngd",       "doors_kg",
                  "hlera_m2",          "doors_m2",
                  "breidd",            NA,
                  "ummal",             NA,
                  "fjoldi_byrda",      NA, 
                  "moskvagerd_nr",     NA,
                  "skiljugerd_nr",     NA,
                  "rimlabil",          NA,
                  "leggglugga_ms",     NA,
                  "veidarf_lengd",     NA,
                  "veidarf_haed",      NA,
                  "flatarmal",         NA,
                  "thraedir",          NA,
                  "lengd_nets",        NA,
                  "haed_nets",         NA,
                  "veidarf_thyngd",    NA,
                  "onglar",            NA,
                  "fjoldi_bjoda",      NA,
                  "fjoldi_faera",      NA,
                  "ongull_nr",         NA,
                  "tog_nr",            "tid",      # CHECK
                  "togbyrjun",         "t1",
                  "togendir",          "t2",
                  "toghradi",          "speed",
                  "toglengd",          "towlength",
                  "togstefna",         "towheading",
                  "timi",              "towtime",
                  "togdypi_kastad",    "towz1",
                  "togdypi_hift",      "towz2",
                  "togdypishiti",      "towt",
                  "hlerabil",          "doorspread",
                  "lodrett_opnun",     "vo",
                  "larett_opnun",      "ho",
                  "vir_uti",           "wire",
                  "flaedi",            NA,
                  "dregid_fra",        NA,
                  "stada_synis_nr",    NA,
                  "veidarfaeri",       "gid") |> 
  dplyr::mutate(table = "channel.syni_v",
                to = ifelse(is.na(to),from, to))

v08_channel_sampleclass <- 
  tibble::tribble(~from, ~to,
                  "sample_category_no", "sclass",
                  "eng_descr", "sample_class",
                  "descr", "synaflokkur") |> 
  dplyr::mutate(table = "channel.sample_class",
                to = ifelse(is.na(to), from, to))

# biota ------------------------------------------------------------------------
v09_biota_measure <- 
  tibble::tribble(~from, ~to,
                  "measure_id",         ".mid",
                  "sample_id",          ".id",
                  "species_no",         "sid",
                  "measure_type",       "mtype",
                  "count",              "n",
                  "rate",               "r",
                  "length",             "length",
                  "sex_no",             "sex",
                  "sexual_maturity_id", "mat",
                  "maturity_id",        NA,
                  "weight",             "wt",
                  "gutted",             "gwt",
                  "stomach",            "stomach",
                  "liver",              "liver",
                  "genital",            "gonads",
                  "predator_id",        "pred_id",
                  "stomach_type",       "scondition",
                  "note",               NA,
                  "load_source",        NA,
                  "load_tegund",        NA,
                  "load_faeduhopur",    NA,
                  "load_flokkid",       NA,
                  "load_faerslunumer",  NA,
                  "load_note",          NA,
                  "load_id",            NA,
                  "fullness_no",        NA,
                  "digestion_no",       NA) |> 
  dplyr::mutate(table = "biota.measure",
                to = ifelse(is.na(to), from, to))

v <- 
  dplyr::bind_rows(v01_hafvog_leidangur,
                   v02_hafvog_stod,
                   v03_hafvog_togstod,
                   v04_hafvog_umhverfi,
                   v05_hafvog_skraning,
                   v06_channel_station_v,
                   # should really have this in package omar
                   v07_channel_syni_v,
                   v08_channel_sampleclass,
                   v09_biota_measure)


vocabulary <- stats::setNames(object = v$from, nm = v$to)

usethis::use_data(vocabulary, overwrite = TRUE)
