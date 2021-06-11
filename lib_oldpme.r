.pme_dir <- "/home/gustavo/Dropbox/v2/data/PME/FullData"

oldpmeread <- function(unit = "", timeframe = "", pme_dir = .pme_dir) {
  regex1 <- switch(unit, "person" = "person-",
                   "household" = "household-",
                   "(person|household)-")
  regex2 <- switch(timeframe, "80s" = "19(8.|90)",
                   "90s" = "(199[1-9]|2000)",
                   "(19[0-9]{2}|2000)")

  regex <- paste0(regex1, regex2)

  fff <- list.files(.pme_dir, pattern = regex, full.names = TRUE)

  ldata <- lapply(fff, function(fname) {

    rrr <- ".*(person|household)-([0-9]{4})\\.csv"
    ftype <- gsub(rrr, "\\1", fname)
    year <- as.integer(gsub(rrr, "\\2", fname))


    DT <- data.table::fread(fname)

    DT[, .year := year]

    return(list(
      .year = year,
      .unit = ftype,
      DT = DT
    ))
  })
  return(ldata)
}

oldpme_flag_bad_state <- function(hh_old, flagname = ".flbadstate") {
  valid_states_V10 <- c(11, 21, 33, 41, 56, 59)
  valid_states_UF <- c(26, 29, 31, 33, 35, 43)

  hh_old[!is.na(V10), (flagname) := !(V10 %in% valid_states_V10)]
  hh_old[!is.na(UF), (flagname) := !(UF %in% valid_states_UF)]

}

generate_key <- function(dthh, keyvars = c("V10", "V101", "V102", "V103", "V106")) {
  vkey <- Reduce(function(x,y) paste(x, y, sep = "-"),
                 dthh[, keyvars, with = FALSE])
  vna <- Reduce(function(x,y) x | y,
                lapply(dthh[, keyvars, with = FALSE], is.na))

  vkey[vna] <- NA_character_

  return(vkey)
}
