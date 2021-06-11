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

generate_key <- function(dthh, keyvars = c("V10", "V101", "V102", "V103", "V106")) {
  Reduce(function(x,y) paste(x, y, sep = "-"),
         dthh[, keyvars, with = FALSE])
}
