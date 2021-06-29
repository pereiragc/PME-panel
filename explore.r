library(data.table)
library(dtsnippets)

source("lib_oldpme.r", chdir = TRUE)

.pme_dir <- "/home/gustavo/Dropbox/v2/data/PME/FullData"


## -----------------------------------------------------------------------------
##                                 PLAYGROUND
## -----------------------------------------------------------------------------

## - hhid :: household ID (see previous sections)
## - V105 :: month
## - .year :: year
## - V202 :: sex
## - .bdate :: birth date (see previous sections)
## - .age :: inferred age (see previous sections)
## - V201 :: person number (in portuguese, /número de ordem/)
## - V206 :: birth day (1 - 31)
## - V236 :: birth month
## - V246 :: birth year
## - V210 :: degree (0-9)
## - V203 :: household role (in portuguese, /condição no domicílio/)

pp_old_samp[!(ambiguous_id1)]
pp_old_samp[!is.na(V202) & !is.na(V203) &
          !is.na(V210), person_id2 := .GRP, .(hhid, V202, V203, V210)]



pp_old_samp[, .tmp_age := frollapply(.age, 2,
                                     function(x) {
                                       agediff <- x[2] - x[1]
                                       return(!(agediff %in% c(0,1)))
                                     })]

setkey(pp_old_samp, hhid, person_id1, person_id2)


DT <- data.table(
  a = c("A", NA, "B", NA, "B", "A", "C"),
  b = c(1, 2, 3, 4, 1, NA, 1)
)



pp_old[(.fl_available_transition), .(transition_code = .GRP), .(.labor_status_L1, .labor_status)]

pp_old[hhid == "MG-126-31002021-12-4", .(hhid, .age, V202, .year, V105, person_id1, person_id2)]

DT <- pp_old[hhid == "MG-126-31002021-12-4", .(hhid, .age, V202, .year, V105)]

setkey(DT, V202, .age, .year, V105)

DT[, .tag_age_change := frollapply(.age, 2,
                                   function(x) {
                                     agediff <- x[2] - x[1]
                                     return(!(agediff %in% c(0,1)))
                                   },
                                   fill = 1),
   .(hhid, V202)]







## -----------------------------------------------------------------------------
## |                                Old PME                                    |
## -----------------------------------------------------------------------------


pp_old <- rbindlist(lapply(oldpmeread("person"), function(ll) ll$DT), fill = TRUE)
hh_old <- rbindlist(lapply(oldpmeread("household"), function(ll) ll$DT), fill = TRUE)


## Make some things uniform: `UF` should be the same as `V10`

hh_old[is.na(V10), V10 := UF]
hh_old[is.na(V21), V21 := `MÊS`]
hh_old[, c("ANO", "UF") := NULL] # already have more reliable `.year`


## Set keys & merge



pp_old[!grepl("\\d{6,8}", V102), V102 := NA_character_]
pp_old[, V102 := as.integer(V102)]
