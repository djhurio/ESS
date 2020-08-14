# ESS SDDF data ####

# Options ####
options(encoding = "UTF-8")
options(max.print = 10e3)
options(datatable.integer64 = "character")
options(datatable.keepLeadingZeros = TRUE)


# Packages
require(data.table)
require(lubridate)
require(stringr)
require(essurvey)
require(openxlsx)
require(parallel)

detectCores()


# Reset ####
rm(list = ls())
gc()



# ESS data ####
set_email("martins.liberts+ess@gmail.com")


# Rounds 1:8
t1 <- Sys.time()
dat <- mclapply(show_countries(), function(cntry) {
  lapply(show_sddf_cntrounds(cntry), function(rnd) {
    cat(cntry, "round", rnd, "\n")
    sddf <- import_sddf_country(country = cntry, rounds = rnd)
    setDT(sddf)
    sddf[, essround := rnd]
    sddf
  })
}, mc.silent = F, mc.cores = 4)
t2 <- Sys.time()
t2 - t1
# Time difference of 5.098674 mins

dat <- unlist(dat, recursive = F)

datSDDF1 <- rbindlist(dat, use.names = T, fill = T)


# Round 9
dat <- import_rounds(rounds = 9)
setDT(dat)

varnames <- intersect(names(datSDDF1), names(dat))
datSDDF9 <- dat[, ..varnames]
rm(dat)

# Domain variable for R9
dat.domain.r9 <- haven::read_stata("data/R9/ess9_domain.dta")
setDT(dat.domain.r9)

datSDDF9[, .(min_idno = min(idno), max_idno = max(idno)), keyby = .(cntry)]
dat.domain.r9[, .(min_idno = min(idno), max_idno = max(idno)), keyby = .(cntry)]

datSDDF9 <- merge(datSDDF9, dat.domain.r9,
                  by = intersect(names(datSDDF9), names(dat.domain.r9)))



# Combine SDDF for rounds 1:9
datSDDF <- rbindlist(list(datSDDF1, datSDDF9), use.names = T, fill = T)

rm(datSDDF1, datSDDF9)
gc()


# Process the SDDF

class(datSDDF)

datSDDF[, .N, keyby = cntry]

datSDDF[, .N, keyby = essround]

sapply(datSDDF, class)

datSDDF[, .N, keyby = .(domain)]

datSDDF[, domain := as.integer(domain)]
datSDDF[, .N, keyby = .(domain)]

datSDDF[is.na(domain), domain := 1L]
datSDDF[, .N, keyby = .(domain)]


dcast.data.table(data = datSDDF, formula = essround ~ domain,
                 fun.aggregate = length)


datSDDF <- datSDDF[, .(essround, cntry, idno, domain, stratum, psu, prob)]

datSDDF[, lapply(.SD, class)]


# Save ####

save(datSDDF, file = "data/datSDDF.Rdata")
