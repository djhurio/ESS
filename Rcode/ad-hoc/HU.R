require(data.table)
require(essurvey)


# load("data/dat.Rdata")
# names(dat)
# grep("NHHMEM", names(dat), ignore.case = T)
# rm(dat)

set_email("martins.liberts@gmail.com")
show_countries()

dat <- import_country(country = "Hungary", rounds = 9)
setDT(dat)

dat[, .N, keyby = .(domain)]

dat[, .N, keyby = .(domain, stratum)][, .N, keyby = .(domain)]
dat[, .N, keyby = .(domain, psu)][, .N, keyby = .(domain)]
dat[, .N, keyby = .(domain, stratum, psu)][, .N, keyby = .(domain)]

dcast.data.table(data = dat[domain == 2], formula = psu ~ stratum,
                 fun.aggregate = length)

dat[, .N, keyby = .(domain, psu)][, mean(N), keyby = .(domain)]
dat[, .N, keyby = .(domain, stratum, psu)][, mean(N), keyby = .(domain)]

dat[domain == 2, .N, keyby = .(domain, psu, stratum)][psu == min(psu)]
