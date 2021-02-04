require(data.table)
require(essurvey)
require(haven)


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


# Data correction

dat2 <- read_stata(file = "data/HU/HU_psu_corrected_feb21.dta")
setDT(dat2)

dat2[, domain := as.integer(domain)]

dat[, .(cntry, idno, stratum, psu, domain)]
dat2

tmp <- merge(dat[, .(cntry, idno, stratum, psu, domain)], dat2,
             by = c("cntry", "idno"))

tmp[, all.equal(domain.x, domain.y)]

tmp[, all.equal(stratum.x, stratum.y)]
tmp[stratum.x != stratum.y]

tmp[, all.equal(psu.x, psu.y)]
tmp[psu.x != psu.y]

dat2[, .N, keyby = .(stratum)]
dat2[, .N, keyby = .(psu)]
dat2[, .N, keyby = .(psu, stratum)]

dat2[, .N, keyby = .(domain)]
dat2[, .N, keyby = .(domain, stratum)][, .N, keyby = .(domain)]
dat2[, .N, keyby = .(domain, psu)][, .N, keyby = .(domain)]
dat2[, .N, keyby = .(domain, stratum, psu)][, .N, keyby = .(domain)]

