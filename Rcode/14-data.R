# ESS data ####

# Options ####
options(encoding = "UTF-8")
options(stringsAsFactors = F)


# Packages
require(data.table)
require(lubridate)
require(essurvey)


# Reset ####
rm(list = ls())
gc()


# Load ESS data ####
set_email("martins.liberts@gmail.com")


# ESS data ####

show_rounds()

dat <- import_all_rounds(format = "spss")
length(dat)

dat <- rbindlist(dat, fill = T)
class(dat)


# Test
dat[, .N]

dat[, .N, keyby = .(cntry)]

str(dat)

str(dat$prtvtcfi)
attr(dat$prtvtcfi, "label")
attr(dat$prtvtcfi, "labels")

str(dat$prtvtbis)
attr(dat$prtvtbis, "label")
attr(dat$prtvtbis, "labels")

# dat[, .(proddate, dmy(proddate))]
dat[, proddate := dmy(proddate)]
dat[, .N, keyby = proddate]

dat[, .N, keyby = cntry]
dat[, cntry := as.character(cntry)]

dat[, .N, keyby = .(cntry, essround)]


# Save ####

save(dat, file = "data/dat.Rdata")
