# CF
# ESS8 - data from Contact forms, edition 3.0
# https://www.europeansocialsurvey.org/download.html?file=ESS8CFe03&y=2016

# Options ####
options(encoding = "UTF-8")
options(stringsAsFactors = F)
options(max.print = 10e3)
options(datatable.integer64 = "character")
options(datatable.keepLeadingZeros = TRUE)


# Packages
require(data.table)
require(haven)
require(essurvey)


# Reset ####
rm(list = ls())
gc()


# Load SDDF
dat.sddf <- read_spss("data/SDDF/ESS8SDDFe01_1.sav")
setDT(dat.sddf)
dat.sddf <- dat.sddf[cntry == "LT"]
dat.sddf
setkey(dat.sddf, essround, cntry, idno)

# Load CF
dat.cf <- read_spss("data/CF/ESS8CFe03.sav")
setDT(dat.cf)
dat.cf <- dat.cf[cntry == "LT"]
dat.cf[, .(essround, cntry, idno, numhh, nhhmem)]
setkey(dat.cf, essround, cntry, idno)

# Load survey data
set_email("martins.liberts@gmail.com")
dat.survey <- import_country(country = "Lithuania", rounds = 8)
setDT(dat.survey)
dat.survey[, .(essround, cntry, idno, dweight, pweight)]
setkey(dat.survey, essround, cntry, idno)


# Merge
dat <- Reduce(merge,
              list(dat.survey[, .(essround, cntry, idno, dweight, pweight)],
                   dat.sddf,
                   dat.cf[, .(essround, cntry, idno, numhh, nhhmem)]))

dat

dat[, .N, keyby = .(numhh)]
dat[, .N, keyby = .(nhhmem)]
