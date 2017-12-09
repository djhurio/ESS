# ESS data ####

# Options ####
options(encoding = "UTF-8")
options(stringsAsFactors = F)


# Packages
require(data.table)
require(haven)
require(ess)


# Reset ####
rm(list = ls())
gc()



# Load ESS data ####

# Stata
# dat <- read_dta("data/ESS7/ESS7e02_1.stata/ESS7e02_1.dta", encoding = "UTF-8")

# ess package
# dat <- ess_rounds(rounds = 7:8, your_email = "martins.liberts@gmail.com")
# dat <- rbindlist(dat, fill = T)

# SPSS
dat7 <- read_sav("data/ESS7/ESS7e02_1.spss/ESS7e02_1.sav")
dat8 <- read_sav("data/ESS8/ESS8e01.sav")
dat <- rbindlist(list(dat7, dat8), fill = T)

dat

class(dat)

str(dat)

str(dat$prtvtcfi)
attr(dat$prtvtcfi, "label")
attr(dat$prtvtcfi, "labels")

str(dat$prtvtbis)
attr(dat$prtvtbis, "label")
attr(dat$prtvtbis, "labels")

dat[, lapply(.SD, class)]

dat[, .(proddate, as.Date(proddate, format = "%d.%m.%Y"))]
dat[, proddate := as.Date(proddate, format = "%d.%m.%Y")]
dat[, .N, keyby = proddate]

dat[, .N, keyby = cntry]
dat[, cntry := as.character(cntry)]


# Load SDDF data ####

# Stata
# datSDDF <- read_dta("data/ESS7/ESS7SDDFe01_1.stata/ESS7SDDFe1_1.dta",
#                     encoding = "UTF-8")

# SPSS
datSDDF <- read_sav("data/ESS7/ESS7SDDFe01_1.spss/ESS7SDDFe1_1.sav")

setDT(datSDDF)
class(datSDDF)

str(datSDDF)

datSDDF[, lapply(.SD, class)]

datSDDF[, .(proddate, as.Date(proddate, format = "%d.%m.%Y"))]
datSDDF[, proddate := as.Date(proddate, format = "%d.%m.%Y")]

datSDDF[, .N, keyby = cntry]
datSDDF[, cntry := as.character(cntry)]

datSDDF[, .N, keyby = stratify]
datSDDF[stratify == "NA", stratify := "0"]
datSDDF[, stratify := as.integer(stratify)]
datSDDF[, .N, keyby = stratify]




# Save ####

save(dat, file = "data/dat.Rdata")
save(datSDDF, file = "data/datSDDF.Rdata")
