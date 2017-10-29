# ESS7 ####

require(data.table)
require(haven)


# Reset ####

rm(list = ls())
gc()



# Load data ####

# # Stata
# dat <- read_dta("data/ESS7/ESS7e02_1.stata/ESS7e02_1.dta", encoding = "UTF-8")
# datSDDF <- read_dta("data/ESS7/ESS7SDDFe01_1.stata/ESS7SDDFe1_1.dta",
#                     encoding = "UTF-8")

# SPSS
dat <- read_sav("data/ESS7/ESS7e02_1.spss/ESS7e02_1.sav")
datSDDF <- read_sav("data/ESS7/ESS7SDDFe01_1.spss/ESS7SDDFe1_1.sav")

setDT(dat)
setDT(datSDDF)

class(dat)
class(datSDDF)

str(dat)
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


dat[, lapply(.SD, class)]

dat[, .(proddate, as.Date(proddate, format = "%d.%m.%Y"))]
dat[, proddate := as.Date(proddate, format = "%d.%m.%Y")]

dat[, .N, keyby = cntry]
dat[, cntry := as.character(cntry)]



# Save ####

save(dat, file = "data/ESS7/dat.Rdata")
save(datSDDF, file = "data/ESS7/datSDDF.Rdata")
