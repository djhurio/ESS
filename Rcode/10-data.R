# ESS data ####

# Options ####
options(encoding = "UTF-8")
options(stringsAsFactors = F)


# Packages
require(data.table)
require(haven)

# devtools::install_github("cimentadaj/ess")
require(ess)


# Reset ####
rm(list = ls())
gc()



# Load ESS data ####

# ess package
dat <- ess_rounds(rounds = 6:7, your_email = "martins.liberts@gmail.com",
                  format = "spss")
dat <- rbindlist(dat, fill = T)

class(dat)

# Test
str(dat)

str(dat$prtvtcfi)
attr(dat$prtvtcfi, "label")
attr(dat$prtvtcfi, "labels")

# str(dat$prtvtbis)
# attr(dat$prtvtbis, "label")
# attr(dat$prtvtbis, "labels")

# dat[, .(proddate, as.Date(proddate, format = "%d.%m.%Y"))]
dat[, proddate := as.Date(proddate, format = "%d.%m.%Y")]
dat[, .N, keyby = proddate]

dat[, .N, keyby = cntry]
dat[, cntry := as.character(cntry)]


# SDDF data ####

"http://www.europeansocialsurvey.org/file/download?f=ESS7SDDFe01_1.spss.zip&c=&y=2014"

root <- "http://www.europeansocialsurvey.org/file/download"

get.data <- function(root = "", f = "", c = "", y = "", dir.data) {
  # Create and print download link
  file.url <- paste0(root, "?f=", f, "&c=", c, "&y=", y)
  cat(file.url, "\n\n")

  # Download
  current_file <- httr::GET(file.url, httr::progress())

  # Write as a .zip file
  writeBin(httr::content(current_file, "raw"), file.path(dir.data, f))

  # Unzip
  utils::unzip(file.path(dir.data, f), exdir = dir.data)
}


# SDDF - ESS7 (2014) ####
# There is integrated SDDF file :)

dir.data <- "data/ESS7"

get.data(root = root, f = "ESS7SDDFe01_1.spss.zip", y = 2014,
         dir.data = dir.data)

# Import SPSS data file
datSDDF7 <- read_sav(list.files(dir.data, "sav$", full.names = T))
setDT(datSDDF7)
class(datSDDF7)


datSDDF7[, .N, keyby = cntry]
datSDDF7[, cntry := as.character(cntry)]

datSDDF7[, .N, keyby = stratify]
datSDDF7[stratify == "NA", stratify := "0"]
datSDDF7[, stratify := as.integer(stratify)]
datSDDF7[, .N, keyby = stratify]


datSDDF7 <- datSDDF7[, .(essround, cntry, idno, domain, stratify, psu, prob)]
datSDDF7





# SDDF - ESS6 (2012) ####
# There is a seperate SDDF file for each country :(

dir.data <- "data/ESS6"

# SDDF is missing for Israel (IL)
list.cntry <- dat[essround == 6 & cntry != "IL", unique(cntry)]
length(list.cntry)

list.f <- paste("ESS6", list.cntry, "SDDF.stata.zip", sep = "_")
length(list.f)

mapply(get.data, list.f, list.cntry,
       MoreArgs = list(root = root, y = 2012, dir.data = dir.data),
       SIMPLIFY = F, USE.NAMES = F)

table(substr(list.files(dir.data, pattern = "zip$|dta$"), 1, 7))


# Import SPSS data file

list.files(dir.data, "dta$", full.names = T)

datSDDF6 <- lapply(list.files(dir.data, "dta$", full.names = T), read_dta)

lapply(datSDDF6, setDT)

lapply(datSDDF6, `[`, j = .N, keyby = cntry)
# There are missing records in the Denmark file
# cntry    N
# 1:    DK 1650
# 2:    NA  455

datSDDF6 <- rbindlist(datSDDF6, fill = T)

class(datSDDF6)

datSDDF6[, .N, keyby = cntry]

datSDDF6[, .N, keyby = essround]
datSDDF6[, essround := 6L]

sapply(datSDDF6, class)

datSDDF6[, domain := 1L]

datSDDF6 <- datSDDF6[cntry %in% list.cntry,
                     .(essround, cntry, idno, domain, stratify, psu, prob)]


# Renumerate strata
datSDDF6[, .N, keyby = .(cntry, domain, stratify)]
# 545 strata alltogether

datSDDF6[, stratify := as.integer(factor(paste0(cntry, domain, stratify)))]

datSDDF6[, .N, keyby = .(cntry, domain, stratify)]
datSDDF6[, .N, keyby = .(stratify)]


# Renumerate psu
datSDDF6[, .N, keyby = .(cntry, domain, stratify, psu)]
# 21541 psu alltogether

datSDDF6[, psu := as.integer(factor(paste0(cntry, domain, stratify, psu)))]

datSDDF6[, .N, keyby = .(cntry, domain, stratify, psu)]
datSDDF6[, .N, keyby = .(psu)]



# # SDDF - ESS5 (2010) ####
# # There is a seperate SDDF file for each country :(
#
# dir.data <- "data/ESS5"
#
# # SDDF is missing for Israel and Portugal
# list.cntry <- dat[essround == 5 & !(cntry %in% c("IL", "PT")), unique(cntry)]
# length(list.cntry)
#
# list.f <- paste("ESS5", list.cntry, "SDDF.sas.zip", sep = "_")
# length(list.f)
#
# mapply(get.data, list.f, list.cntry,
#        MoreArgs = list(root = root, y = 2010, dir.data = dir.data),
#        SIMPLIFY = F, USE.NAMES = F)
#
# table(substr(list.files(dir.data, pattern = "zip$|sas7bdat$"), 1, 7))
#
#
# # Import SAS data file
#
# list.files(dir.data, "sas7bdat$", full.names = T)
#
# datSDDF5 <- lapply(list.files(dir.data, "sas7bdat$", full.names = T), read_sas)
#
# lapply(datSDDF5, setDT)
#
# lapply(datSDDF5, `[`, j = .N, keyby = CNTRY)
#
# datSDDF5 <- rbindlist(datSDDF5, fill = T)
#
# class(datSDDF5)
#
# setnames(datSDDF5, tolower(names(datSDDF5)))
#
# datSDDF5[, .N, keyby = cntry]
#
# datSDDF5[, essround := 5L]
#
# sapply(datSDDF5, class)
#
# datSDDF5[, domain := 1L]
#
# datSDDF5 <- datSDDF5[cntry %in% list.cntry,
#                      .(essround, cntry, idno, domain, stratify, psu, prob)]
#
#
# # Renumerate strata
# datSDDF5[, .N, keyby = .(cntry, domain, stratify)]
# # 8704 strata alltogether
#
# datSDDF5[, stratify := as.integer(factor(paste0(cntry, domain, stratify)))]
#
# datSDDF5[, .N, keyby = .(cntry, domain, stratify)]
# datSDDF5[, .N, keyby = .(stratify)]
#
#
# # Renumerate psu
# datSDDF5[, .N, keyby = .(cntry, domain, stratify, psu)]
# # 13124 psu alltogether
#
# datSDDF5[, psu := as.integer(factor(paste0(cntry, domain, stratify, psu)))]
#
# datSDDF5[, .N, keyby = .(cntry, domain, stratify, psu)]
# datSDDF5[, .N, keyby = .(psu)]






# Combine SDDF files ####

# sapply(datSDDF5, class)
sapply(datSDDF6, class)
sapply(datSDDF7, class)

datSDDF <- rbindlist(list(datSDDF6, datSDDF7))

str(datSDDF)

datSDDF[, lapply(.SD, class)]



# Save ####

save(dat, file = "data/dat.Rdata")
save(datSDDF, file = "data/datSDDF.Rdata")
