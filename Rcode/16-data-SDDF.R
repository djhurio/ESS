# Sample design data files (SDDF) ####

# Options ####
options(encoding = "UTF-8")
options(max.print = 10e3)
options(datatable.integer64 = "character")
options(datatable.keepLeadingZeros = TRUE)


# Packages
require(data.table)
require(essurvey)


# Reset ####
rm(list = ls())
gc()


# ESS data ####
set_email("martins.liberts+ess@gmail.com")

import_sddf_country(country = "Albania", rounds = 6)
import_sddf_country(country = "Finland", rounds = 6)

class(try(1 / "1"))

# Import SDDF for the rounds 1:8
t1 <- Sys.time()

# Double loop through all countries and rounds
dat <- lapply(show_countries(), function(cntry) {
  lapply(show_sddf_cntrounds(cntry), function(rnd) {
    cat(cntry, "round", rnd, "\n")
    sddf <- try(import_sddf_country(country = cntry, rounds = rnd,
                                    format = "spss"))
    if (class(sddf) == "try-error") {
      print(sddf)
      return(NULL)
    }
    setDT(sddf)
    sddf[, essround := rnd]
    return(sddf)
  })
})

t2 <- Sys.time()
t2 - t1
# Time difference of 16.56868 mins
# Time difference of 17.84737 mins (2020-12-10)


# Convert to one level list
dat <- unlist(dat, recursive = F)

# Combine into one data.table
datSDDF1 <- rbindlist(dat, use.names = T, fill = T)

datSDDF1[, .N, keyby = .(essround)]
datSDDF1[essround == 6, .N, keyby = .(cntry)]


# Round 9
# Since the round 9 SDDF variables are included in the main survey data file
dat <- import_rounds(rounds = 9, format = "spss")
setDT(dat)

# Remove all extra attributes (from the SPSS data file)
str(dat)
dat <- haven::zap_formats(dat)
dat <- haven::zap_label(dat)
dat <- haven::zap_labels(dat)
dat <- haven::zap_missing(dat)
dat <- haven::zap_widths(dat)
str(dat)


# Keep only SDDF variables
varnames <- intersect(names(datSDDF1), names(dat))
datSDDF9 <- dat[, ..varnames]
rm(dat)


# # Domain variable for R9
# datSDDF9[, .N, keyby = .(domain)]
# datSDDF9[!is.na(domain), .N, keyby = .(cntry, domain)]


# Correction for HU
n1 <- datSDDF9[cntry == "HU", .N, keyby = .(stratum, psu)][, .N]
n2 <- datSDDF9[cntry == "HU", .N, keyby = .(psu)][, .N]

if (n1 != n2) {
  require(haven)
  datSDDF9HU2 <- read_stata(file = "data/HU/HU_psu_corrected_feb21.dta")
  setDT(datSDDF9HU2)

  datSDDF9HU1 <- datSDDF9[cntry == "HU"]
  datSDDF9HU1[, c("domain", "stratum", "psu") := NULL]

  datSDDF9HU <- merge(datSDDF9HU1, datSDDF9HU2, by = c("cntry", "idno"))

  datSDDF9 <- rbindlist(list(datSDDF9[cntry != "HU"], datSDDF9HU),
                        use.names = T)
}

n1 <- datSDDF9[cntry == "HU", .N, keyby = .(stratum, psu)][, .N]
n2 <- datSDDF9[cntry == "HU", .N, keyby = .(psu)][, .N]

if (n1 != n2) stop("Check HU round 9")

rm(datSDDF9HU, datSDDF9HU1, datSDDF9HU2)
rm(n1, n2)

# Combine SDDF for rounds 1:9

# sapply(datSDDF1, class)
# sapply(datSDDF9, class)
datSDDF <- rbindlist(list(datSDDF1, datSDDF9), use.names = T, fill = T)

rm(datSDDF1, datSDDF9)
gc()


# Process the SDDF
class(datSDDF)

datSDDF[, .N, keyby = cntry]
datSDDF[, .N, keyby = essround]

sapply(datSDDF, class)

# Domain
datSDDF[, domain := as.integer(domain)]
datSDDF[, .N, keyby = .(domain)]

# Domains are available only since round 7
dcast.data.table(data = datSDDF, formula = essround ~ domain,
                 fun.aggregate = length)

# I assume 1 domain if domain information is not available
datSDDF[is.na(domain), domain := 1L]

dcast.data.table(data = datSDDF, formula = essround ~ domain,
                 fun.aggregate = length)


# Keep only necessary SDDF variables
datSDDF <- datSDDF[, .(essround, cntry, idno, domain, stratum, psu, prob)]

datSDDF[, lapply(.SD, class)]


# Save for the next step
save(datSDDF, file = "data/datSDDF.Rdata")
