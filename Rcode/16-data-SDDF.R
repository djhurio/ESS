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


# Import SDDF for the rounds 1:8
t1 <- Sys.time()

# Double loop through all countries and rounds
dat <- lapply(show_countries(), function(cntry) {
  lapply(show_sddf_cntrounds(cntry), function(rnd) {
    cat(cntry, "round", rnd, "\n")
    sddf <- import_sddf_country(country = cntry, rounds = rnd, format = "spss")
    setDT(sddf)
    sddf[, essround := rnd]
    sddf
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
