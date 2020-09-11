# ESS SDDF data ####

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
    sddf <- import_sddf_country(country = cntry, rounds = rnd)
    setDT(sddf)
    sddf[, essround := rnd]
    sddf
  })
})

t2 <- Sys.time()
t2 - t1
# Time difference of 16.56868 mins


# Convert to one level list
dat <- unlist(dat, recursive = F)

# Combine into one data file
datSDDF1 <- rbindlist(dat, use.names = T, fill = T)


# Round 9
# Since round 9 SDDF variables are included in the main survey data file
dat <- import_rounds(rounds = 9)
setDT(dat)

# Keep only SDDF variables
varnames <- intersect(names(datSDDF1), names(dat))
datSDDF9 <- dat[, ..varnames]
rm(dat)

# Domain variable for R9
# By mistake domain variable for the R9 has not been published into the public file (edition 2)
# Please contact the ESS SWEP for the domain variable
dat.domain.r9 <- haven::read_stata("data/R9/ess9_domain_all.dta")
setDT(dat.domain.r9)

# # Check the IDNO
# datSDDF9[, .(min_idno = min(idno), max_idno = max(idno)), keyby = .(cntry)]
# dat.domain.r9[, .(min_idno = min(idno_scrambled),
#                   max_idno = max(idno_scrambled)), keyby = .(cntry)]


# Merge the domain variable
datSDDF9 <- merge(x = datSDDF9,
                  y = dat.domain.r9[, .(cntry, idno_scrambled, domain)],
                  by.x = c("cntry", "idno"),
                  by.y = c("cntry", "idno_scrambled"),
                  all.x = T, sort = F)


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
