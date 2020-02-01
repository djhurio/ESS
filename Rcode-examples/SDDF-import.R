# ESS
# Example how to import the SDDF information

require(data.table)
require(essurvey)

# Please change to your own e-mail address
set_email(ess_email = "martins.liberts@gmail.com")

# Rounds of interest
R <- 6:8

# There are 19 countries who participated in all rounds from 6 to 8
show_rounds_country(rounds = R)

# Function to test if the SDDF is available for those countries for rounds 6-8
test_sddf <- function(x) all(R %in% show_sddf_cntrounds(country = x))

# Test rezults for all R6-8 countries
# It takes some time to compute
test <- sapply(show_rounds_country(rounds = R), test_sddf)

# R6-8 countries with SDDF available for all rounds (18)
names(test[test])
cat(paste(names(test[test]), collapse = ", "))

# R6-8 countries with SDDF not available (1)
names(test[!test])

show_sddf_cntrounds(country = names(test[!test])[1])

# SDDF import
dat <- lapply(X = names(test[test]), FUN = import_sddf_country, rounds = R)

length(unlist(dat, recursive = F)) == 18 * 3

dat <- rbindlist(unlist(dat, recursive = F), use.names = T, fill = T)

dat[, .N, keyby = .(name)]
dat[, .N, keyby = .(essround)]

dat[is.na(essround), essround := 6L]

dcast.data.table(data = dat, formula = cntry ~ essround,
                 fun.aggregate = length, value.var = "idno")
