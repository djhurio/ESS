# ESS
# Information for the SDS from the R9

require(data.table)
require(openxlsx)
require(essurvey)
require(countrycode)


# Gross and net sample size and RR from the data protocol

tab1 <- read.xlsx(xlsxFile = "data/ESS9-data-rr.xlsx")
setDT(tab1)

tab1[, cntry := countrycode(sourcevar = country,
                            origin = "country.name", destination = "iso2c")]

tab1[, ri := 1 - n_net / n_gross / rr]


# Please change to your own e-mail address
set_email(ess_email = "martins.liberts@gmail.com")

# Round of interest
R <- 9

# Countries
show_rounds_country(rounds = R)

# Import data
dat <- import_rounds(rounds = R)
setDT(dat)

# Net sample size
dat[, .N, keyby = .(essround, cntry)]

dat[, weight1 := dweight * pweight * 10e3]

tab2 <- dat[, .(n_net_test = .N,
                deff_p = .N * sum(weight1 ^ 2) / sum(weight1) ^ 2),
            keyby = .(essround, cntry)]

tab <- merge(tab1, tab2, by = c("essround", "cntry"), all = T)

if (tab[, all.equal(n_net, n_net_test)]) tab[, n_net_test := NULL]

tab

write.xlsx(x = tab, file = "results/ESS9-sample-size-rr-ri-deff_p.xlsx")

names(dat)
