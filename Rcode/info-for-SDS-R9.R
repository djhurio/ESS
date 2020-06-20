# ESS
# Information for the SDS from the R9

require(data.table)
require(openxlsx)
require(essurvey)
require(countrycode)
require(haven)
require(ggplot2)


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

dat[, class(dweight)]
dat[, class(pweight)]

dat[, weight1 := dweight * pweight * 10e3]


# BG contact files ESS9
dat.cf <- read_spss("~/Dropbox/Darbs/ESS-SWEP/ESS09/Data/ESS9cf_BG.sav")
setDT(dat.cf)

names(dat.cf)

dat.cf[, .N, keyby = .(cntry, typesamp)]
dat[, .N, keyby = .(essround, cntry)]

dat.cf[cntry == "BG"]


dat.cf[cntry == "BG", summary(idno)]
dat[cntry == "BG", summary(idno)]

dat.cf[, class(nhhmem)]
dat.cf[, .N, keyby = .(nhhmem)]

dat.cf[, nhhmem := as.integer(nhhmem)]


tmp <- merge(x = dat[cntry == "BG", .(essround, cntry, idno, region, domicil,
                                      dweight, weight1)],
             y = dat.cf[cntry == "BG", .(cntry, idno, nhhmem)],
             by = c("cntry", "idno"), all.x = T)

tmp[, .N, keyby = .(nhhmem)]

tmp[, .N, keyby = .(region)]
tmp[, .N, keyby = .(domicil)]

tmp[, str1 := grepl("A big city", domicil)]

tmp[, .N, keyby = .(domicil, str1)]

dcast.data.table(data = tmp, region ~ str1)


tmp[, .N, keyby = .(region, str1)]


tmp[order(-dweight)][1:10]

tmp[, psu := dweight / nhhmem]
tmp[order(psu)]

tmp[, .N, by = .(region, psu)]

tmp[order(region, domicil, psu)]

setorder(tmp, region, -str1, psu)

write.xlsx(x = tmp, file = "~/Downloads/tmp.xlsx")
fwrite(x = tmp, file = "~/Downloads/tmp.csv")

ggplot(data = tmp, mapping = aes(x = dweight, y = nhhmem)) +
  geom_point() +
  facet_wrap(~ region)

tmp[, cor(dweight, nhhmem)]
tmp[, cor(dweight, nhhmem), keyby = .(region)]


tmp[region == "BG411" & psu == 0.4081832][order(dweight)]



tab2 <- dat[, .(n_net_test = .N,
                deff_p = .N * sum(weight1 ^ 2) / sum(weight1) ^ 2),
            keyby = .(essround, cntry)]

tab <- merge(tab1, tab2, by = c("essround", "cntry"), all = T)

if (tab[, all.equal(n_net, n_net_test)]) tab[, n_net_test := NULL]

tab

write.xlsx(x = tab, file = "results/ESS9-sample-size-rr-ri-deff_p.xlsx")

names(dat)




# SDDF LT

dat.sddf.lt <- read_spss(file = "/home/djhurio/Dropbox/Darbs/ESS-SWEP/ESS09/Sampling/My Countries/LT - Lithuania/SDDF/ESS9sddf_Lithuania_200112_v1.sav")

setDT(dat.sddf.lt)

dat.sddf.lt

dat.sddf.lt[, .N]
dat.sddf.lt[, .N, keyby = .(domain)]
