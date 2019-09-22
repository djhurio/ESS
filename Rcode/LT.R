require(data.table)
require(essurvey)


# load("data/dat.Rdata")
# names(dat)
# grep("NHHMEM", names(dat), ignore.case = T)
# rm(dat)

set_email("martins.liberts@gmail.com")
show_countries()
dat <- import_country(country = "Lithuania", rounds = 8)
setDT(dat)

grep("NHHMEM", names(dat), ignore.case = T)


load("data/dat2.Rdata")

tab <- dat2[cntry == "LT", .(N1 = sum(dweight * pweight * 10e3),
                             N2 = sum(pspwght * pweight * 10e3)),
            keyby = .(essround, domain)]

tab[, P1 := round(prop.table(N1), 2), by = .(essround)]
tab[, P2 := round(prop.table(N2), 2), by = .(essround)]

tab

tab <- dat2[cntry == "LT", .(deff_p = .N * sum(weight1 ^ 2) / sum(weight1) ^ 2),
            keyby = .(essround, cntry, domain)]

# Kopēt bez rindu nosaukumiem
write.table(tab, file = "clipboard", sep = "\t", row.names = F,
            fileEncoding = "native.enc")

tab <- dat2[cntry == "LT", .(deff_p = .N * sum(weight1 ^ 2) / sum(weight1) ^ 2),
            keyby = .(essround, cntry)]

# Kopēt bez rindu nosaukumiem
write.table(tab, file = "clipboard", sep = "\t", row.names = F,
            fileEncoding = "native.enc")
