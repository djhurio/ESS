# Validation ####

# Options ####
options(encoding = "UTF-8")
options(stringsAsFactors = F)

# Packages
require(data.table)
require(openxlsx)
require(essurvey)

# Reset ####
rm(list = ls())
gc()


# Load data ####
load("data/datSDDF.Rdata")

show_rounds()
datSDDF[, essround := factor(essround, show_rounds(), show_rounds())]
dcast(datSDDF, cntry ~ essround)


# Sampling information ####
datSDDF

names(datSDDF)

datSDDF[, lapply(.SD, class)]

datSDDF[, .N, cntry][order(N)]
dcast(datSDDF, cntry ~ essround)

datSDDF[cntry == "NA"]
datSDDF[cntry == "NA", .N]
datSDDF[cntry == "NA", .N,
        keyby = .(essround, cntry, idno, file.name)]

datSDDF[cntry != "NA" & is.na(idno)]
datSDDF[cntry != "NA" & is.na(idno), .N]
datSDDF[cntry != "NA" & is.na(idno), .N,
        keyby = .(essround, cntry, idno, file.name)]

datSDDF[cntry == "NA" | is.na(idno), .N]
datSDDF[cntry == "NA" | is.na(idno), .N,
        keyby = .(essround, cntry, idno, file.name)]

# Delete empty records
datSDDF <- datSDDF[!is.na(idno)]

anyDuplicated(datSDDF, by = c("essround", "cntry", "domain", "idno"))

datSDDF[, n := .N, by = .(essround, cntry, domain, idno)]
datSDDF[n > 1][order(essround, cntry, domain, idno)]
datSDDF[, n := NULL]

# str(datSDDF)

datSDDF[, .N, keyby = domain]
datSDDF[, .N, keyby = .(essround, cntry, domain)]

datSDDF[, .N, keyby = .(essround, cntry)]
datSDDF[, .N, keyby = .(essround, cntry, stratum)]
datSDDF[, .N, keyby = .(essround, cntry, stratum, domain)]

datSDDF[is.na(stratum), .N, keyby = .(essround, cntry, stratum)]
datSDDF[grep("NA", stratum), .N,
        keyby = .(essround, cntry, stratum = trimws(stratum))]

# datSDDF[, .N, keyby = nchar(stratum)]


# Create strata variable from country, domain and stratum
datSDDF[, STR := paste(essround, cntry, domain, trimws(stratum), sep = "_")]
datSDDF[, .N, keyby = .(essround, cntry, domain, stratum)]
datSDDF[, .N, keyby = .(STR)]

# Create PSU variable from STR and psu
datSDDF[, min(psu, na.rm = T)]

datSDDF[, .N, keyby = is.na(psu)]

datSDDF[is.na(psu), .N, keyby = .(essround, cntry, domain)]

datSDDF[, netsamps := .N, by = .(essround, cntry, domain)]
datSDDF[, n_na_psu := sum(is.na(psu)), by = .(essround, cntry, domain)]

datSDDF[n_na_psu > 0, .(.N, sum(is.na(psu))),
        keyby = .(essround, cntry, domain)]

# psu := idno if all psu are NA
datSDDF[n_na_psu == netsamps,
        .(.N, sum(is.na(psu))), keyby = .(essround, cntry)]
datSDDF[n_na_psu == netsamps, psu := idno]

datSDDF[, n_na_psu := sum(is.na(psu)), by = .(essround, cntry, domain)]

datSDDF[n_na_psu > 0, .(.N, sum(is.na(psu))),
        keyby = .(essround, cntry, domain)]

m <- datSDDF[!is.na(psu), max(nchar(psu))]
m

datSDDF[, PSU := paste(STR, formatC(psu, digits = m - 1, flag = 0),
                       sep = "_")]

datSDDF[, .N, keyby = .(essround, cntry, domain, stratum, psu)]
datSDDF[, .N, keyby = .(PSU)]
datSDDF[, .N, keyby = .(STR, PSU)]

datSDDF[is.na(psu), .(stratum, psu, STR, PSU)]

tab_cntry <- datSDDF[, .(n_strat = sum(!duplicated(STR)),
                         n_psu = sum(!duplicated(PSU)),
                         n_resp = .N), keyby = .(essround, cntry, domain)]
tab_cntry

tab_strata <- datSDDF[, .(n_psu = sum(!duplicated(PSU)),
                          n_resp = .N),
                      keyby = .(essround, cntry, domain, STR)]
tab_strata

tab_psu <- datSDDF[, .(n_resp = .N),
                   keyby = .(essround, cntry, domain, STR, PSU)]
tab_psu

tabl <- list(tab_cntry, tab_strata, tab_psu)
names(tabl) <- c("cntry", "strata", "psu")

write.xlsx(tabl, file = "results/SDDF-tables-2.xlsx",
           colWidths = "auto", firstRow = T,
           headerStyle = createStyle(textDecoration = "italic",
                                     halign = "center"))


datSDDF2 <- datSDDF[, .(essround, cntry, idno, domain, STR, PSU, prob)]


# Survey data ####

load("data/dat.Rdata")

show_rounds()
dat[, essround := factor(essround, show_rounds(), show_rounds())]
dcast(dat, cntry ~ essround)



# Merge ####

intersect(names(dat), names(datSDDF2))

dat[, dat_surv := T]
datSDDF2[, dat_sddf := T]

setkey(dat, essround, cntry, idno)
setkey(datSDDF2, essround, cntry, idno)

anyDuplicated(dat, by = c("essround", "cntry", "idno"))
anyDuplicated(datSDDF2, by = c("essround", "cntry", "idno"))

dat[, .N]
datSDDF2[, .N]

# Test merge ####
tmp2 <- merge(dat, datSDDF2,
              by = c("essround", "cntry", "idno"), all = T)

tmp2[, .N, keyby = .(dat_surv, dat_sddf)]
tmp2[is.na(dat_surv), .N, keyby = .(essround, cntry, dat_surv, dat_sddf)]
tmp2[is.na(dat_sddf), .N, keyby = .(essround, cntry, dat_surv, dat_sddf)]


# Merge ####

dat2 <- merge(dat, datSDDF2,
              by = c("essround", "cntry", "idno"), all.x = T)

nrow(dat)
nrow(dat2)

dat2[, .N, keyby = .(dat_surv, dat_sddf)]
dat2[is.na(dat_surv), .N, keyby = .(essround, cntry, dat_surv, dat_sddf)]
dat2[is.na(dat_sddf), .N, keyby = .(essround, cntry, dat_surv, dat_sddf)]

dat2[is.na(dat_sddf) & essround == 8]

dat2[, n_na := sum(is.na(dat_sddf)), by = .(essround, cntry)]

# Select only cases with full SDDF information
dat2 <- dat2[n_na == 0]
dat2[, n_na := NULL]


dat2[, .N, keyby = .(domain)]
dat2[, domain := factor(domain)]

# Net sample size

nrow(dat)
nrow(dat2)

dat2[, .N, keyby = .(cntry, essround)]
dat2[, .N, keyby = .(cntry, essround, domain)]

dat2[cntry == "LT", .N, keyby = .(cntry, essround)]
dat2[cntry == "LT", .N, keyby = .(cntry, essround, domain)]


# Weights
dat2[(dat_sddf), .(essround, cntry, dweight, pspwght, pweight, prob)]

dat2[(dat_sddf), .N, keyby = .(is.na(prob), is.na(dweight))]

dat2[, .N, keyby = .(dweight = !is.na(dweight),
                     pspwght = !is.na(pspwght),
                     pweight = !is.na(pweight))]

dat2[is.na(dweight), .(essround, cntry, dweight, pspwght, pweight)]


# Design weights computed from sampling probabilities
dat2[, summary(prob)]
dat2[, min(prob)]

dat2[(dat_sddf) & prob > 0, dw := 1 / prob]
dat2[prob == 0, dw := 1]

tmp <- dat2[(dat_sddf),
            .(essround, cntry, dweight, pspwght, pweight, prob, dw)]

tmp[, c(.(n = .N), lapply(.SD, sum)),
    .SDcols = c("dweight", "pspwght", "pweight"),
    keyby = .(essround, cntry)]

tmp[, dweight2 := .N * dw / sum(dw), by = .(essround, cntry)]

tmp[is.na(dweight2)]

tmp[, all.equal(dweight, dweight2, check.attributes = F)]

tmp[is.na(dweight)]
tmp[is.na(dweight2)]
tmp[prob == 0]

tmp[abs(dweight - dweight2) > .1,
    .(essround, cntry, prob, dw, dweight, dweight2)]
tmp[abs(dweight - dweight2) > .1, .N, keyby = .(essround, cntry)]

# dat2[, weight0 := dw]
dat2[, weight1 := dweight * pweight * 10e3]
# dat2[, weight2 := dweight * pspwght * pweight * 10e3]


# Save ####

save(dat2, file = "data/dat2.Rdata")
