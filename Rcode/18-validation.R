# Validation ####

# Options ####
options(encoding = "UTF-8")
options(max.print = 10e3)
options(datatable.integer64 = "character")
options(datatable.keepLeadingZeros = TRUE)


# Packages
require(data.table)
require(openxlsx)
require(essurvey)
require(ggplot2)


# Reset ####
rm(list = ls())
gc()


# Load data ####
load("data/datSDDF.Rdata")

show_rounds()
datSDDF[, essround := factor(essround, show_rounds(), show_rounds())]

dcast(datSDDF, cntry ~ paste0("R", essround), fun.aggregate = length)


# Sampling information ####
datSDDF

names(datSDDF)

datSDDF[, lapply(.SD, class)]

datSDDF[, .N, cntry][order(N)]

datSDDF[cntry == "NA"]
datSDDF[cntry == "NA", .N]
datSDDF[cntry == "NA", .N,
        keyby = .(essround, cntry, idno)]

datSDDF[cntry != "NA" & is.na(idno)]
datSDDF[cntry != "NA" & is.na(idno), .N]
datSDDF[cntry != "NA" & is.na(idno), .N,
        keyby = .(essround, cntry, idno)]

datSDDF[cntry == "NA" | is.na(idno), .N]
datSDDF[cntry == "NA" | is.na(idno), .N,
        keyby = .(essround, cntry, idno)]


# Delete empty records
datSDDF <- datSDDF[!is.na(idno)]

# domain
dcast.data.table(datSDDF, essround ~ domain, fun.aggregate = length)

anyDuplicated(datSDDF, by = c("essround", "cntry", "domain", "idno"))

datSDDF[, n := .N, by = .(essround, cntry, domain, idno)]
datSDDF[n > 1][order(essround, cntry, domain, idno)]
datSDDF[, n := NULL]


# str(datSDDF)
# sapply(datSDDF, class)

datSDDF[, .N, keyby = domain]
datSDDF[, .N, keyby = .(essround, cntry, domain)]

datSDDF[is.na(stratum), .N, keyby = .(essround, cntry, stratum)]

m <- datSDDF[, min(stratum, na.rm = T)]
m

# Recode missing stratum values
datSDDF[is.na(stratum), stratum := m - 1L]

# Create strata variable from country, domain and stratum
m <- datSDDF[, nchar(max(stratum))]
m

datSDDF[, STR := paste(paste0("R", essround), cntry, paste0("D", domain),
                       stringr::str_pad(string = stratum, width = m, pad = "0"),
                       sep = "_")]
datSDDF[, .N, keyby = .(essround, cntry, domain, stratum, STR)]
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

datSDDF[n_na_psu > 0, .(.N, PSU_NA = sum(is.na(psu))),
        keyby = .(essround, cntry, domain)]

m <- datSDDF[, min(psu, na.rm = T)]
m

# Recode missing PSU values
datSDDF[is.na(psu), psu := m - 1L]

m <- datSDDF[, nchar(max(psu))]
m

datSDDF[, PSU := paste(STR,
                       stringr::str_pad(string = psu, width = m, pad = "0"),
                       sep = "_")]

datSDDF[, .N, keyby = .(essround, cntry, domain, stratum, psu, PSU)]
datSDDF[, .N, keyby = .(essround, cntry, domain, stratum, psu)]
datSDDF[, .N, keyby = .(PSU)]
datSDDF[, .N, keyby = .(STR, PSU)]

datSDDF[is.na(psu), .(stratum, psu, STR, PSU)]

tab_cntry <- datSDDF[, .(n_strat = sum(!duplicated(STR)),
                         n_psu   = sum(!duplicated(PSU)),
                         n_resp  = .N), keyby = .(essround, cntry, domain)]
tab_cntry

tab_strata <- datSDDF[, .(n_psu  = sum(!duplicated(PSU)),
                          n_resp = .N),
                      keyby = .(essround, cntry, domain, STR)]
tab_strata

tab_psu <- datSDDF[, .(n_resp = .N),
                   keyby = .(essround, cntry, domain, STR, PSU)]
tab_psu

tabl <- list(tab_cntry, tab_strata, tab_psu)
names(tabl) <- c("cntry", "strata", "psu")

write.xlsx(tabl, file = "results/SDDF-tables.xlsx",
           colWidths = "auto", firstRow = T,
           headerStyle = createStyle(textDecoration = "italic",
                                     halign = "center"))


datSDDF2 <- datSDDF[, .(essround, cntry, domain, STR, PSU, idno, prob)]


# Survey data ####
load("data/dat.Rdata")

show_rounds()
dat[, essround := factor(essround, show_rounds(), show_rounds())]

tab <- dcast(dat, cntry ~ paste0("R", essround), fun.aggregate = length)
tab
fwrite(tab, file = "tables/ESS_country_rounds.csv")




# Merge ####

intersect(names(dat), names(datSDDF2))

dat[, dat_surv := T]
datSDDF2[, dat_sddf := T]

setkey(dat, essround, cntry, idno)
setkey(datSDDF2, essround, cntry, idno)

anyDuplicated(dat,      by = c("essround", "cntry", "idno"))
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
dat2[is.na(dat_sddf) & essround == 9]

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
dat2[(dat_sddf), .(essround, cntry, dweight, pspwght, pweight, anweight, prob)]

dat2[(dat_sddf), .N, keyby = .(is.na(prob), is.na(dweight))]

dat2[, .N, keyby = .(essround,
                     dweight  = !is.na(dweight),
                     pspwght  = !is.na(pspwght),
                     pweight  = !is.na(pweight),
                     anweight = !is.na(anweight))]

dat2[is.na(dweight), .(essround, cntry, dweight, pspwght, pweight, anweight)]


# Design weights computed from sampling probabilities
dat2[, summary(prob)]

dat2[prob == 0, .N, keyby = .(essround, cntry)]
dat2[prob >  1, .N, keyby = .(essround, cntry)]

dat2[prob >  0, dw := 1 / prob]
dat2[prob == 0 | prob > 1, dw := 1]

tmp <- dat2[(dat_sddf),
            .(essround, cntry, dweight, pspwght, pweight, anweight, prob, dw)]

tmp[, c(.(n = .N), lapply(.SD, sum)),
    .SDcols = c("dweight", "pspwght", "pweight", "anweight"),
    keyby = .(essround, cntry)]

tmp[, dweight2 := .N * dw / sum(dw), by = .(essround, cntry)]

tmp[is.na(dweight2)]

tmp[, all.equal(dweight, dweight2, check.attributes = F)]
tmp[order(abs(dweight - dweight2))]

tmp[is.na(dweight)]
tmp[is.na(dweight2)]
tmp[prob == 0]

tmp[, diff := dweight2 - dweight]

pl <- ggplot(tmp, aes(x = dweight, y = dweight2, colour = diff)) +
    geom_point() +
    scale_colour_gradient2(low = "blue", mid = "grey", high = "red") +
    facet_grid(essround ~ cntry) +
    theme_bw()

ggsave(filename = "results/plot_dweight.png", plot = pl, width = 16, height = 9)


# Extreme design weights are being cut



tmp[, .N, keyby = .(abs(dweight - dweight2) > .1)]

tmp[abs(dweight - dweight2) > .1][order(abs(dweight - dweight2))]

tmp[abs(dweight - dweight2) > .1,
    .(essround, cntry, prob, dw, dweight, dweight2)]

tmp[abs(dweight - dweight2) > .1 & dweight2 > dweight,
    .(essround, cntry, prob, dw, dweight, dweight2)]
tmp[abs(dweight - dweight2) > .1 & dweight2 < dweight,
    .(essround, cntry, prob, dw, dweight, dweight2)]

tmp[abs(dweight - dweight2) > .1, .N, keyby = .(essround, cntry)]
tmp[abs(dweight - dweight2) > .1, .N, keyby = .(dweight2 > dweight)]


# dat2[, weight_des := dw]
dat2[, weight_des := dweight * pweight * 10e3]
dat2[, weight_est := pspwght * pweight * 10e3]

dat2[, lapply(.SD, sum), .SDcols = c("weight_des", "weight_est"),
     keyby = .(essround, cntry)]


# Save ####

save(dat2, file = "data/dat2.Rdata")
