# ESS7 ####

require(data.table)
require(haven)
require(openxlsx)
require(vardpoor)


# Reset ####

rm(list = ls())
gc()


# Load data ####

load("data/ESS7/dat.Rdata")
load("data/ESS7/datSDDF.Rdata")



# Sampling information ####

datSDDF

names(datSDDF)

datSDDF[, lapply(.SD, class)]

datSDDF[, .N, cntry][order(N)]

str(datSDDF)

datSDDF[, .N, keyby = stratify]

tab_cntry <- datSDDF[, .(n_strat = sum(!duplicated(stratify)),
                         n_psu = sum(!duplicated(psu)),
                         n_resp = .N),
                     keyby = .(essround, edition, cntry)]

tab_stratify <- datSDDF[, .(n_psu = sum(!duplicated(psu)),
                            n_resp = .N),
                        keyby = .(essround, edition, cntry, stratify)]

tab_psu <- datSDDF[, .(n_resp = .N),
                   keyby = .(essround, edition, cntry, stratify, psu)]

tabl <- list(tab_cntry, tab_stratify, tab_psu)
names(tabl) <- c("cntry", "stratify", "psu")

write.xlsx(tabl, file = "data/ESS7/ESS7-SDDF-tables.xlsx",
           colWidths = "auto")


# Merge ####

intersect(names(dat), names(datSDDF))

dat[, .N, keyby = .(name, essround, edition, proddate)]
datSDDF[, .N, keyby = .(name, essround, edition, proddate)]

vars <- c("name", "edition", "proddate")
setnames(datSDDF, vars, paste0(vars, "SDDF"))

intersect(names(dat), names(datSDDF))

dat2 <- merge(dat, datSDDF, by = c("essround", "cntry", "idno"))



# Weights

dat2[, .(dweight, pspwght, pweight, prob)]

# Design weights computed from sampling probabilities
dat2[, dw := 1 / prob]

tmp <- dat2[, .(cntry, dweight, pspwght, pweight, prob, dw)]
tmp[, dweight2 := .N * dw / sum(dw), by = cntry]
tmp[, all.equal(dweight, dweight2, check.attributes = F)]
tmp[abs(dweight - dweight2) > .1][order(-abs(dweight - dweight2))]
tmp[abs(dweight - dweight2) > .1, .N, keyby = cntry]

dat2[, weight0 := dw]
dat2[, weight1 := dweight * pweight * 10e3]
dat2[, weight2 := dweight * pspwght * pweight * 10e3]

tab <- dat2[, lapply(.SD, sum), .SDcols = paste0("weight", 0:2), keyby = cntry]

tab[, paste0("p", 0:2) := lapply(.SD, function(x) round(x / weight2, 3)),
    .SDcols = paste0("weight", 0:2)]

tab



# PPLTRST Most people can be trusted or you can't be too careful
# Valid values are from 0 to 10

dat2[, .N, keyby = ppltrst]

dat2[!is.na(ppltrst), .(ppltrst0 = weighted.mean(ppltrst, weight0),
                        ppltrst1 = weighted.mean(ppltrst, weight1),
                        ppltrst2 = weighted.mean(ppltrst, weight2))]

# Compute Y and Z variables
dat2[, ppltrst_y := ifelse(is.na(ppltrst), 0L, ppltrst)]
dat2[, ppltrst_z := as.integer(!is.na(ppltrst))]

dat2[, .(ppltrst0 = sum(ppltrst_y * weight0) / sum(ppltrst_z * weight0),
         ppltrst1 = sum(ppltrst_y * weight1) / sum(ppltrst_z * weight1),
         ppltrst2 = sum(ppltrst_y * weight2) / sum(ppltrst_z * weight2))]

dat2[!is.na(ppltrst), .(ppltrst0 = weighted.mean(ppltrst, weight0),
                        ppltrst1 = weighted.mean(ppltrst, weight1),
                        ppltrst2 = weighted.mean(ppltrst, weight2)),
     keyby = .(essround, cntry)]

dat2[, .(ppltrst0 = sum(ppltrst_y * weight0) / sum(ppltrst_z * weight0),
         ppltrst1 = sum(ppltrst_y * weight1) / sum(ppltrst_z * weight1),
         ppltrst2 = sum(ppltrst_y * weight2) / sum(ppltrst_z * weight2)),
     keyby = .(essround, cntry)]

dat2[, .N, keyby = stratify]

tab <- dat2[, .(n = .N, pop0 = sum(weight0),
                pop1 = sum(weight1), pop2 = sum(weight2)),
            keyby = .(essround, cntry, stratify, psu)]
tab <- tab[, .(n = .N, pop0 = sum(pop0), pop1 = sum(pop1), pop2 = sum(pop2)),
           keyby = .(essround, cntry, stratify)]
tab
tab[n == 1 & pop0 > 1]

dat2[, essround_cntry := paste("ESS", essround, cntry, sep = "_")]
dat2[, .N, keyby = essround_cntry]

tab_deff0 <- vardom(Y = "ppltrst_y", Z = "ppltrst_z",
                    H = "stratify", PSU = "psu", w_final = "weight0",
                    period = "essround_cntry", fh_zero = TRUE,
                    dataset = dat2)

tab_deff1 <- vardom(Y = "ppltrst_y", Z = "ppltrst_z",
                    H = "stratify", PSU = "psu", w_final = "weight1",
                    period = "essround_cntry", fh_zero = TRUE,
                    dataset = dat2)

tab_deff2 <- vardom(Y = "ppltrst_y", Z = "ppltrst_z",
                    H = "stratify", PSU = "psu", w_final = "weight2",
                    period = "essround_cntry", fh_zero = TRUE,
                    dataset = dat2)

tab_deff <- rbindlist(list(tab_deff0$all_result,
                           tab_deff1$all_result,
                           tab_deff2$all_result), idcol = T)

tab_deff[, .N, keyby = .id]

tab_deff[, weight := paste0("weight", .id - 1)]

names(tab_deff)

dcast(tab_deff, essround_cntry + variable ~ weight,
      value.var = c("estim", "se", "deff"))
