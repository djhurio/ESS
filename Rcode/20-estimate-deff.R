# ESS design effect estimation ####

# Options ####
options(encoding = "UTF-8")
options(stringsAsFactors = F)

# Sys.getenv("R_ZIPCMD")
Sys.getenv("R_ZIPCMD", "zip")
Sys.setenv(R_ZIPCMD = "/usr/bin/zip")

# Packages
require(data.table)
require(haven)
require(openxlsx)
require(vardpoor)


# Reset ####

rm(list = ls())
gc()


# Load data ####

load("data/dat.Rdata")
load("data/datSDDF.Rdata")


# Sampling information ####

datSDDF

names(datSDDF)

datSDDF[, lapply(.SD, class)]

datSDDF[, .N, cntry][order(N)]

str(datSDDF)

datSDDF[, .N, keyby = domain]
datSDDF[, .N, keyby = stratify]
datSDDF[, .N, keyby = .(domain, stratify)]

datSDDF[, .N, keyby = nchar(stratify)]

m <- max(nchar(datSDDF$stratify))


# Create strata variable from domain and stratify
datSDDF[, strata := paste(domain, formatC(stratify, digits = m - 1, flag = 0),
                          sep = "_")]
datSDDF[, .N, keyby = .(domain, stratify, strata)]


tab_cntry <- datSDDF[, .(n_strat = sum(!duplicated(strata)),
                         n_psu = sum(!duplicated(psu)),
                         n_resp = .N),
                     keyby = .(essround, edition, cntry)]

tab_strata <- datSDDF[, .(n_psu = sum(!duplicated(psu)),
                          n_resp = .N),
                      keyby = .(essround, edition, cntry, strata)]

tab_psu <- datSDDF[, .(n_resp = .N),
                   keyby = .(essround, edition, cntry, strata, psu)]

tabl <- list(tab_cntry, tab_strata, tab_psu)
names(tabl) <- c("cntry", "strata", "psu")

write.xlsx(tabl, file = "results/SDDF-tables.xlsx",
           colWidths = "auto", firstRow = T,
           headerStyle = createStyle(textDecoration = "bold",
                                     halign = "center"))


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


# Variables ####

variables <- lapply(list.files("variables", full.names = T), read.table)
names(variables) <- list.files("variables")

variables <- rbindlist(variables, idcol = "file")
setnames(variables, "V1", "varname")
variables[, varname := tolower(varname)]
variables

variables[, is.available := varname %in% names(dat2)]

foo <- function(x) {
  if (x %in% names(dat2)) {
    paste(sort(head(unique(dat2[[x]]))), collapse = ", ")
  } else {
    NA_character_
  }
}

foo("vote")
foo("asd")

variables[, values := foo(varname), by = varname]

write.xlsx(variables, file = "results/variables.xlsx",
           colWidths = "auto", firstRow = T,
           headerStyle = createStyle(textDecoration = "bold",
                                     halign = "center"))
fwrite(variables, file = "tables/variables.csv", quote = T)


variables[, .N, keyby = is.available]

vars_binary <- variables[grepl("binary", file) & (is.available), varname]
vars_other <- variables[!grepl("binary", file) & (is.available), varname]


dat3 <- copy(dat2)

y_vars_binary <- paste("y", vars_binary, sep = "_")
z_vars_binary <- paste("z", vars_binary, sep = "_")

y_vars_other <- paste("y", vars_other, sep = "_")
z_vars_other <- paste("z", vars_other, sep = "_")

dat3[, c(y_vars_binary) := lapply(.SD, function(x) as.integer(!is.na(x) & x == 1)),
     .SDcols = vars_binary]
dat3[, c(z_vars_binary) := lapply(.SD, function(x) as.integer(!is.na(x))),
     .SDcols = vars_binary]

dat3[, c(y_vars_other) := lapply(.SD, function(x) ifelse(!is.na(x), x, 0)),
     .SDcols = vars_other]
dat3[, c(z_vars_other) := lapply(.SD, function(x) as.integer(!is.na(x))),
     .SDcols = vars_other]

lapply(vars_binary, function(x) dat3[, .N, keyby = c(x, paste0("y_", x), paste0("z_", x))])
lapply(vars_other, function(x) dat3[, .N, keyby = c(x, paste0("y_", x), paste0("z_", x))])


# # PPLTRST Most people can be trusted or you can't be too careful
# # Valid values are from 0 to 10
#
# dat2[, .N, keyby = ppltrst]
#
# dcast(dat2, essround + cntry ~ ppltrst)
#
# dat2[!is.na(ppltrst), var(ppltrst), keyby = .(essround, cntry)]
#
# dat2[!is.na(ppltrst), .(ppltrst0 = weighted.mean(ppltrst, weight0),
#                         ppltrst1 = weighted.mean(ppltrst, weight1),
#                         ppltrst2 = weighted.mean(ppltrst, weight2))]
#
# # Compute Y and Z variables
# dat2[, ppltrst_y := ifelse(is.na(ppltrst), 0L, ppltrst)]
# dat2[, ppltrst_z := as.integer(!is.na(ppltrst))]
#
# dat2[, .(ppltrst0 = sum(ppltrst_y * weight0) / sum(ppltrst_z * weight0),
#          ppltrst1 = sum(ppltrst_y * weight1) / sum(ppltrst_z * weight1),
#          ppltrst2 = sum(ppltrst_y * weight2) / sum(ppltrst_z * weight2))]
#
# dat2[!is.na(ppltrst), .(ppltrst0 = weighted.mean(ppltrst, weight0),
#                         ppltrst1 = weighted.mean(ppltrst, weight1),
#                         ppltrst2 = weighted.mean(ppltrst, weight2)),
#      keyby = .(essround, cntry)]
#
# dat2[, .(ppltrst0 = sum(ppltrst_y * weight0) / sum(ppltrst_z * weight0),
#          ppltrst1 = sum(ppltrst_y * weight1) / sum(ppltrst_z * weight1),
#          ppltrst2 = sum(ppltrst_y * weight2) / sum(ppltrst_z * weight2)),
#      keyby = .(essround, cntry)]

dat3[, .N, keyby = strata]

tab <- dat3[, .(n = .N, pop0 = sum(weight0),
                pop1 = sum(weight1), pop2 = sum(weight2)),
            keyby = .(essround, cntry, strata, psu)]
tab <- tab[, .(n = .N, pop0 = sum(pop0), pop1 = sum(pop1), pop2 = sum(pop2)),
           keyby = .(essround, cntry, strata)]
tab
tab[n == 1 & pop0 > 1]

dat3[, essround_cntry := paste("ESS", essround, cntry, sep = "_")]
dat3[, .N, keyby = essround_cntry]

y_vars <- c(y_vars_binary, y_vars_other)
z_vars <- c(z_vars_binary, z_vars_other)

tab_deff0 <- vardom(Y = y_vars, Z = z_vars,
                    H = "strata", PSU = "psu", w_final = "weight0",
                    period = "essround_cntry", fh_zero = TRUE,
                    dataset = dat3)

tab_deff1 <- vardom(Y = y_vars, Z = z_vars,
                    H = "strata", PSU = "psu", w_final = "weight1",
                    period = "essround_cntry", fh_zero = TRUE,
                    dataset = dat3)

tab_deff2 <- vardom(Y = y_vars, Z = z_vars,
                    H = "strata", PSU = "psu", w_final = "weight2",
                    period = "essround_cntry", fh_zero = TRUE,
                    dataset = dat3)

tab_deff <- rbindlist(list(tab_deff0$all_result,
                           tab_deff1$all_result,
                           tab_deff2$all_result), idcol = T)

tab_deff[, .N, keyby = .id]

tab_deff[, weight := paste0("weight", .id - 1)]

setorder(tab_deff, essround_cntry, weight)



names(tab_deff)

tab_deff_subsel <- dcast(tab_deff, essround_cntry + variable ~ weight,
                         value.var = c("estim", "se", "deff"))
tab_deff_subsel


tab_deff_summary <- dcast(tab_deff, essround_cntry ~ weight,
                          fun.aggregate = mean, na.rm = T,
                          value.var = c("deff", "n_eff"))
setorder(tab_deff_summary, deff_weight0)
tab_deff_summary



tabl <- list(tab_deff, tab_deff_subsel, tab_deff_summary)
names(tabl) <- c("all_estimates", "subselection", "summary")

write.xlsx(tabl, file = "results/ESS-deff.xlsx",
           colWidths = "auto", firstRow = T,
           headerStyle = createStyle(textDecoration = "bold",
                                     halign = "center"))

fwrite(tab_deff, file = "results/tab_deff.csv", quote = T)
fwrite(tab_deff_subsel, file = "results/tab_deff_subsel.csv", quote = T)
fwrite(tab_deff_summary, file = "results/tab_deff_summary.csv", quote = T)
