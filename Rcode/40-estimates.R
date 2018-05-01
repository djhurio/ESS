# ESS design effect estimation ####

# Options ####
options(encoding = "UTF-8")
options(stringsAsFactors = F)

Sys.getenv("R_ZIPCMD")
Sys.getenv("R_ZIPCMD", "zip")
Sys.getenv("PATH")
Sys.setenv(R_ZIPCMD = "/usr/bin/zip")

# Packages
require(data.table)
require(haven)
require(openxlsx)
require(vardpoor)
require(ICC)
require(ggplot2)
require(ess)


# Reset ####
rm(list = ls())
gc()


# Load data ####
load("data/dat.Rdata")
load("data/datSDDF.Rdata")

show_rounds()

dat[, essround := factor(essround, show_rounds(), show_rounds())]
datSDDF[, essround := factor(essround, show_rounds(), show_rounds())]


# Sampling information ####
datSDDF

names(datSDDF)

datSDDF[, lapply(.SD, class)]

datSDDF[, .N, cntry][order(N)]

str(datSDDF)

datSDDF[, .N, keyby = domain]
datSDDF[, .N, keyby = .(essround, cntry, stratify)]
datSDDF[, .N, keyby = .(essround, cntry, stratify, domain)]

datSDDF[, .N, keyby = nchar(stratify)]


datSDDF[cntry == "FI", .N, keyby = .(essround, cntry, stratify)][, .N, keyby = .(essround, cntry)]

# Create one stratum for FI
datSDDF[cntry == "FI", stratify := 0]
datSDDF[cntry == "FI", .N, keyby = .(essround, cntry, stratify)]

# Create one stratum for LT R5
datSDDF[cntry == "LT", .N, keyby = .(essround, cntry, stratify)]
datSDDF[cntry == "LT" & essround == 5, stratify := 0]
datSDDF[cntry == "LT", .N, keyby = .(essround, cntry, stratify)]




# Create strata variable from country, domain and stratify
m <- max(nchar(datSDDF$stratify))
m

datSDDF[, STR := paste(essround, cntry, domain,
                       formatC(stratify, digits = m - 1, flag = 0),
                       sep = "_")]
datSDDF[, .N, keyby = .(essround, cntry, domain, stratify)]
datSDDF[, .N, keyby = .(STR)]

# Create PSU variable from STR and psu
datSDDF[, min(psu, na.rm = T)]

datSDDF[, .N, keyby = is.na(psu)]

datSDDF[, .N, keyby = .(essround, cntry)]
datSDDF[, .N, keyby = .(essround, cntry, is.na(psu))]

dcast(datSDDF, essround + cntry ~ is.na(psu))

# R1 Spain is problematic
datSDDF[essround == 1 & cntry == "ES", .N,
        keyby = .(essround, cntry, is.na(psu))]
datSDDF[essround == 1 & cntry == "ES", .N,
        keyby = .(essround, cntry, stratify, is.na(psu))]
datSDDF[essround == 1 & cntry == "ES"]


# This is wrong for R1 Spain
datSDDF[is.na(psu), psu := idno]

m <- datSDDF[, max(nchar(psu))]
m

datSDDF[, PSU := paste(STR, formatC(psu, digits = m - 1, flag = 0),
                       sep = "_")]

datSDDF[, .N, keyby = .(essround, cntry, domain, stratify, psu)]
datSDDF[, .N, keyby = .(PSU)]
datSDDF[, .N, keyby = .(STR, PSU)]


tab_cntry <- datSDDF[, .(n_strat = sum(!duplicated(STR)),
                         n_psu = sum(!duplicated(PSU)),
                         n_resp = .N), keyby = .(essround, cntry)]
tab_cntry

tab_strata <- datSDDF[, .(n_psu = sum(!duplicated(PSU)),
                          n_resp = .N),
                      keyby = .(essround, cntry, STR)]
tab_strata

tab_psu <- datSDDF[, .(n_resp = .N),
                   keyby = .(essround, cntry, STR, PSU)]
tab_psu

tabl <- list(tab_cntry, tab_strata, tab_psu)
names(tabl) <- c("cntry", "strata", "psu")

write.xlsx(tabl, file = "results/SDDF-tables.xlsx",
           colWidths = "auto", firstRow = T,
           headerStyle = createStyle(textDecoration = "italic",
                                     halign = "center"))


# Merge ####

intersect(names(dat), names(datSDDF))

dat[, dat_surv := T]
datSDDF[, dat_sddf := T]

setkey(dat, essround, cntry, idno)
setkey(datSDDF, essround, cntry, idno)

anyDuplicated(dat, by = c("essround", "cntry", "idno"))
anyDuplicated(datSDDF, by = c("essround", "cntry", "idno"))

dat[, .N]
datSDDF[, .N]

dat2 <- merge(dat, datSDDF, by = c("essround", "cntry", "idno"), all = T)

nrow(dat)
nrow(datSDDF)
nrow(dat2) - nrow(datSDDF)

dat2[, .N, keyby = .(dat_surv, dat_sddf)]
dat2[, .N, keyby = .(cntry, essround, dat_surv, dat_sddf)]

dat2[cntry == "CY" & essround == 5, .N,
     keyby = .(cntry, essround, dat_sddf)]

dat2[cntry == "CY" & essround == 5 & is.na(dat_sddf),
     .(essround, cntry, idno, dat_surv, dat_sddf,
       stratify, psu, prob, weight)]

dat2[is.na(dat_surv) & !is.na(dat_sddf),
     .(essround, cntry, idno, dat_surv, dat_sddf,
       stratify, psu, prob)]

dat2[cntry == "LT" & essround == 5,
     .(essround, cntry, idno, dat_surv, dat_sddf,
       stratify, psu, prob)]




# All survey data
dat2 <- merge(dat, datSDDF, by = c("essround", "cntry", "idno"))

# rm(dat, datSDDF)
# gc()

dat2[is.na(dat_surv), .(essround, cntry, idno)]


# Weights

dat2[(dat_sddf), .(essround, cntry, dweight, pspwght, pweight, prob)]

dat2[(dat_sddf), .N, keyby = .(is.na(prob), is.na(dweight))]

dat2[, .N, keyby = .(dweight = !is.na(dweight),
                     pspwght = !is.na(pspwght),
                     pweight = !is.na(pweight))]

dat2[, .N, keyby = .(essround, cntry, dweight = !is.na(dweight),
                     pspwght = !is.na(pspwght),
                     pweight = !is.na(pweight))]

dat2[is.na(dweight), .(essround, cntry, dweight, pspwght, pweight)]

# # Weight imputation
# dat2[is.na(dweight), dweight := 1]
# dat2[is.na(pspwght), pspwght := 1]

dat2[, .N, keyby = .(dweight = !is.na(dweight),
                     pspwght = !is.na(pspwght),
                     pweight = !is.na(pweight))]


# Design weights computed from sampling probabilities
dat2[(dat_sddf), dw := 1 / prob]

tmp <- dat2[(dat_sddf),
            .(essround, cntry, dweight, pspwght, pweight, prob, dw)]

tmp[, c(.(n = .N), lapply(.SD, sum)),
    .SDcols = c("dweight", "pspwght", "pweight"),
    keyby = .(essround, cntry)]

tmp[, dweight2 := .N * dw / sum(dw), by = .(essround, cntry)]

tmp[, all.equal(dweight, dweight2, check.attributes = F)]

tmp[abs(dweight - dweight2) > .1,
    .(essround, cntry, prob, dw, dweight, dweight2)]
tmp[abs(dweight - dweight2) > .1, .N, keyby = .(essround, cntry)]

dat2[, weight0 := dw]
dat2[, weight1 := dweight * pweight * 10e3]
dat2[, weight2 := dweight * pspwght * pweight * 10e3]

tab <- dat2[, lapply(.SD, sum), .SDcols = paste0("weight", 0:2),
            keyby = .(essround, cntry)]

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
    paste(sort(head(unique(dat2[[x]]), n = 10)), collapse = ", ")
  } else {
    NA_character_
  }
}

foo("vote")
foo("asd")

variables[, values := foo(varname), by = varname]

write.xlsx(variables, file = "results/variables.xlsx",
           colWidths = "auto", firstRow = T,
           headerStyle = createStyle(textDecoration = "italic",
                                     halign = "center"))

fwrite(variables, file = "tables/variables.csv", quote = T)


variables[, .N, keyby = is.available]

vars_binary <- variables[ grepl("binary", file) & (is.available), varname]
vars_other  <- variables[!grepl("binary", file) & (is.available), varname]

intersect(names(dat2), variables$varname)

head(names(dat2), 10)
tail(names(dat2), 10)

varnames.design <- c("essround", "cntry", "idno", "dat_sddf", "STR", "PSU",
                     "weight0", "weight1", "weight2")

dat3 <- dat2[, c(varnames.design, intersect(names(dat2), variables$varname)),
             with = F]

# rm(dat2)
# gc()

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

# lapply(vars_binary, function(x) dat3[, .N, keyby = c(x, paste0("y_", x), paste0("z_", x))])
# lapply(vars_other,  function(x) dat3[, .N, keyby = c(x, paste0("y_", x), paste0("z_", x))])

# dat3[order(-weight0), .(essround, cntry, weight0, weight1, weight2)][1:10]
# dat3[order(-weight1), .(essround, cntry, weight0, weight1, weight2)][1:10]
# dat3[order(-weight2), .(essround, cntry, weight0, weight1, weight2)][1:10]


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

dat3[(dat_sddf), .N, keyby = .(essround, cntry, STR)]

tab <- dat3[(dat_sddf), .(n = .N, pop = sum(weight0)),
            keyby = .(essround, cntry, STR, PSU)]
tab <- tab[, .(n = .N, pop = sum(pop)), keyby = .(essround, cntry, STR)]
tab
tab[n == 1 & pop > 1]
tab[n == 1 & pop > 1, .N, keyby = .(essround, cntry)]
tab[n == 1 & pop > 1, .N, keyby = .(cntry, essround)]

dat3[, essround_cntry := paste("ESS", essround, cntry, sep = "_")]
dat3[, .N, keyby = essround_cntry]

y_vars <- c(y_vars_binary, y_vars_other)
z_vars <- c(z_vars_binary, z_vars_other)

dat_deff0 <- vardom(Y = y_vars, Z = z_vars,
                    H = "STR", PSU = "PSU", w_final = "weight1",
                    period = "essround_cntry", fh_zero = TRUE,
                    dataset = dat3[(dat_sddf)], outp_lin = T)

dat3[, .N, keyby = .(essround_cntry, STR, PSU)]
dat3[, .N, keyby = .(essround_cntry, PSU)]
dat3[, .N, keyby = .(PSU)]


# Linearized variables ####

dat_deff0$lin_out

# Test
# names(dat_deff0$lin_out)
#
# dat3[, .N, keyby = cntry]
#
# ICCbare("psu", "y_vote", dat3[cntry == "AT"])
#
# tmpb <- dat3[, .N, keyby = .(essround_cntry, psu)][, .(b = mean(N)), keyby = essround_cntry]
# tmp_ <- dat3[, .(ICC = ICCbare(factor(psu), y_vote)), keyby = essround_cntry]
# tmp0 <- dat_deff0$lin_out[, .(ICC0 = ICCbare(factor(psu), y_vote)), keyby = essround_cntry]
# tmp1 <- dat_deff1$lin_out[, .(ICC1 = ICCbare(factor(psu), y_vote)), keyby = essround_cntry]
# tmp2 <- dat_deff2$lin_out[, .(ICC2 = ICCbare(factor(psu), y_vote)), keyby = essround_cntry]
#
# l <- mget(ls(pattern = "^tmp.$"))
#
# tmp <- Reduce(merge, l)
# tmp

# Average number of respondnets per PSU
dat_b <- dat3[(dat_sddf), .N, keyby = .(essround, cntry, essround_cntry, PSU)]
dat_b <- dat_b[, .(b = mean(N)), keyby = .(essround, cntry, essround_cntry)]
dat_b

# ggplot(dat_b) + geom_col(aes(x = cntry, y = b)) +
#   theme(axis.text.x = element_text(angle = 90, vjust = .5)) +
#   facet_grid(essround ~ .)

ggplot(dat_b) +
  geom_col(aes(x = essround, y = b, fill = essround)) +
  scale_y_continuous(breaks = 0:10, minor_breaks = NULL) +
  facet_wrap(~ cntry) +
  theme_bw()


dat_deff_p <- dat3[, .(deff_p = .N * sum(weight1 ^ 2) / sum(weight1) ^ 2),
                   keyby = .(essround, cntry, essround_cntry)]
dat_deff_p

# ggplot(dat_deff_p) + geom_col(aes(x = cntry, y = deff_p)) +
#   theme(axis.text.x = element_text(angle = 90, vjust = .5)) +
#   facet_grid(essround ~ .)

ggplot(dat_deff_p) +
  geom_col(aes(x = essround, y = deff_p, fill = essround)) +
  facet_wrap(~ cntry) +
  theme_bw()



dat_deff0$lin_out

lin_out <- melt(dat_deff0$lin_out,
                id.vars = c("id", "essround_cntry", "PSU"))

lin_out[, .N, keyby = is.na(value)][, P := prop.table(N)][]

lin_out <- lin_out[!is.na(value)]


# Estimate ICC
dat_ICC <- lin_out[, .(ICC = ICCbare(factor(PSU), value)),
                   keyby = .(essround_cntry, variable)]
warnings()

dat_ICC[is.na(ICC)]

save(dat_ICC, file = "data/dat_ICC_2.Rdata")

load("data/dat_ICC_2.Rdata")

dat_deff_mod <- merge(dat_ICC, dat_b, by = "essround_cntry", all.x = T)
dat_deff_mod[b == 1, ICC := 0]
dat_deff_mod[is.na(ICC)]

dat_deff_mod <- merge(dat_deff_mod, dat_deff_p,
                      by = c("essround_cntry", "essround", "cntry"), all.x = T)

dat_deff_mod[, deff_c := 1 + (b - 1) * ICC]

dat_deff_mod[, deff_mod := deff_p * deff_c]

dat_deff_mod[, variable := sub("y_", "", variable)]
dat_deff_mod


# tab_ICC <- dcast(dat_ICC, variable ~ essround_cntry, value.var = "ICC")
# tab_deff <- dcast(dat_ICC, variable ~ essround_cntry, value.var = "deff")


# dat_deff <- rbindlist(list(dat_deff0$all_result,
#                            dat_deff1$all_result,
#                            dat_deff2$all_result), idcol = T)
# dat_deff[, .N, keyby = .id]
# dat_deff[, weight := paste0("weight", .id - 1)]
# setorder(dat_deff, essround_cntry, weight)

dat_deff_est <- dat_deff0$all_result
# setorder(dat_deff_est, essround_cntry, variable)

dat_deff_est[, variable := gsub("^.*_", "", variable)]

dat_deff_est[!is.na(estim), .N]
dat_deff_mod[, .N]

intersect(names(dat_deff_est), names(dat_deff_mod))

dat_deff <- merge(dat_deff_est[!is.na(estim)], dat_deff_mod,
                  by = c("essround_cntry", "variable"))
dat_deff

dat_deff[is.na(deff_sam), .N]
dat_deff[is.na(deff_mod), .N]

names(dat_deff)

dat_deff[is.na(deff_sam), .(essround_cntry, variable, estim, Y_est, Z_est,
                            deff_sam, deff_mod)][order(estim)]

dat_deff[is.na(deff_sam) & !is.na(deff_mod),
         .(essround_cntry, variable, estim, Y_est, Z_est,
           deff_sam, deff_mod)][order(estim)]

dat_deff[, .N] - dat_deff[Y_est > 0 & Y_est != Z_est, .N]

tab_deff <- dat_deff[Y_est > 0 & Y_est != Z_est,
                     c(.(n_variable = .N), lapply(.SD, mean)),
                     keyby = .(essround, cntry),
                     .SDcols = c("deff_sam", "deff_mod",
                                 "deff_p", "deff_c", "ICC", "b", "pop_size",
                                 "respondent_count")]
tab_deff

tab_deff[, n_eff_sam := respondent_count / deff_sam]
tab_deff[, n_eff_mod := respondent_count / deff_mod]

# tab_deff[, sum(n_variable)]

tab_deff <- merge(tab_deff, tab_cntry)

tab_deff[, min_n_eff := ifelse(pop_size < 2e6, 800L, 1500L)]

tab_deff[, assessment := n_eff_mod > min_n_eff]

tab_deff[, .N, keyby = .(essround, assessment)]



pl_deff <- ggplot(dat_deff[Y_est > 0 & Y_est != Z_est]) +
  geom_point(aes(deff_sam, deff_mod, colour = essround)) +
  geom_abline(slope = 1, intercept = 0, colour = "red") +
  geom_vline(xintercept = 1, colour = "red", linetype = "dotted") +
  geom_hline(yintercept = 1, colour = "red", linetype = "dotted") +
  facet_wrap(~ cntry) +
  ggtitle("ESS Design Effect Estimates") +
  xlab("Deff estimated from survey data") +
  ylab("Deff estimated by ICC") +
  theme_bw()
pl_deff


pl_fun <- function(x) {
  ggplot(dat_deff[Y_est > 0 & Y_est != Z_est & cntry == x]) +
    geom_point(aes(deff_sam, deff_mod, colour = essround)) +
    geom_abline(slope = 1, intercept = 0, colour = "red") +
    geom_vline(xintercept = 1, colour = "red", linetype = "dotted") +
    geom_hline(yintercept = 1, colour = "red", linetype = "dotted") +
    ggtitle(paste("ESS Design Effect Estimates for", x)) +
    xlab("Deff estimated from survey data") +
    ylab("Deff estimated by ICC") +
    theme_bw()
}

list.cntry <- dat_deff[, unique(cntry)]

pl_deff_cntry <- lapply(list.cntry, pl_fun)

pl_deff_cntry[[1]]


pl_summary_deff <- ggplot(tab_deff, aes(deff_sam, deff_mod, colour = essround)) +
  geom_point() +
  geom_text(aes(label = cntry), hjust = 0, vjust = 0, alpha = .5) +
  geom_abline(slope = 1, intercept = 0, colour = "red") +
  geom_vline(xintercept = 1, colour = "red", linetype = "dotted") +
  geom_hline(yintercept = 1, colour = "red", linetype = "dotted") +
  ggtitle("ESS Design Effect Estimates") +
  xlab("Deff estimated from survey data") +
  ylab("Deff estimated by ICC") +
  theme_bw()
pl_summary_deff

names(tab_deff)

pl_summary_effss_1500 <- ggplot(tab_deff[min_n_eff == 1500]) +
  geom_col(aes(x = cntry, y = n_eff_mod, fill = assessment)) +
  facet_grid(essround ~ .) +
  geom_hline(yintercept = 1500, colour = "red", linetype = "dotted") +
  ggtitle("ESS Effective Sample Size",
          "Population more than 2 million people aged 15 or over") +
  xlab("Country") +
  ylab("Effective Sample Size") +
  theme_bw()
pl_summary_effss_1500

pl_summary_effss_800 <- ggplot(tab_deff[min_n_eff == 800]) +
  geom_col(aes(x = cntry, y = n_eff_mod, fill = assessment)) +
  facet_grid(essround ~ .) +
  geom_hline(yintercept = 800, colour = "red", linetype = "dotted") +
  ggtitle("ESS Effective Sample Size",
          "Population fewer than 2 million people aged 15 or over") +
  xlab("Country") +
  ylab("Effective Sample Size") +
  theme_bw()
pl_summary_effss_800

tab_deff[cntry == "CY"]


pdf("results/ESS_plot_deff_2.pdf", width = 16, height = 9)
pl_deff
dev.off()

pdf("results/ESS_plot_deff_cntry_2.pdf", width = 16, height = 9)
pl_deff_cntry
dev.off()

pdf("results/ESS_plot_summary_2.pdf", width = 16, height = 9)
pl_summary_deff
pl_summary_effss_1500
pl_summary_effss_800
dev.off()


# Save ###

# For each parameter

save(dat_deff, file = "results/ESS_dat_deff_2.Rdata")

write.xlsx(dat_deff, file = "results/ESS_dat_deff_2.xlsx",
           colWidths = "auto", firstRow = T,
           headerStyle = createStyle(textDecoration = "italic",
                                     halign = "center"))

fwrite(dat_deff, file = "results/ESS_dat_deff_2.csv", quote = T)


# Average

save(tab_deff, file = "results/ESS_tab_deff_2.Rdata")

write.xlsx(tab_deff, file = "results/ESS_tab_deff_2.xlsx",
           colWidths = "auto", firstRow = T,
           headerStyle = createStyle(textDecoration = "italic",
                                     halign = "center"))

fwrite(tab_deff, file = "results/ESS_tab_deff_2.csv", quote = T)
