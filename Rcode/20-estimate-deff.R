# ESS design effect estimation ####

# Options ####
options(encoding = "UTF-8")
options(stringsAsFactors = F)


# Packages
require(data.table)
require(openxlsx)
require(vardpoor)
require(ICC)
require(ggplot2)
require(essurvey)


# Reset ####

rm(list = ls())
gc()


# Load data ####

load("data/dat2.Rdata")
load("data/variables.Rdata")

variables[varname == "ppltrst"]

vars_binary <- variables[ grepl("binary", file) & (is.available), varname]
vars_other  <- variables[!grepl("binary", file) & (is.available), varname]

y_vars_binary <- paste("y", vars_binary, sep = "_")
z_vars_binary <- paste("z", vars_binary, sep = "_")

y_vars_other <- paste("y", vars_other, sep = "_")
z_vars_other <- paste("z", vars_other, sep = "_")

dat2[, c(y_vars_binary) := lapply(.SD, function(x) as.integer(!is.na(x) & x == 1)),
     .SDcols = vars_binary]
dat2[, c(z_vars_binary) := lapply(.SD, function(x) as.integer(!is.na(x))),
     .SDcols = vars_binary]

dat2[, c(y_vars_other) := lapply(.SD, function(x) ifelse(!is.na(x), x, 0)),
     .SDcols = vars_other]
dat2[, c(z_vars_other) := lapply(.SD, function(x) as.integer(!is.na(x))),
     .SDcols = vars_other]

# lapply(vars_binary, function(x) dat2[, .N, keyby = c(x, paste0("y_", x), paste0("z_", x))])
# lapply(vars_other,  function(x) dat2[, .N, keyby = c(x, paste0("y_", x), paste0("z_", x))])



# PPLTRST Most people can be trusted or you can't be too careful
# Valid values are from 0 to 10

dat2[, .N, keyby = ppltrst]

dcast(dat2, essround + cntry ~ ppltrst)

dat2[!is.na(ppltrst), var(ppltrst), keyby = .(essround, cntry)]

dat2[!is.na(ppltrst), .(ppltrst1 = weighted.mean(ppltrst, weight1))]

# Test run ####
dat2[, .(ppltrst1 = sum(y_ppltrst * weight1) / sum(z_ppltrst * weight1))]

dat2[!is.na(ppltrst), .(ppltrst1 = weighted.mean(ppltrst, weight1)),
     keyby = .(essround, cntry)]

dat2[, .(ppltrst1 = sum(y_ppltrst * weight1) / sum(z_ppltrst * weight1)),
     keyby = .(essround, cntry)]


# Live run ####

dat2[, .N, keyby = .(essround, cntry, STR)]

tab <- dat2[, .(n = .N, pop = sum(weight1)),
            keyby = .(essround, cntry, STR, PSU)]
tab <- tab[, .(n = .N, pop = sum(pop)), keyby = .(essround, cntry, STR)]
tab
tab[n == 1 & pop > 1]

dat2[, essround_cntry := paste("ESS", essround, cntry, sep = "_")]
dat2[, .N, keyby = essround_cntry]

dat2[, essround_cntry_dom := paste("ESS", essround, cntry, domain, sep = "_")]
dat2[, .N, keyby = essround_cntry_dom]

y_vars <- c(y_vars_binary, y_vars_other)
z_vars <- c(z_vars_binary, z_vars_other)

dat_deff0 <- vardom(Y = y_vars, Z = z_vars,
                    H = "STR", PSU = "PSU", w_final = "weight1",
                    period = "essround_cntry_dom", fh_zero = TRUE,
                    dataset = dat2, outp_lin = T)

dat2[, .N, keyby = .(essround_cntry, STR, PSU)]
dat2[, .N, keyby = .(essround_cntry, PSU)]
dat2[, .N, keyby = .(PSU)]

# Linearized variables ####

dat_deff0$lin_out

# Test
# names(dat_deff0$lin_out)
#
# dat2[, .N, keyby = cntry]
#
# ICCbare("PSU", "y_vote", dat2[cntry == "AT"])
#
# tmpb <- dat2[, .N, keyby = .(essround_cntry, PSU)][, .(b = mean(N)), keyby = essround_cntry]
# tmp_ <- dat2[, .(ICC = ICCbare(factor(PSU), y_vote)), keyby = essround_cntry]
# tmp0 <- dat_deff0$lin_out[, .(ICC0 = ICCbare(factor(PSU), y_vote)), keyby = essround_cntry]
# tmp1 <- dat_deff1$lin_out[, .(ICC1 = ICCbare(factor(PSU), y_vote)), keyby = essround_cntry]
# tmp2 <- dat_deff2$lin_out[, .(ICC2 = ICCbare(factor(PSU), y_vote)), keyby = essround_cntry]
#
# l <- mget(ls(pattern = "^tmp.$"))
#
# tmp <- Reduce(merge, l)
# tmp

# Average number of respondnets per PSU
dat_b <- dat2[, .N, keyby = .(essround, cntry, domain,
                              essround_cntry_dom, PSU)]
dat_b <- dat_b[, .(b = mean(N)), keyby = .(essround, cntry, domain,
                                           essround_cntry_dom)]
dat_b

# ggplot(dat_b) + geom_col(aes(x = cntry, y = b)) +
#   theme(axis.text.x = element_text(angle = 90, vjust = .5)) +
#   facet_grid(essround ~ .)

pl_b <- ggplot(dat_b) +
  geom_col(aes(x = essround, y = b,
               fill = essround, linetype = domain),
           colour = "black", position = "dodge") +
  ggtitle("ESS avearge cluster size (b)") +
  facet_wrap(~ cntry)


dat_deff_p <- dat2[, .(deff_p = .N * sum(weight1 ^ 2) / sum(weight1) ^ 2),
                   keyby = .(essround, cntry, domain, essround_cntry_dom)]
dat_deff_p

# ggplot(dat_deff_p) + geom_col(aes(x = cntry, y = deff_p)) +
#   theme(axis.text.x = element_text(angle = 90, vjust = .5)) +
#   facet_grid(essround ~ .)

pl_deff_p <- ggplot(dat_deff_p) +
  geom_col(aes(x = essround, y = deff_p,
               fill = essround, linetype = domain),
           colour = "black", position = "dodge") +
  ggtitle("ESS deff_p") +
  facet_wrap(~ cntry)



dat_deff0$lin_out

lin_out <- melt(dat_deff0$lin_out,
                id.vars = c("id", "essround_cntry_dom", "PSU"))
# lin_out[, PSU := factor(PSU)]
setkey(lin_out, essround_cntry_dom, variable, PSU, id)


# Estimate ICC

lin_out[!is.na(value)]

lin_out[!is.na(value), sd := sd(value),
        by = .(essround_cntry_dom, variable)]

lin_out[sd == 0]

# vlist <- sort(unique(lin_out$essround_cntry_dom))
dat_b[, summary(b)]
vlist <- dat_b[, essround_cntry_dom]

estimICC <- function(x) {
  cat(x, "\n\n")
  dat <- lin_out[essround_cntry_dom == x & !is.na(value) & sd > 0]
  dat[, psu := factor(PSU)]
  dat[, .(ICC = ICCbare(psu, value)),
      keyby = .(essround_cntry_dom, variable)]
}

# estimICC(vlist[3])
# lin_out[essround_cntry_dom == vlist[1] & variable == "y_dscretn"][order(value)]
# dat2[essround_cntry_dom == vlist[1],
#      .(dscretn, y_dscretn, z_dscretn)][order(dscretn)]

# dat_ICC <- lapply(vlist, estimICC)
# dat_ICC <- rbindlist(dat_ICC, use.names = T)
# save(dat_ICC, file = "data/dat_ICC.Rdata")

load("data/dat_ICC.Rdata")

dat_deff_mod <- merge(dat_ICC, dat_b,
                      by = "essround_cntry_dom", all.x = T)
dat_deff_mod[b == 1, ICC := 0]

dat_deff_mod <- merge(dat_deff_mod, dat_deff_p,
                      by = c("essround_cntry_dom",
                             "essround", "cntry", "domain"), all.x = T)

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

dat_deff_est[n_nonzero > 0 & var > 0]

dat_deff_est[, .N]
dat_deff_est[n_nonzero > 0, .N]
dat_deff_est[var > 0, .N]

dat_deff_mod[, .N]

intersect(names(dat_deff_est), names(dat_deff_mod))

dat_deff <- merge(dat_deff_est, dat_deff_mod,
                  by = c("essround_cntry_dom", "variable"), all = T)
dat_deff

dat_deff[is.na(deff_sam), .N]
dat_deff[is.na(deff_mod), .N]

names(dat_deff)

dat_deff[is.na(deff_sam), .(essround_cntry_dom, variable,
                            estim, Y_est, Z_est,
                            deff_sam, deff_mod)][order(estim)]

dat_deff[is.na(deff_sam) & !is.na(deff_mod),
         .(essround_cntry_dom, variable, estim, Y_est, Z_est,
           deff_sam, deff_mod)][order(estim)]

dat_deff[is.na(deff_sam) & !is.na(deff_mod),
         .(essround_cntry_dom, variable, estim, Y_est, Z_est,
           deff_sam, deff_mod)][order(estim)]

dat_deff[, .N] - dat_deff[Y_est > 0 & Y_est != Z_est, .N]

dat_deff[Y_est > 0 & Y_est != Z_est, .N]
dat_deff[!is.na(deff_sam), .N]
dat_deff[Y_est > 0 & Y_est != Z_est & is.na(deff_sam), .N]
dat_deff[Y_est > 0 & Y_est != Z_est & is.na(deff_sam)]

dat_deff[, .N, keyby = .(n_nonzero)]

dat_deff[!is.na(deff_mod), .N]
dat_deff[!is.na(deff_sam), .N]
dat_deff[var > 0, .N]
dat_deff[!is.na(deff_sam) & var == 0, .N]

tab_deff <- dat_deff[!is.na(deff_mod) & !is.na(deff_sam),
                     c(.(n_variable = .N), lapply(.SD, mean)),
                     keyby = .(essround, cntry, domain),
                     .SDcols = c("deff_sam", "deff_mod",
                                 "deff_p", "deff_c", "ICC", "b",
                                 "pop_size", "respondent_count")]
tab_deff
tab_deff[, sum(n_variable)]

tab_cntry <- dat2[, .(n_strat = sum(!duplicated(STR)),
                      n_psu = sum(!duplicated(PSU)),
                      n_resp = .N), keyby = .(essround, cntry, domain)]

tab_deff <- merge(tab_deff, tab_cntry,
                  by = c("essround", "cntry", "domain"))

tab_deff[, all.equal(n_resp, respondent_count)]

tab_deff[, n_eff_sam := n_resp / deff_sam]
tab_deff[, n_eff_mod := n_resp / deff_mod]



# tab_deff[, min_n_eff := ifelse(pop_size < 2e6, 800L, 1500L)]
#
# tab_deff[, assessment := n_eff > min_n_eff]
#
# tab_deff[, .N, keyby = .(essround, assessment)]



pl_deff <- ggplot(dat_deff[!is.na(deff_sam) & !is.na(deff_mod)]) +
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
  ggplot(dat_deff[!is.na(deff_sam) & !is.na(deff_mod) & cntry == x]) +
    geom_point(aes(deff_sam, deff_mod, colour = essround)) +
    geom_abline(slope = 1, intercept = 0, colour = "red") +
    geom_vline(xintercept = 1, colour = "red", linetype = "dotted") +
    geom_hline(yintercept = 1, colour = "red", linetype = "dotted") +
    ggtitle(paste("ESS Design Effect Estimates for", x)) +
    xlab("Deff estimated from survey data") +
    ylab("Deff estimated by ICC") +
    theme_bw()
}

list.cntry <- dat2[, sort(unique(cntry))]

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

# pl_summary_effss_1500 <- ggplot(tab_deff[min_n_eff == 1500]) +
#   geom_col(aes(x = cntry, y = n_eff, fill = assessment)) +
#   facet_grid(essround ~ .) +
#   geom_hline(yintercept = 1500, colour = "red", linetype = "dotted") +
#   ggtitle("ESS Effective Sample Size",
#           "Population more than 2 million people aged 15 or over") +
#   xlab("Country") +
#   ylab("Effective Sample Size") +
#   theme_bw()
# pl_summary_effss_1500
#
# pl_summary_effss_800 <- ggplot(tab_deff[min_n_eff == 800]) +
#   geom_col(aes(x = cntry, y = n_eff, fill = assessment)) +
#   facet_grid(essround ~ .) +
#   geom_hline(yintercept = 800, colour = "red", linetype = "dotted") +
#   ggtitle("ESS Effective Sample Size",
#           "Population fewer than 2 million people aged 15 or over") +
#   xlab("Country") +
#   ylab("Effective Sample Size") +
#   theme_bw()
# pl_summary_effss_800

tab_deff[cntry == "CY"]


pdf("results/ESS_plot_deff.pdf", width = 16, height = 9)
pl_b
pl_deff_p
pl_deff
pl_deff_cntry
pl_summary_deff
# pl_summary_effss_1500
# pl_summary_effss_800
dev.off()


# Save ###

# For each parameter

save(dat_deff, file = "results/ESS_dat_deff.Rdata")

write.xlsx(dat_deff, file = "results/ESS_dat_deff.xlsx",
           colWidths = "auto", firstRow = T,
           headerStyle = createStyle(textDecoration = "italic",
                                     halign = "center"))

fwrite(dat_deff, file = "results/ESS_dat_deff.csv", quote = T)

# Average

save(tab_deff, file = "results/ESS_tab_deff.Rdata")

write.xlsx(tab_deff, file = "results/ESS_tab_deff.xlsx",
           colWidths = "auto", firstRow = T,
           headerStyle = createStyle(textDecoration = "italic",
                                     halign = "center"))

fwrite(tab_deff, file = "results/ESS_tab_deff.csv", quote = T)
