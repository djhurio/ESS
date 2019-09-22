# Plot CI and SE

# Options ####
options(encoding = "UTF-8")
options(stringsAsFactors = F)


# Packages
require(data.table)
require(ggplot2)


# Reset ####
rm(list = ls())
gc()


# Load data ####
load("results/ESS_dat_deff.Rdata")

dat_deff

dat_deff[, essround := as.integer(substr(essround_cntry_dom, 5, 5))]
dat_deff[, cntry := substr(essround_cntry_dom, 7, 8)]

dat <- dat_deff[, lapply(.SD, sum), .SDcols = c("Y_est", "Z_est", "var"),
                keyby = .(cntry, essround, variable)]

dat[Z_est > 0, R_est := Y_est / Z_est]
dat[, se := sqrt(var)]
dat[R_est > 0, cv := se / R_est * 100]
dat[, ci_lo := R_est - qnorm(0.975) * se]
dat[, ci_hi := R_est + qnorm(0.975) * se]

cntry.list <- sort(unique(dat$cntry))

pl_ci <- function(x) ggplot(dat[cntry == x & !is.na(R_est)]) +
  geom_point(aes(x = essround, y = R_est)) +
  geom_errorbar(aes(x = essround, ymin = ci_lo, ymax = ci_hi)) +
  facet_wrap(~ variable, scales = "free_y") +
  theme_bw() + ggtitle(paste("Country", x))

pl_se <- function(x) ggplot(dat[cntry == x & !is.na(se)]) +
  geom_col(aes(x = essround, y = se)) +
  facet_wrap(~ variable, scales = "free_y") +
  theme_bw() + ggtitle(paste("Country", x))

pl_cv <- function(x) ggplot(dat[cntry == x & !is.na(cv)]) +
  geom_col(aes(x = essround, y = cv, fill = cv)) +
  facet_wrap(~ variable, scales = "free_y") +
  theme_bw() + ggtitle(paste("Country", x))

cairo_pdf(filename = "results/plot-ci-by-cntry.pdf",
          width = 16, height = 9, onefile = T)
lapply(cntry.list, pl_ci)
dev.off()

cairo_pdf(filename = "results/plot-se-by-cntry.pdf",
          width = 16, height = 9, onefile = T)
lapply(cntry.list, pl_se)
dev.off()

cairo_pdf(filename = "results/plot-cv-by-cntry.pdf",
          width = 16, height = 9, onefile = T)
lapply(cntry.list, pl_cv)
dev.off()
