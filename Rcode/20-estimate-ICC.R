# ICC estimation ####

# Options
options(encoding = "UTF-8")
options(max.print = 10e3)
options(datatable.integer64 = "character")
options(datatable.keepLeadingZeros = TRUE)
options(warn = 2) # Will stop on warning

# Packages
require(essurvey)
require(data.table)
require(openxlsx)
require(ggplot2)

# Reset
rm(list = ls())
gc()


# Load data
load("data/dat2.Rdata")
load("data/variables.Rdata")


# Average number of respondnets per PSU
dat_b <- dat2[, .N, keyby = .(essround, cntry, domain, PSU)]
dat_b <- dat_b[, .(b = mean(N)), keyby = .(essround, cntry, domain)]

dat_b
dat_b[order(b)]
dat_b[, summary(b)]

dat_b[b == 1]


# Transfer data to long format
dat3 <- melt.data.table(data = dat2,
                        id.vars = c("essround", "cntry", "domain",
                                    "STR", "PSU", "idno",
                                    "weight_des", "weight_est"),
                        measure.vars = variables$varname, na.rm = F,
                        variable.name = "varname", variable.factor = F)


# Add type of variable
dat3 <- merge(dat3, variables[, .(varname, type)],
              by = "varname", all.x = T, sort = F)
dat3[, .N, keyby = .(type)]


# Ratio of two totals is used for the estimation
# Calculate Y and Z values for ratio estimation

# Y
dat3[type == "Binary", value_y := as.numeric(value == 1)]
dat3[type != "Binary", value_y := value]
dat3[is.na(value), value_y := 0]

# Z
dat3[, value_z := as.integer(!is.na(value))]


# Test
dat3[, lapply(.SD, function(x) round(mean(x), 3)),
     .SDcols = c("value_y", "value_z"), by = .(varname)][order(-value_y)]
dat3[, lapply(.SD, function(x) round(mean(x), 3)),
     .SDcols = c("value_y", "value_z"), by = .(varname)][order(-value_z)]

dcast.data.table(data = dat3, formula = varname ~ paste0("R", essround),
                 value.var = "value_y",
                 fun.aggregate = function(x) round(mean(x), 3))
dcast.data.table(data = dat3, formula = varname ~ paste0("R", essround),
                 value.var = "value_z",
                 fun.aggregate = function(x) round(mean(x), 3))


# Variable name extended with round, country, and domain
dat3[, varname_ext := paste(paste0("R", essround), cntry, paste0("D", domain),
                            varname, sep = "_")]
setkey(dat3, varname_ext)
dat3[, .N, keyby = .(varname_ext)]


# Taylor linearisation of ratio of two totals
dat3[, total_Y := sum(value_y * weight_est), by = .(varname_ext)]
dat3[, total_Z := sum(value_z * weight_est), by = .(varname_ext)]
dat3[total_Z > 0, lin_val := (value_y - total_Y / total_Z * value_z) / total_Z]



# PSU variance
tab_psu <- dat3[!is.na(value), .(n = .N, sd_y_psu = sd(value_y)),
                keyby = .(varname_ext, PSU)]
tab_psu[n == 1L, sd_y_psu := 0]
tab_psu <- tab_psu[, .(max_sd_y_psu = max(sd_y_psu)), keyby = .(varname_ext)]
tab_psu[max_sd_y_psu == 0]

tab_psu[("R5_LT_D1_emplno")]


# Summary table
tab_variables <- dat3[, .(n_resp = .N,
                          n_na = sum(is.na(value)),
                          sd_y = sd(value_y),
                          pop_size = sum(weight_est),
                          total_Y  = sum(weight_est * value_y),
                          total_Z  = sum(weight_est * value_z)),
                      keyby = .(varname_ext, essround, cntry, domain,
                                varname, type)]

tab_variables <- merge(tab_variables, dat_b,
                       by = c("essround", "cntry", "domain"), all.x = T)

tab_variables <- merge(tab_variables, tab_psu,
                       by = c("varname_ext"), all.x = T)

tab_variables[total_Z > 0, ratio := total_Y / total_Z]


# Cases where esimtation is not possible
tab_variables[n_na == n_resp] # All cases missing
tab_variables[n_resp - n_na == 1L] # Only 1 respondent
tab_variables[n_na < n_resp & total_Y == 0] # All answers are 0
tab_variables[n_na < n_resp & total_Y > 0 & total_Y == total_Z] # All answers are 1
tab_variables[sd_y == 0] # No variation in variable
tab_variables[max_sd_y_psu == 0] # No variation in PSUs

# Mark varibales where estimation of effective sample size is not possible:
# 1) variable is a constant (sd_y == 0)
# 2) mean estimate is 1 (total_Y == total_Z)
# 3) There is only one respondent ((n_resp - n_na) == 1L)
# 4) There is no variance in PSUs (max_sd_y_psu == 0)
tab_variables[, flag := (b > 1) & (sd_y == 0 | total_Y == total_Z |
                (n_resp - n_na) == 1L | max_sd_y_psu == 0)]
tab_variables[, .N, keyby = .(flag)]

tab_variables[("R5_LT_D1_emplno")]

# Aggregate up to round and country
tab_variables[, flag := any(flag), by = .(essround, cntry, varname)]
tab_variables[, .N, keyby = .(flag)]


# Number of variables by country, domain, round
dcast.data.table(data = tab_variables[!(flag)],
                 formula = cntry + domain ~ paste0("R", essround),
                 fun.aggregate = length)

save(tab_variables, file = "data/tab_variables.Rdata")


# List of extended variable names to be used in the ICC estimation
varname_list <- tab_variables[(b > 1) & !(flag), varname_ext]
length(varname_list)

"R5_LT_D1_emplno" %in% varname_list


# Estimate ICC ####

# R package
#
# ICC: Facilitating Estimation of the Intraclass Correlation Coefficient
#
# Assist in the estimation of the Intraclass Correlation Coefficient (ICC) from
# variance components of a one-way analysis of variance and also estimate the
# number of individuals or groups necessary to obtain an ICC estimate with a
# desired confidence interval width.
#
# https://cran.r-project.org/package=ICC
# https://github.com/matthewwolak/ICC

# Why is the equation for Intra-class correlation aligned with design effect of
# cluster sampling?
# https://stats.stackexchange.com/q/436816/3330

# Methods in Sample Surveys
# Cluster Sampling
# https://ocw.jhsph.edu/courses/StatMethodsForSampleSurveys/PDFs/Lecture5.pdf


# Order by
setkey(dat3, varname_ext, PSU)


# Test - compare ICC with samplesize4surveys

# set.seed(1)
# tmp <- sort(sample(varname_list, 10))
# dat3[(tmp), ICC::ICCbare(x = factor(PSU), y = lin_val, data = .SD),
#      keyby = .(varname_ext)]
# dat3[(tmp), samplesize4surveys::ICC(y = lin_val, cl = PSU),
#      keyby = .(varname_ext)]
#
# tmp <- c("R6_IS_D1_dscrna", "R6_IS_D1_dscrref", "R6_IS_D1_pdjobev",
#          "R9_HU_D1_ctzcntr", "R9_PT_D1_dscrdsb")
# dat3[(tmp), ICC::ICCbare(x = factor(PSU), y = lin_val, data = .SD),
#      keyby = .(varname_ext)]
# dat3[(tmp), samplesize4surveys::ICC(y = lin_val, cl = PSU),
#      keyby = .(varname_ext)]

# tab_variables[varname_ext == "R8_HU_D2_wkhtotp"]
# dat3[varname_ext == "R8_HU_D2_wkhtotp"][order(value)]

estimICC <- function(x) {
  cat(which(x == varname_list), "/", length(varname_list), ":", x, "\n")
  data.table(varname_ext = x,
             ICC1 = max(0, ICC::ICCbare(x = factor(PSU),
                                        y = value,
                                        data = dat3[(x)][!is.na(value)])),
             ICC2 = max(0, ICC::ICCbare(x = factor(PSU),
                                        y = lin_val,
                                        data = dat3[(x)])))
}

estimICC(sample(varname_list, 1))
# estimICC("R5_LT_D1_emplno")

dat3[("R5_LT_D1_emplno"), .N, keyby = .(value, value_y, value_z)]
dat3[("R5_LT_D1_emplno")][!is.na(value), .N,
                          keyby = .(PSU, value, value_y, value_z)]


# # ICC testing
# # The case of ESS8 CZ data
#
# # Load ICC estimates received from Peter
# ICC.est.test <- fread(file = "data/ESS8CZ/ESS8-CZ-ICC-Peter.txt", sep = " ",
#                       drop = c(1, 2, 6), blank.lines.skip = T, na.strings = ".")
#
# # x <- "R9_LV_D1_happy"
#
# estimICC_test <- function(x) {
#   cat("TEST:", x, "\n")
#
#   tab.anova <- anova(aov(value ~ PSU, data = dat3[(x)][!is.na(value)]))
#   MeanSq <- tab.anova$`Mean Sq`
#
#   psu.size  <- dat3[(x)][!is.na(value), .N, by = .(PSU)][, N]
#   psu.count <- length(psu.size)
#
#   var.a <- (MeanSq[1] - MeanSq[2]) /
#     ((1 / (psu.count - 1)) * (sum(psu.size) - (sum(psu.size ^ 2) / sum(psu.size))))
#
#   data.table(varname_ext = x,
#              ICC1 = max(0, ICC::ICCbare(x = factor(PSU),
#                                         y = value,
#                                         data = dat3[(x)][!is.na(value)])),
#              ICC2 = max(0, ICC::ICCbare(x = factor(PSU),
#                                         y = lin_val,
#                                         data = dat3[(x)])))
# }
#
# estimICC_test("R8_CZ_D1_aesfdrk")
# # 0.1403413
#
# estimICC_test(sample(varname_list, 1))
#
# ICC.est <- rbindlist(lapply(grep("R8_CZ", varname_list, value = T),
#                             estimICC_test))
# ICC.est
#
# ggplot(data = ICC.est, mapping = aes(ICC1, ICC2)) +
#   geom_point() +
#   geom_text(mapping = aes(label = substring(varname_ext, 10)),
#             hjust = 1, vjust = 0, colour = "grey") +
#   geom_abline(intercept = 0, slope = 1, colour = "red") +
#   ggtitle("ICC estimates for R8 CZ") +
#   theme_bw()
#
# dat3[("R8_CZ_D1_emplno"), .N, keyby = .(value, value_y, value_z)]
#
# dat_test <- merge(tab_variables[cntry == "CZ" & essround == 8], ICC.est,
#                   by = "varname_ext", all.x = T)
# dat_test <- merge(dat_test, ICC.est.test, by = c("cntry", "varname"), all.x = T)
#
# dat_test[, test1 := abs(ICC1 - roh) < 1e-6]
# dat_test[, test2 := abs(ICC2 - roh) < 1e-6]
#
# dat_test[, .N, keyby = .(flag, test1, test2)]
#
# dat_test[!(test1), .(varname_ext, n_na, ICC1, ICC2, roh)]
#
# dat_test[is.na(test1)]
#
# dat_test[total_Z == pop_size][order(abs(ICC1 - roh))]
#
#
# # # Save
# # write.xlsx(dat_test, file = "results/ESS8_CZ_dat_deff.xlsx",
# #            colWidths = "auto", firstRow = T,
# #            headerStyle = createStyle(textDecoration = "italic",
# #                                      halign = "center"))



# Real estimation for all rounds and countries
gc()
t1 <- Sys.time()
dat_ICC <- lapply(varname_list, estimICC)
t2 <- Sys.time()
t2 - t1
# Time difference of 42 mins

# Options
options(warn = 1)

dat_ICC[1:3]

dat_ICC <- rbindlist(dat_ICC, use.names = T, fill = T)

setkey(dat_ICC, varname_ext)

save(dat_ICC, file = "data/dat_ICC.Rdata")


# Load

load("data/dat_ICC.Rdata")

dat_ICC

dat_ICC[, summary(ICC1)]
dat_ICC[, summary(ICC2)]

dat_ICC[is.na(ICC1)]
dat_ICC[is.na(ICC2)]


dat_ICC[, essround := substring(varname_ext, 1, 2)]
dat_ICC[, cntry    := substring(varname_ext, 4, 5)]
dat_ICC[, domain   := substring(varname_ext, 7, 8)]
dat_ICC[, varname  := substring(varname_ext, 10)]


pl0 <- ggplot(data = dat_ICC, mapping = aes(x = ICC1, y = ICC2)) +
  geom_point(alpha = .5) +
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  ggtitle("Intraclass correlation coefficient (ICC or ρ)") +
  theme_bw()

pl1 <- ggplot(data = dat_ICC, mapping = aes(x = ICC1, y = ICC2)) +
  geom_point(alpha = .5) +
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  facet_wrap(~ essround) +
  ggtitle("Intraclass correlation coefficient (ICC or ρ)") +
  theme_bw()

pl2 <- ggplot(data = dat_ICC, mapping = aes(x = ICC1, y = ICC2, colour = domain)) +
  geom_point(alpha = .5) +
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  facet_wrap(~ cntry) +
  ggtitle("Intraclass correlation coefficient (ICC or ρ)") +
  theme_bw()

pl3 <- ggplot(data = dat_ICC, mapping = aes(x = ICC1, y = ICC2)) +
  geom_point(alpha = .5) +
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  facet_wrap(~ varname) +
  ggtitle("Intraclass correlation coefficient (ICC or ρ)") +
  theme_bw()

cairo_pdf(filename = "results/ICC1_ICC2.pdf", width = 16, height = 9, onefile = T)
pl0
pl1
pl2
pl3
dev.off()

fwrite(x = dat_ICC, file = "results/ICC1_ICC2.csv")

dat_ICC[("R9_PL_D2_emplno")]

tab <- dat3[("R9_PL_D2_emplno"), .(n = .N),
            keyby = .(varname_ext, lin_val, value, value_y, value_z)]
tab[, p := prop.table(n)]
tab

dat3[("R9_PL_D2_emplno")][!is.na(value), .(n = .N),
     keyby = .(varname_ext, PSU, lin_val, value, value_y, value_z)]
