# ICC estimation ####

# Options
options(encoding = "UTF-8")
options(max.print = 10e3)
options(datatable.integer64 = "character")
options(datatable.keepLeadingZeros = TRUE)
options(warn = 2)

# Packages
require(essurvey)
require(data.table)
require(openxlsx)

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
tab_psu <- dat3[, .(n = .N, sd_y_psu = sd(value_y)), keyby = .(varname_ext, PSU)]
tab_psu[n == 1L, sd_y_psu := 0]
tab_psu <- tab_psu[, .(max_sd_y_psu = max(sd_y_psu)), keyby = .(varname_ext)]
tab_psu[max_sd_y_psu == 0]

# Summary table
tab_variables <- dat3[, .(n_resp = .N,
                          n_na = sum(is.na(value)),
                          sd_y = sd(value_y),
                          sd_z = sd(value_z),
                          pop_size = sum(weight_est),
                          total_Y  = sum(value_y * weight_est),
                          total_Z  = sum(value_z * weight_est)),
                      keyby = .(varname_ext, essround, cntry, domain, varname)]

tab_variables <- merge(tab_variables, dat_b,
                       by = c("essround", "cntry", "domain"), all.x = T)

tab_variables <- merge(tab_variables, tab_psu,
                       by = c("varname_ext"), all.x = T)

tab_variables[total_Z > 0, ratio := total_Y / total_Z]


# Cases where esimtation is not possible
tab_variables[n_na == n_resp]
tab_variables[n_resp - n_na == 1L]
tab_variables[n_na < n_resp & total_Y == 0]
tab_variables[n_na < n_resp & total_Y > 0 & total_Y == total_Z]
tab_variables[sd_y == 0]
tab_variables[max_sd_y_psu == 0]

# Mark varibales where estimation of effective sample size is not possible:
# 1) variable is a constant (sd_y == 0)
# 2) mean estimate is 1 (total_Y == total_Z)
# 3) There is only one respondent ((n_resp - n_na) == 1L)
# 4) There is no variance in PSUs (max_sd_y_psu == 0)
tab_variables[, flag := b > 1 & (sd_y == 0 | total_Y == total_Z |
                (n_resp - n_na) == 1L | max_sd_y_psu == 0)]
tab_variables[, .N, keyby = .(flag)]

# Aggregate up to round and country
tab_variables[, flag := any(flag), by = .(essround, cntry, varname)]
tab_variables[, .N, keyby = .(flag)]
#     flag     N
# 1: FALSE 13227
# 2:  TRUE   948


tab_variables[!(flag) & b > 1 & max_sd_y_psu == 0, .N, keyby = .(varname_ext)]

# Number of variables by country, domain, round
dcast.data.table(data = tab_variables[!(flag)],
                 formula = cntry + domain ~ paste0("R", essround),
                 fun.aggregate = length)

save(tab_variables, file = "data/tab_variables.Rdata")


# List of extended variable names to be used in claculations
varname_list <- tab_variables[!(flag) & b > 1, varname_ext]
length(varname_list)

head(varname_list)
tail(varname_list)


# Estimate ICC ####

# Linearized variables
dat3

# # PSU size
# dat3[, PSU_size := .N, by = .(varname_ext, PSU)]
# dat3[, .N, keyby = .(PSU_size)]
#
# dat3[, max_PSU_size := max(PSU_size), by = .(varname_ext)]
#
# dat3[max_PSU_size == 1, .N, keyby = .(essround, cntry, domain)]

# tmp <- dat3[(varname_list), sd(lin_val), keyby = .(varname_ext)]
# tmp[order(V1)]

# # Test
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
  dat3[(x), .(ICC = max(0, ICC::ICCbare(x = factor(PSU), y = lin_val))),
       by = .(varname_ext)]
}

# estimICC(varname_list[1])

# options(warn = 1)
# estimICC("R1_CZ_D1_dscretn")
# dat3[("R1_CZ_D1_dscretn"), .N, keyby = .(value, value_y, value_z)]
# dat3[("R1_CZ_D1_dscretn")][value_y == 1]
#
# options(warn = 1)
# estimICC("R1_CZ_D1_dscrrlg")
# dat3[("R1_CZ_D1_dscrrlg"), .N, keyby = .(value, value_y, value_z)]
# dat3[("R1_CZ_D1_dscrrlg")][value_y == 1]
# dat3[("R1_CZ_D1_dscrrlg")][, sd(lin_val), keyby = .(PSU)][!is.na(V1)][order(V1)]
#
# options(warn = 1)
# estimICC("R6_HU_D1_dscrsex")
# dat3[("R6_HU_D1_dscrsex"), .N, keyby = .(value, value_y, value_z)]
# dat3[("R6_HU_D1_dscrsex")][value_y == 1]
#
# options(warn = 1)
# estimICC("R6_IS_D1_dscrntn")
# dat3[("R6_IS_D1_dscrntn"), .N, keyby = .(value, value_y, value_z, lin_val)]
# dat3[("R6_IS_D1_dscrntn")][value_y == 1]
# dat3[("R6_IS_D1_dscrntn")][, sd(lin_val), keyby = .(PSU)][!is.na(V1)][order(V1)]

# ICC testing
# The case of ESS8 CZ data

# Load ICC estimates received from Peter
ICC.est.test <- fread(file = "data/ESS8-CZ-ICC-Peter.txt", sep = " ",
                      drop = c(1, 2, 6), blank.lines.skip = T, na.strings = ".")
str(ICC.est.test)

ICC.est <- rbindlist(lapply(grep("R8_CZ", varname_list, value = T), estimICC))
ICC.est

dat_test <- merge(tab_variables[cntry == "CZ" & essround == 8], ICC.est,
                  by = "varname_ext", all.x = T)
dat_test <- merge(dat_test, ICC.est.test, by = c("cntry", "varname"), all.x = T)

dat_test[, diff := ICC - roh]
dat_test[, abs_diff := abs(diff)]

dat_test[, test := abs_diff < 1e-6]
dat_test[, .N, keyby = .(test)]

dat_test[(test)]
dat_test[!(test)]

dat_test[is.na(test)]
dat_test[!is.na(test)][order(abs(diff))]

ggplot(data = dat_test[!is.na(test)], mapping = aes(x = n_na, y = diff)) +
  geom_point()

dat_test[total_Z == pop_size][order(abs_diff)]

setorder(dat_test, abs_diff)


# Save
write.xlsx(dat_test, file = "results/ESS8_CZ_dat_deff.xlsx",
           colWidths = "auto", firstRow = T,
           headerStyle = createStyle(textDecoration = "italic",
                                     halign = "center"))




gc()
t1 <- Sys.time()
dat_ICC <- lapply(varname_list, estimICC)
t2 <- Sys.time()
t2 - t1
# Time difference of 42 mins

# warnings()

dat_ICC[1:3]

dat_ICC <- rbindlist(dat_ICC, use.names = T, fill = T)
save(dat_ICC, file = "data/dat_ICC.Rdata")

load("data/dat_ICC.Rdata")

dat_ICC
dat_ICC[, summary(ICC)]
dat_ICC[is.na(ICC)]


# Options
options(warn = 1)
