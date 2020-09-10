# ESS
# Example of weighting and estimation for the ESS data
# http://www.europeansocialsurvey.org/
# https://github.com/SNStatComp/awesome-official-statistics-software


# Options
options(encoding  = "UTF-8")
options(max.print = 10e3)


# Libraries
library(data.table) # data operations
library(essurvey)   # ESS data import
library(survey)     # estimation
library(vardpoor)   # estimation


# Please change to your own e-mail address
set_email(ess_email = "martins.liberts+ess@gmail.com")


# Set round & country ####
round   <- 8
country <- "United Kingdom"


# Show countries for a round
show_rounds_country(rounds = round)

# Show rounds for a country with survey data available
show_country_rounds(country = country)

# Show rounds for a country with SDDF available
show_sddf_cntrounds(country = country)


# Import data ####

# Survey data
data.survey <- import_country(country = country, rounds = round)
setDT(data.survey)

# SDDF data
data.sddf <- import_sddf_country(country = country, rounds = round)
setDT(data.sddf)

# Merge survey and SDDF data
data1 <- merge(data.survey,
               data.sddf[, .(essround, cntry, idno, domain, stratum, psu)],
               by = c("essround", "cntry", "idno"))

# Compute survey weights
data1[, anweight := pspwght * pweight * 10e3]


# Estimation with survey library ####

# Define sample design
design <- svydesign(ids = ~psu, strata = ~stratum, weights = ~anweight,
                    data = data1)

# Values of netusoft
data1[, .N, keyby = .(netusoft)]

# Convert to factor (categorical variable)
data1[, netusoft := factor(netusoft)]

# Estimate proportions
svymean(x = ~netusoft, design = design)


# Summary of netustm
data1[, summary(netustm)]

# Extra variables because of missing values in netustm
data1[, netustm_y := ifelse(!is.na(netustm), netustm, 0)]
data1[, netustm_z := as.integer(!is.na(netustm))]
data1[, as.list(summary(netustm_y)), keyby = .(netustm_z)]

# Estimate mean
svymean(x = ~netustm, design = design, na.rm = T)

# Estimate mean as ratio of two totals
svyratio(numerator = ~netustm_y, denominator = ~netustm_z, design = design)


# Estimation with vadpoor library ####

# netusoft
data1[, .N, keyby = .(netusoft)]

# Dummy variables
data1[, c(paste0("netusoft_y_", 1:5)) := data.table(model.matrix(object = ~ netusoft - 1))]
data1[, c(paste0("netusoft_z_", 1:5)) := 1]

est_netusoft <- vardom(Y = paste0("netusoft_y_", 1:5),
                       Z = paste0("netusoft_z_", 1:5),
                       H = "stratum", PSU = "psu",
                       w_final = "anweight", fh_zero = T,
                       dataset = data1)

est_netusoft$all_result[, .(variable, respondent_count, n_nonzero,
                            pop_size, estim, se)]


# netustm
est_netustm <- vardom(Y = "netustm_y", Z = "netustm_z",
                      H = "stratum", PSU = "psu",
                      w_final = "anweight", fh_zero = T,
                      dataset = data1)

est_netustm$all_result[, .(variable, respondent_count, n_nonzero,
                           pop_size, estim, se)]

