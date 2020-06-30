# ICC testing
# The case of ESS8 CZ data

# Options
options(encoding = "UTF-8")
options(stringsAsFactors = F)
options(max.print = 10e3)
options(datatable.integer64 = "character")
options(datatable.keepLeadingZeros = TRUE)


# Packages
require(data.table)
require(openxlsx)
require(essurvey)
require(ICC)


# Reset
rm(list = ls())
gc()



# Variables ####
variables <- read.xlsx(xlsxFile = "variables/ICC-variables.xlsx")
setDT(variables)

variables <- melt(data = variables, measure.vars = names(variables),
                  na.rm = T)

setnames(variables, c("type", "varname"))

variables[, varname := tolower(varname)]
variables




# Load R8 CZ data

show_countries()
set_email("martins.liberts+ess@gmail.com")

# dat <- import_country(country = "Czechia", rounds = 8, format = "spss")
dat.sddf <- import_sddf_country(country = "Czechia", rounds = 8)
setDT(dat.sddf)




# CSV file prepared with IBM SPSS Statistics
dat <- fread(file = "data/ESS8CZ/ESS8CZ.csv")

dat[, .(crpdwk)]
table(dat$crpdwk, useNA = "ifany")
as.integer(dat$crpdwk)
str(dat$crpdwk)

dat$crpdwk

varnames.design <- c("essround", "cntry", "idno",
                     "dweight", "pspwght", "pweight", "proddate")

dat <- dat[, c(varnames.design,
               intersect(names(dat), variables$varname)), with = F]



# Merge
dat.sddf[, .(idno, domain, stratum, psu)]

dat.sddf[is.na(domain), domain := 1L]
dat.sddf[is.na(stratum)]
dat.sddf[is.na(psu)]

dat.sddf[, PSU := paste(domain, stratum, psu, sep = "_")]

dat <- merge(dat, dat.sddf[, .(idno, PSU)], by = "idno")


# ICC estimation

dat3 <- melt.data.table(data = dat,
                        id.vars = c("essround", "cntry", "idno", "PSU"),
                        measure.vars = intersect(names(dat), variables$varname),
                        variable.name = "varname", variable.factor = F,
                        na.rm = F)

dat3

dat3[is.na(value)]

dat3[, .N, keyby = .(varname)]

dat3[, PSU := factor(PSU)]

ICC.est <- dat3[, .(ICC = ICCbare(x = PSU, y = value)),
                keyby = .(essround, cntry, varname)]

ICC.est[ICC < 0, ICC := 0]


# Load ICC estimates received from Peter

ICC.est.test <- fread(file = "data/ESS8CZ/ESS8-CZ-ICC-Peter.txt", sep = " ",
                      drop = c(1, 2, 6), blank.lines.skip = T, na.strings = ".")
ICC.est.test
str(ICC.est.test)

ICC.est.test[, c(.SD, .(as.numeric(roh)))]
ICC.est.test[, roh := as.numeric(roh)]


# # Load my results
# load("results/ESS_dat_deff.Rdata")
#
#
# dat_deff[essround == 8 & cntry == "CZ"]
#
# ICC.est <- dat_deff[essround == 8 & cntry == "CZ",
#                     .(essround, cntry, variable, ICC,
#                       estim, Y_est, Z_est, pop_size)]


# Merge
dat <- merge(x = ICC.est, y = ICC.est.test,
             by = c("cntry", "varname"), all = T)

dat[, test := round(ICC, 3) == round(roh, 3)]

dat[, .N, keyby = .(test)]

dat[(test)]



# Save
write.xlsx(dat[order(test, varname)],
           file = "results/ESS8_CZ_dat_deff.xlsx",
           colWidths = "auto", firstRow = T,
           headerStyle = createStyle(textDecoration = "italic",
                                     halign = "center"))
