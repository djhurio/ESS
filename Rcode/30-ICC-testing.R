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


# Reset
rm(list = ls())
gc()


# Load ICC estimates received from Peter

ICC.est.test <- fread(file = "data/ESS8-CZ-ICC-Peter.txt", sep = " ",
                      drop = c(2, 6), blank.lines.skip = T, na.strings = ".")
ICC.est.test
str(ICC.est.test)

ICC.est.test[, c(.SD, .(as.numeric(roh)))]
ICC.est.test[, roh := as.numeric(roh)]


# Load my results
load("results/ESS_dat_deff.Rdata")


dat_deff[essround == 8 & cntry == "CZ"]

ICC.est <- dat_deff[essround == 8 & cntry == "CZ",
                    .(essround, cntry, variable, ICC,
                      estim, Y_est, Z_est, pop_size)]


# Merge
dat <- merge(x = ICC.est, y = ICC.est.test,
             by.x = c("cntry", "variable"),
             by.y = c("cntry", "varname"), all = T)

dat[, test := round(ICC, 3) == round(roh, 3)]

dat[, .N, keyby = .(test)]

dat[(test)]

dat[Z_est == pop_size][order(ICC)]

setorder(dat, test, Z_est, ICC)

dat



# Save
write.xlsx(dat, file = "results/ESS8_CZ_dat_deff.xlsx",
           colWidths = "auto", firstRow = T,
           headerStyle = createStyle(textDecoration = "italic",
                                     halign = "center"))
