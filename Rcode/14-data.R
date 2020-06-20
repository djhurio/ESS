# ESS data ####

# Options ####
options(encoding = "UTF-8")
options(max.print = 10e3)
options(datatable.integer64 = "character")
options(datatable.keepLeadingZeros = TRUE)


# Packages
require(haven)
require(data.table)
require(openxlsx)
require(lubridate)
require(essurvey)


# Reset ####
rm(list = ls())
gc()


# Load ESS data ####
set_email("martins.liberts+ess@gmail.com")

dat <- import_all_rounds(format = "spss")

length(dat)
sapply(dat, class)

dat <- rbindlist(dat, use.names = T, fill = T)
class(dat)


# Variables ####
variables <- read.xlsx(xlsxFile = "variables/ICC-variables.xlsx")
setDT(variables)

variables <- melt(data = variables, measure.vars = names(variables),
                  na.rm = T)

setnames(variables, c("type", "varname"))

variables[, varname := tolower(varname)]
variables

variables[, is.available := varname %in% names(dat)]
variables[, .N, keyby = .(is.available)]

foo <- function(x) {
  if (x %in% names(dat)) {
    paste(sort(head(unique(dat[[x]]), n = 10)), collapse = ",")
  } else {
    NA_character_
  }
}

# foo("vote")
# foo("asd")

variables[, values := foo(varname), by = varname]

write.xlsx(variables, file = "results/variables.xlsx",
           colWidths = "auto", firstRow = T,
           headerStyle = createStyle(textDecoration = "italic",
                                     halign = "center"))

fwrite(variables, file = "tables/variables.csv", quote = T)


intersect(names(dat), variables$varname)

head(names(dat), 10)
tail(names(dat), 10)

varnames.design <- c("essround", "cntry", "idno",
                     "dweight", "pspwght", "pweight", "anweight", "proddate")

dat <- dat[, c(varnames.design,
               intersect(names(dat), variables$varname)), with = F]

dim(dat)




# Test
dat[, .N]

dat[, .N, keyby = .(cntry)]


# Remove all extra attributes
str(dat)
dat <- zap_formats(dat)
dat <- zap_label(dat)
dat <- zap_labels(dat)
dat <- zap_missing(dat)
dat <- zap_widths(dat)
str(dat)


# dat[, .(proddate, dmy(proddate))]
dat[, proddate := dmy(proddate)]
dat[, .N, keyby = proddate]

dat[, .N, keyby = cntry]
dat[, cntry := as.character(cntry)]

dcast.data.table(data = dat, formula = cntry ~ paste0("R", essround),
                 fun.aggregate = length)


# Save ####

save(dat, file = "data/dat.Rdata")
save(variables, file = "data/variables.Rdata")
