# ESS survey data ####

# Options ####
options(encoding = "UTF-8")
options(max.print = 10e3)
options(datatable.integer64 = "character")
options(datatable.keepLeadingZeros = TRUE)


# Packages
require(data.table)
require(openxlsx)
require(essurvey)


# Reset ####
rm(list = ls())
gc()


# Load all ESS survey data ####
set_email("martins.liberts+ess@gmail.com")

dat <- import_all_rounds(format = "spss")

length(dat)
sapply(dat, class)

# Combine all round data files into one data file
dat <- rbindlist(dat, use.names = T, fill = T)
class(dat)


# Load 75 variable names to be used for ICC estimation
# Taken from the

# DELIVERABLE NUMBER: 3.6
# DELIVERABLE TITLE: Report on Sample Quality for Round 8
# WORK PACKAGE Number: 3
# SUBMITTED BY: University of Essex
# AUTHOR(S): Peter LYNN, University of Essex
# page 9
# Appendix: Variables used for estimating the intra-cluster correlation, ρ

variables <- read.xlsx(xlsxFile = "variables/ICC-variables.xlsx")
setDT(variables)

variables <- melt(data = variables, measure.vars = names(variables),
                  na.rm = T)

setnames(variables, c("type", "varname"))

variables[, varname := tolower(varname)]
variables

variables[, is.available := varname %in% names(dat)]
variables[, .N, keyby = .(is.available)]


# Check values for the target variables

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

# Write out table about the target variables
write.xlsx(variables, file = "results/variables.xlsx",
           colWidths = "auto", firstRow = T,
           headerStyle = createStyle(textDecoration = "italic",
                                     halign = "center"))

fwrite(variables, file = "tables/variables.csv", quote = T)


# intersect(names(dat), variables$varname)
# head(names(dat), 10)
# tail(names(dat), 10)


# Necessary variables
varnames.design <- c("essround", "edition", "proddate", "cntry", "idno",
                     "dweight", "pspwght", "pweight", "anweight")


# Keep only necessary variables
dat <- dat[, c(varnames.design, variables$varname), with = F]

# dim(dat)

# Check the edition and production dates
dat[, .N, keyby = .(essround, edition, proddate)]

# # Test
# dat[, .N]
# dat[, .N, keyby = .(cntry)]


# Remove all extra attributes (from the SPSS data file)
str(dat)
dat <- haven::zap_formats(dat)
dat <- haven::zap_label(dat)
dat <- haven::zap_labels(dat)
dat <- haven::zap_missing(dat)
dat <- haven::zap_widths(dat)
str(dat)


# Production date
dat[, proddate := lubridate::dmy(proddate)]
dat[, .N, keyby = proddate]


# Convert country code to character
dat[, cntry := as.character(cntry)]
dat[, .N, keyby = cntry]

# Number of respondents by country and round
dcast.data.table(data = dat, formula = cntry ~ paste0("R", essround),
                 fun.aggregate = length)


# Save data files for the next step
save(dat, file = "data/dat.Rdata")
save(variables, file = "data/variables.Rdata")
