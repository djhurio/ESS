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

# Round labels
round.labels <- paste0("R", sprintf("%02d", show_rounds()))
round.labels

# Import data files from all rounds (SPSS data format is used)
dat <- import_all_rounds(format = "spss")

length(dat)
sapply(dat, class)


# Save names of the variables
dat.names <- lapply(dat, names)
sapply(dat.names, length)


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

variables$varname

# Mark which variables are available in each round
variables[, c(round.labels) := lapply(dat.names,
                                      function(x) (variables$varname %in% x))]

variables[, is.available := all(unlist(.SD)), .SDcols = round.labels,
          by = .(varname)]
variables[, .N, keyby = .(is.available)]
variables[!(is.available)]
# Some of the 75 variables are not available in all rounds
# ICC will be assumed to be 0 in rounds when a variable is not available



# Variable selection to reduce the size of a data.table

# Survey design variables and weights
varnames.design <- c("essround", "edition", "proddate", "cntry", "idno",
                     "dweight", "pspwght", "pweight", "anweight")


# Helper function to subselect necessary variables
foo <- function(x) {
  name_sel <- intersect(names(x), c(varnames.design, variables$varname))
  x[, c(name_sel), with = F]
}

# Keep only necessary variables
dat <- lapply(dat, foo)

# dat <- dat[, c(varnames.design, variables$varname), with = F]
# # dim(dat)
# gc()



# Combine data from all rounds in one data.table
dat <- rbindlist(dat, use.names = T, fill = T)
class(dat)
gc()


# Check values for the target variables

# Function returns a character of max 10 smalles values from a variable
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

fwrite(dat, file = "data-pub/dat.csv")
