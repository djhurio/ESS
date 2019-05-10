# ESS data ####

# Options ####
options(encoding = "UTF-8")
options(stringsAsFactors = F)
options(max.print = 10e3)
options(datatable.integer64 = "character")
options(datatable.keepLeadingZeros = TRUE)


# Packages
require(data.table)
require(openxlsx)
require(lubridate)
require(essurvey)


# Reset ####
rm(list = ls())
gc()


# Load ESS data ####
set_email("martins.liberts@gmail.com")


# ESS data ####

show_rounds()

dat <- import_all_rounds(format = "spss")
length(dat)

sapply(dat, class)

dat <- rbindlist(dat, fill = T)
class(dat)



# Variables ####

variables <- lapply(list.files("variables", full.names = T), read.table)
names(variables) <- list.files("variables")

variables <- rbindlist(variables, idcol = "file")
setnames(variables, "V1", "varname")
variables[, varname := tolower(varname)]
variables

variables[, is.available := varname %in% names(dat)]
variables[, .N, keyby = .(is.available)]

foo <- function(x) {
  if (x %in% names(dat)) {
    paste(sort(head(unique(dat[[x]]), n = 10)), collapse = ", ")
  } else {
    NA_character_
  }
}

foo("vote")
foo("asd")

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
                     "dweight", "pspwght", "pweight", "proddate")

dat1 <- dat[, c(varnames.design,
                intersect(names(dat), variables$varname)), with = F]

dim(dat)
dim(dat1)

dat <- dat1

rm(dat1)
gc()





# Test
dat[, .N]

dat[, .N, keyby = .(cntry)]

str(dat)

# dat[, .(proddate, dmy(proddate))]
dat[, proddate := dmy(proddate)]
dat[, .N, keyby = proddate]

dat[, .N, keyby = cntry]
dat[, cntry := as.character(cntry)]

dat[, .N, keyby = .(cntry, essround)]


# Save ####

save(dat, file = "data/dat.Rdata")
