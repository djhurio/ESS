# ESS data ####

# Subselect of countries
# In my case:
# CY - Cyprus
# ES - Spain
# FI - Finland
# LT - Lithuania
# LV - Latvia
# NO - Norway
# RS - Serbia


# Options ####
options(encoding = "UTF-8")
options(stringsAsFactors = F)


# Packages
require(data.table)
require(lubridate)
require(haven)
require(countrycode)
require(rvest)
require(stringr)
require(openxlsx)

# devtools::install_github("cimentadaj/ess")
require(ess)


# Reset ####
rm(list = ls())
gc()


# Codes for selected countries
country.codes <- c("CY", "ES", "FI", "LT", "LV", "NO", "RS")


# Names for selected countries
data("codelist")
setDT(codelist)
country.names <- codelist[iso2c %in% country.codes, country.name.en]
country.names


# Country names have participated in ESS before
country.names.down <- intersect(country.names, show_countries())
country.names.down

tab1 <- sapply(country.names.down, show_country_rounds) %>%
  lapply(data.table) %>%
  rbindlist(idcol = "country") %>%
  dcast(country ~ V1, fun.aggregate = length, value.var = "V1")
tab1

write.xlsx(tab1, file = "results/ESS_country_rounds.xlsx")
fwrite(tab1, file = "tables/ESS_country_rounds.csv")


# Load ESS data ####

dat <- lapply(country.names.down, ess_all_cntrounds,
              your_email = "martins.liberts@gmail.com", format = "spss")
length(dat)

dat <- Reduce(c, dat)
length(dat)

dat <- rbindlist(dat, fill = T)
class(dat)


# Test
dat[, .N]
str(dat)

str(dat$prtvtcfi)
attr(dat$prtvtcfi, "label")
attr(dat$prtvtcfi, "labels")

str(dat$prtvtbis)
attr(dat$prtvtbis, "label")
attr(dat$prtvtbis, "labels")

# dat[, .(proddate, dmy(proddate))]
dat[, proddate := dmy(proddate)]
dat[, .N, keyby = proddate]

dat[, .N, keyby = cntry]
dat[, cntry := as.character(cntry)]


# SDDF data ####

# Function to create list of download URLs

get.url.list <- function(url.name) {
  html.doc <- read_html(url.name)

  url.list <- html_nodes(html.doc, "a") %>% html_attr(name = "href")
  SDDF.url.list <- grep("SDDF", url.list, value = T)
  SDDF.url.list

  regex.string <- paste(paste0("c=", country.codes), collapse = "|")
  regex.string

  root <- "http://www.europeansocialsurvey.org/file"

  file.list <- grep(regex.string, SDDF.url.list, value = T)
  file.list <- gsub("download.html?file", "download?f", file.list, fixed = T)
  file.list <- gsub("SDDF&", "SDDF.sas.zip&", file.list, fixed = T)
  file.list <- paste0(root, file.list)
  file.list
}

html.urls <- paste0("http://www.europeansocialsurvey.org/data/download.html?r=", 1:6)
html.urls

file.url.list <- lapply(html.urls, get.url.list)
file.url.list

# str_extract(file.list, "[A-z0-9_]*SDDF.sas.zip")
# file.url <- file.list[1]

# Function to download and extract files
get.data <- function(file.url, dir.data) {
  # Print download link
  cat(file.url, "\n\n")

  # File name
  f <- str_extract(file.url, "ESS.*zip")

  # Download
  current_file <- httr::GET(file.url, httr::progress())

  # Write as a .zip file
  writeBin(httr::content(current_file, "raw"), file.path(dir.data, f))

  # Unzip
  utils::unzip(file.path(dir.data, f), exdir = dir.data)
}

dir.data <- "data/SDDF"

lapply(unlist(file.url.list), get.data, dir.data = dir.data)

# table(substr(list.files(dir.data, pattern = "zip$|por$"), 1, 7))


# Import SDDF data files

list.files(dir.data, "sas7bdat$", full.names = T)

datSDDF1_6 <- sapply(list.files(dir.data, "sas7bdat$", full.names = T),
                     read_sas, simplify = F)

lapply(datSDDF1_6, setDT)
lapply(datSDDF1_6, function(x) setnames(x, tolower(names(x))))

lapply(datSDDF1_6, `[`, j = .N, keyby = cntry)

datSDDF1_6 <- rbindlist(datSDDF1_6, fill = T, idcol = "name")

class(datSDDF1_6)

datSDDF1_6[, .N, keyby = cntry]

datSDDF1_6[, .N, keyby = str_extract(toupper(name), "ESS[1-6]")]

datSDDF1_6[, essround := str_extract(toupper(name), "ESS[1-6]")]
datSDDF1_6[, essround := as.integer(str_extract(essround, "[1-6]"))]

datSDDF1_6[, .N, keyby = essround]

sapply(datSDDF1_6, class)

datSDDF1_6[, domain := 1L]

datSDDF1_6 <- datSDDF1_6[, .(essround, cntry, idno, domain, stratify, psu, prob)]



# SDDF - ESS7 (2014) ####
# There is only integrated SDDF file

dir.data.7 <- "data/ESS7"

get.data(file.url = "http://www.europeansocialsurvey.org/file/download?f=ESS7SDDFe01_1.spss.zip&c=&y=2014",
         dir.data = dir.data.7)

# Import SPSS data file
datSDDF7 <- read_sav(list.files(dir.data.7, "sav$", full.names = T))
setDT(datSDDF7)
class(datSDDF7)

datSDDF7[, .N, keyby = cntry]
datSDDF7[, cntry := as.character(cntry)]

datSDDF7 <- datSDDF7[cntry %in% country.codes]
datSDDF7[, .N, keyby = cntry]

datSDDF7 <- datSDDF7[, .(essround, cntry, idno, domain, stratify, psu, prob)]




# Combine rounds 1 - 7

datSDDF <- rbindlist(list(datSDDF1_6, datSDDF7), use.names = T)

datSDDF[, .N, keyby = stratify]
datSDDF[stratify == "NA", stratify := "0"]
datSDDF[, stratify := as.integer(stratify)]
datSDDF[, .N, keyby = stratify]

datSDDF[, lapply(.SD, class)]



# Save ####

save(dat, file = "data/dat.Rdata")
save(datSDDF, file = "data/datSDDF.Rdata")
