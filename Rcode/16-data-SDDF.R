# ESS SDDF data ####

# Options ####
options(encoding = "UTF-8")
options(stringsAsFactors = F)


# Install the branch version with this:
# remotes::install_github("ropensci/essurvey", ref = "sddf",
#                         quiet = TRUE, upgrade = 'always')

# Packages
require(data.table)
require(lubridate)
require(haven)
require(foreign)
require(rvest)
require(stringr)
require(essurvey)
require(openxlsx)

# Reset ####
rm(list = ls())
gc()



# ESS data ####
set_email("martins.liberts@gmail.com")

# Authentification to the ESS website
# store your e-mail address in a list to be passed to the website
values <- list(u = Sys.getenv("ESS_EMAIL"))
url_login <- "http://www.europeansocialsurvey.org/user/login"
# authenticate on the ess website
authen <- httr::POST(url_login, body = values)
check_authen <- httr::GET(url_login, query = values)
authen_xml <- xml2::read_html(check_authen)
error_node <- xml2::xml_find_all(authen_xml, '//p [@class="error"]')


# SDDF data ####

root.url <- "https://www.europeansocialsurvey.org"

# Get list of SDDFs
html.doc <- read_html(paste0(root.url, "/data/download_sample_data.html"))
url.list <- html_nodes(html.doc, "a") %>% html_attr(name = "href")
SDDF.url.list <- sort(paste0(root.url, grep("SDDF", url.list, value = T)))
length(SDDF.url.list)


# Get list of ZIP files
get.zip.urls <- function(url) {
  html.doc <- read_html(url)
  url.list <- html_nodes(html.doc, "a") %>% html_attr(name = "href")
  paste0(root.url, grep("zip", url.list, value = T))
}

# get.zip.urls(SDDF.url.list[1])
zip.urls <- sort(unlist(lapply(SDDF.url.list, get.zip.urls)))
length(zip.urls)


# Download & extract zips

# Function to download and extract files
get.data <- function(file.url, dir.data) {
  # Print download link
  cat(file.url, "\n\n")

  # Remove any existing pdf or zip files
  # file.remove(list.files(dir.data, pattern = "pdf$|zip$", full.names = T))

  # File name
  f <- str_extract(file.url, "ESS.*zip")

  # Download
  current_file <- httr::GET(file.url, httr::progress())

  # Write as a .zip file
  writeBin(httr::content(current_file, as = "raw"), file.path(dir.data, f))

  # Unzip
  utils::unzip(zipfile = file.path(dir.data, f), exdir = dir.data)

  # Delete zip
  file.remove(file.path(dir.data, f))
}

dir.data <- "data/SDDF"

# Delete all data files
list.files(path = dir.data, full.names = T, recursive = T)
file.remove(list.files(path = dir.data, full.names = T, recursive = T))
list.files(path = dir.data, full.names = T, recursive = T)

lapply(zip.urls, get.data, dir.data = dir.data)


# Delete pdf and txt files
list.files(path = dir.data, pattern = "pdf$|txt$",
           full.names = T, recursive = T)
file.remove(list.files(path = dir.data, pattern = "pdf$|txt$",
                       full.names = T, recursive = T))
list.files(path = dir.data, pattern = "pdf$|txt$",
           full.names = T, recursive = T)


# File names
fnames <- data.table(file.name = toupper(list.files(path = dir.data)))
fnames[, file.name := gsub("-", "_", file.name)]
fnames[, c("name", "ext") := tstrsplit(file.name, split = "\\.")]
fnames <- dcast.data.table(data = fnames, formula = name ~ ext,
                           fun.aggregate = length, value.var = "file.name")
write.xlsx(x = fnames, file = "results/SDDF-file-names.xlsx", firstRow = T)


# Import SDDF data files

length(list.files(dir.data, "\\.sas7bdat$", full.names = T))
length(list.files(dir.data, "\\.sav$", full.names = T))
length(list.files(dir.data, "\\.por$", full.names = T))
length(list.files(dir.data, "\\.dta$", full.names = T))
length(list.files(dir.data, "\\.dat$", full.names = T))

datSDDF.por <- sapply(list.files(dir.data, "^...[1-4].*por$",
                                 full.names = T),
                      foreign::read.spss, use.value.labels = F, simplify = F)

datSDDF.sav <- sapply(list.files(dir.data, "^...[578].*sav$",
                                 full.names = T),
                      foreign::read.spss, use.value.labels = F, simplify = F)

datSDDF.sas <- sapply(list.files(dir.data, "^...[6].*sas7bdat$",
                                 full.names = T),
                      haven::read_sas, encoding = "UTF-8", simplify = F)

datSDDF <- c(datSDDF.por, datSDDF.sav, datSDDF.sas)

lapply(datSDDF, setDT)
lapply(datSDDF, function(x) setnames(x, tolower(names(x))))
lapply(datSDDF, function(x) if ("stratify" %in% names(x))
  setnames(x, "stratify", "stratum"))
datSDDF <- rbindlist(datSDDF, use.names = T, fill = T, idcol = "file.name")

datSDDF[, file.name := sub(".*/", "", file.name)]


#
class(datSDDF)

datSDDF[, .N, keyby = cntry]

datSDDF[order(cntry), .(file.name, cntry)]

datSDDF[, .N, keyby = str_extract(toupper(file.name), "ESS[1-9]")]

datSDDF[, essround := str_extract(toupper(file.name), "ESS[1-9]")]
datSDDF[, essround := as.integer(str_extract(essround, "[1-9]"))]

datSDDF[, .N, keyby = essround]

sapply(datSDDF, class)


datSDDF[, .N, keyby = .(round(domain))]
datSDDF[is.na(domain), domain := 0L]
datSDDF[, .N, keyby = .(round(domain))]



datSDDF <- datSDDF[, .(essround, cntry, idno, domain,
                       stratum, psu, prob, file.name)]

datSDDF[, lapply(.SD, class)]


# Save ####

save(datSDDF, file = "data/datSDDF.Rdata")
