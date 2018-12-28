# ESS SDDF data ####

# Options ####
options(encoding = "UTF-8")
options(stringsAsFactors = F)


# Packages
require(data.table)
require(lubridate)
require(haven)
require(rvest)
require(stringr)
require(essurvey)


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

# Function to create list of download URLs
get.url.list <- function(url.name, format = "sas") {
  html.doc <- read_html(url.name)

  url.list <- html_nodes(html.doc, "a") %>% html_attr(name = "href")
  SDDF.url.list <- grep("SDDF", url.list, value = T)

  root <- "http://www.europeansocialsurvey.org/file"

  file.list <- SDDF.url.list
  file.list <- sub("download.html?file", "download?f", file.list, fixed = T)
  file.list <- sub("&", paste0(".", format, ".zip&"), file.list, fixed = T)
  file.list <- paste0(root, file.list)
  file.list
}

html.urls <- paste0("http://www.europeansocialsurvey.org/data/download.html?r=",
                    show_rounds())
html.urls

file.url.list <- lapply(html.urls, get.url.list)
file.url.list <- unlist(file.url.list)
file.url.list


# Function to download and extract files
get.data <- function(file.url, dir.data) {
  # Print download link
  cat(file.url, "\n\n")

  # Remove any existing pdf or zip files
  file.remove(list.files(dir.data, pattern = "pdf$|zip$", full.names = T))

  # File name
  f <- str_extract(file.url, "ESS.*zip")

  # Download
  current_file <- httr::GET(file.url, httr::progress())

  # Write as a .zip file
  writeBin(httr::content(current_file, as = "raw"), file.path(dir.data, f))

  # Unzip
  utils::unzip(file.path(dir.data, f), exdir = dir.data)
}

dir.data <- "data/SDDF"

lapply(file.url.list, get.data, dir.data = dir.data)

# table(substr(list.files(dir.data, pattern = "zip$|por$"), 1, 7))


# Import SDDF data files

list.files(dir.data, "sas7bdat$", full.names = T)
list.files(dir.data, "por$", full.names = T)

datSDDF <- sapply(list.files(dir.data, "sas7bdat$", full.names = T),
                  read_sas, encoding = "UTF-8", simplify = F)

lapply(datSDDF, setDT)
lapply(datSDDF, function(x) setnames(x, tolower(names(x))))
lapply(datSDDF, function(x) if ("stratify" %in% names(x)) setnames(x, "stratify", "stratum"))

lapply(datSDDF, `[`, j = .N, keyby = cntry)

datSDDF <- rbindlist(datSDDF, fill = T, idcol = "name")

class(datSDDF)

datSDDF[, .N, keyby = cntry]
datSDDF <- datSDDF[cntry != ""]

datSDDF[, .N, keyby = str_extract(toupper(name), "ESS[1-9]")]

datSDDF[, essround := str_extract(toupper(name), "ESS[1-9]")]
datSDDF[, essround := as.integer(str_extract(essround, "[1-9]"))]

datSDDF[, .N, keyby = essround]

sapply(datSDDF, class)


datSDDF[, .N, keyby = .(round(domain))]
datSDDF[is.na(domain), domain := 0L]
datSDDF[, .N, keyby = .(round(domain))]



datSDDF <- datSDDF[, .(essround, cntry, idno, domain, stratum, psu, prob)]

datSDDF[, lapply(.SD, class)]


# Save ####

save(datSDDF, file = "data/datSDDF.Rdata")
