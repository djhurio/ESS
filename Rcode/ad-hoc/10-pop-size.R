# Population size
# Eurostat

# All persons aged 15 and over (no upper age limit) resident within private
# households in each country, regardless of their nationality, citizenship or language

# Noklusētās MND R opcijas
options(encoding = "UTF-8")
options(stringsAsFactors = F)
options(max.print = 10e3)
options(datatable.integer64 = "character")
options(datatable.keepLeadingZeros = TRUE)


# Packages
require(data.table)
require(eurostat)
require(openxlsx)


# Reset
rm(list = ls())
gc()


# Get data

pop <- get_eurostat(id = "demo_pjan")
setDT(pop)

pop[, .N, keyby = .(unit)]
pop[, .N, keyby = .(age)]
pop[, .N, keyby = .(sex)]
pop[, .N, keyby = .(time)]

pop[, .N, keyby = .(age)]
pop[age == "Y_LT1", age_numeric := 0L]
pop[grep("^Y[0-9]{1,2}$", age), age_numeric := as.integer(substring(age, 2))]
pop[, .N, keyby = .(age_numeric, age)]

tab <- pop[grepl("^[A-Z]{2}$", geo) & sex == "T" & time == max(time) & age_numeric >= 15L,
           sum(values), keyby = .(time, sex, geo)]


write.xlsx(x = tab, file = "results/pop-size-recent.xlsx")
