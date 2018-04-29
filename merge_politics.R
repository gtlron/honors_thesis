# Merge FH and UDS data

setwd("~/Desktop/thesis")
fh <- read.csv("fh.csv")
uds <- read.csv("uds_summary.csv")
m <- merge(fh, uds, by=c("cown","year"), all=TRUE, sort = TRUE)
m$country <- countrycode(m$cown, "cown", "country.name", warn = TRUE)
m$iso <- countrycode(m$cown, "cown", "iso3c", warn = TRUE)
write.csv(m, "fh_uds.csv")

polity <- read.csv("polity.csv")
fh <- read.csv("fh_uds.csv")
m <- merge(fh, polity, by=c("cown","year"), all=TRUE, sort = TRUE)
library(countrycode)
m$country <- countrycode(m$cown, "cown", "country.name", warn = TRUE)
m$iso <- countrycode(m$cown, "cown", "iso3c", warn = TRUE)
write.csv(m, "fh_polity_uds.csv")

# Merge Polity FH UDS and CNTS

polity <- read.csv("polity.csv")
cnts <- read.csv("cnts.csv")
m <- merge(polity, cnts, by=c("cown","year"), all=TRUE, sort = TRUE)
library(countrycode)
m$country <- countrycode(m$cown, "cown", "country.name", warn = TRUE)
m$iso <- countrycode(m$cown, "cown", "iso3c", warn = TRUE)

# Merge Polity FH UDS CNTS with DPI

polity <- read.csv("polity_cnts.csv")
dpi <- read.csv("dpi.csv")
k <- merge(polity, dpi, by=c("cown","year"), sort = TRUE)
write.csv(k, "polity_dpi.csv")

# Then merge CPDS

polity <- read.csv("polity_dpi.csv")
cpds <- read.csv("cpds.csv")
cpds$cown <- countrycode(cpds$iso, "iso3c", "cown", warn = TRUE)
short <- merge(polity, cpds, by=c("cown","year"), sort=TRUE)
long <- merge(polity, cpds, by=c("cown","year"), all = TRUE, sort=TRUE)
long <- long[long$year > 1974,]
short$country <- countrycode(short$cown, "cown", "country.name", warn = TRUE)
short$iso <- countrycode(short$cown, "cown", "iso3c", warn = TRUE)
long$country <- countrycode(long$cown, "cown", "country.name", warn = TRUE)
long$iso <- countrycode(long$cown, "cown", "iso3c", warn = TRUE)

write.csv(short, "s_fh_cpds.csv")
write.csv(long, "l_fh_cpds.csv")

