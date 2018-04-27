# Merge FH and UDS data

setwd("~/Desktop/thesis")
fh <- read.csv("fh.csv")
uds <- read.csv("uds_summary.csv")
m <- merge(fh, uds, by=c("cown","year"), all=TRUE, sort = TRUE)
m$country <- countrycode(m$cown, "cown", "country.name", warn = TRUE)
m$iso <- countrycode(m$cown, "cown", "iso3c", warn = TRUE)
write.csv(m, "fh_uds.csv")
