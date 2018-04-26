# Add cown to cnts dataset

cnts <- read.csv("cnts.csv")
library(countrycode)
cnts$cown <- countrycode(cnts$Wbcode, "iso3c", "cown", warn = TRUE)