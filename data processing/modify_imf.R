# Restructure imf dataset and add country codes, combine

imf_deficit <- read.csv("imf_deficit.csv")
imf_debt <- read.csv("imf_dbt.csv")
library(reshape)
imf_deficit <- melt(imf_deficit, id.vars = c(1:4), measure.vars = c(5:17))
imf_debt <- melt(imf_debt, id.vars = c(1:4), measure.vars = c(5:17))
write.csv(imf_deficit, "imf_deficit.csv")
write.csv(imf_debt, "imf_debt.csv")

imf <- read.csv("imf_deficit_debt.csv")
library(countrycode)
imf$cown <- countrycode(imf$Country.name, "country.name", "cown", warn = TRUE)
imf$iso <- countrycode(imf$Country.name, "country.name", "iso3c", warn = TRUE)
imf <- imf[order(imf$Country.name),]
write.csv(imf, "imf_deficit_debt.csv")
