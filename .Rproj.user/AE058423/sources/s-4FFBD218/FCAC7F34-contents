setwd("~/Desktop/thesis")
debt <- read.csv("debt.csv")
colnames(debt)[6:64] <- as.numeric(colnames(debt)[6:64])
library(reshape)
debt <- melt(debt, id=c(1:5))
debt <- debt[order(Country.Name),]
debt$cown <- countrycode(debt$Country.Code, "iso3c", "cown", warn = TRUE)
debt$cown <- countrycode(debt$Country.Name, "country.name", "cown", warn = TRUE)
