# restructure world bank dataset

setwd("~/Desktop")

#real gdp growth dataset
realgdpgr <- read.csv("realgdpgr.csv", check.names = FALSE)
colnames(realgdpgr)[5:62] <- as.numeric(colnames(realgdpgr)[5:62])

library(reshape)
realgdpgr <- melt(realgdpgr, id=c(1:4))

colnames(realgdpgr)[1] <- "Country.Name"
colnames(realgdpgr)[5] <- "year"
colnames(realgdpgr)[6] <- "realgdpgr"
realgdpgr <- realgdpgr[order(realgdpgr$Country.Name),]
realgdpgr$cown <- countrycode(realgdpgr$Country.Name, "country.name", "cown", warn = TRUE)
write.csv(realgdpgr, "realgdpgr.csv")

# gdp growth per capita dataset
gdppcgr <- read.csv("gdppcgr.csv", check.names = FALSE)
colnames(gdppcgr)[5:62] <- as.numeric(colnames(gdppcgr)[5:62])

gdppcgr <- melt(gdppcgr, id=c(1:4))

colnames(gdppcgr)[1] <- "Country.Name"
colnames(gdppcgr)[5] <- "year"
colnames(gdppcgr)[6] <- "gdppcgr"
gdppcgr <- gdppcgr[order(gdppcgr$Country.Name),]
gdppcgr$cown <- countrycode(gdppcgr$Country.Name, "country.name", "cown", warn = TRUE)
write.csv(gdppcgr, "gdppcgr.csv")

# real gdp current us dollar
currentgdp <- read.csv("currentgdp.csv", check.names = FALSE)
colnames(currentgdp)[5:62] <- as.numeric(colnames(currentgdp)[5:62])

currentgdp <- melt(currentgdp, id=c(1:4))

colnames(currentgdp)[1] <- "Country.Name"
colnames(currentgdp)[5] <- "year"
colnames(currentgdp)[6] <- "currentgdp"
currentgdp <- currentgdp[order(currentgdp$Country.Name),]
currentgdp$cown <- countrycode(currentgdp$Country.Name, "country.name", "cown", warn = TRUE)
write.csv(currentgdp, "currentgdp.csv")

# inflation
inflation <- read.csv("inflation.csv", check.names = FALSE)
colnames(inflation)[5:62] <- as.numeric(colnames(inflation)[5:62])

inflation <- melt(inflation, id=c(1:4))

colnames(inflation)[1] <- "Country.Name"
colnames(inflation)[5] <- "year"
colnames(inflation)[6] <- "inflation"
inflation <- inflation[order(inflation$Country.Name),]
inflation$cown <- countrycode(inflation$Country.Name, "country.name", "cown", warn = TRUE)
write.csv(inflation, "inflation.csv")