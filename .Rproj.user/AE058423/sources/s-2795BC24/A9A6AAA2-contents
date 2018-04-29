# Merge data from worldbank together

setwd("~/Desktop/thesis")
worldbank <- read.csv("worldbank.csv")
debt <- read.csv("debt.csv")
m <- merge(worldbank, debt, by=c("country","year"), all=TRUE, sort = TRUE)
m <- subset(m, m$year!=1959)
m$cown.y <- NULL
View(m[which(m$iso.x!=m$iso.y),])

write.csv(m, "worldbank_new.csv")

# Merge OECD and IMF data
OECD <- read.csv("deficit.csv")
IMF <- read.csv("imf_deficit_debt.csv")
m <- merge(IMF, OECD, by=c("iso","year"), all=TRUE, sort = TRUE)
m$Country.name[is.na(m$Country.name)] <- m$country.name[is.na(m$Country.name)]
colnames(m)[4] <- "cown"
m$cown[is.na(m$cown)] <- m$cown.y[is.na(m$cown)]
write.csv(m, "imf_oecd.csv")

# Merge WB and IMF&OECD data
wb <- read.csv("worldbank.csv")
imf_oecd <- read.csv("imf_oecd.csv")
m <- merge(wb, imf_oecd, by=c("iso","year"), all=TRUE, sort = TRUE)
m$cown.x[is.na(m$cown.x)] <- m$cown.y[is.na(m$cown.x)]
write.csv(m, "econ_outcomes.csv")

# Modify incorrect terms in econ_outcomes
econ <- read.csv("econ_outcomes.csv")
library(countrycode)
econ$cown <- countrycode(econ$iso, "iso3c", "cown", warn = TRUE)
region <- read.csv("countryclass.csv")
econ <- merge(econ, region, by=c("iso"), all=TRUE, sort = TRUE)
econ$region.x <- NULL
write.csv(econ, "econ.csv")
