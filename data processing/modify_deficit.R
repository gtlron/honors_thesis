# Add cown and country names to dataset

deficit <- read.csv("deficit.csv")
library(countrycode)
deficit$cown <- countrycode(deficit$X...LOCATION, "iso3c", "cown", warn = TRUE)
deficit$country.name <- countrycode(deficit$X...LOCATION, "iso3c", "country.name", 
                                    warn = TRUE)
