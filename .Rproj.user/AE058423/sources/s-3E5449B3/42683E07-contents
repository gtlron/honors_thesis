# Add cown to dpi dataset

dpi <- read.csv("DPI2017_basefile_Jan2018.csv")
dpi$cown <- countrycode(dpi$ifs, "iso3c", "cown", warn = TRUE)