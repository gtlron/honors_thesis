# Edit polity.csv

polity <- read.csv("polity.csv")

#check if polity annual code is the same as cown code
empty <- polity[polity$polity_annual_ccode != polity$cown,]
