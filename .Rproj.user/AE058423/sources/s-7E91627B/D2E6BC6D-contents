# Merge data from worldbank together

setwd("~/Desktop/thesis")
worldbank <- read.csv("worldbank.csv")
debt <- read.csv("debt.csv")
m <- merge(worldbank, debt, by=c("country","year"), all=TRUE, sort = TRUE)
m <- subset(m, m$year!=1959)
m$cown.y <- NULL
View(m[which(m$iso.x!=m$iso.y),])

write.csv(m, "worldbank_new.csv")
