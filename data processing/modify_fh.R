# DO NOT USE. Fix fh.csv manually instead.
# Keep only the useful columns of fh, remove entries 
# with no ISO codes

fh <- read.csv("fh.csv", stringsAsFactors = FALSE)
View(fh)
fh$iso[fh$fh_country == "Micronesia"] <- "FSM"
fh[fh ==""] <- NA
fh <- na.omit(fh)
fh$GWn <- NULL
fh$in_GW_system <- NULL
fh$x <- NULL
write.csv(fh, "fh.csv")
