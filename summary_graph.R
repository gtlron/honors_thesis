setwd("~/Desktop")
small <- read.csv("small.csv")
large <- read.csv("large.csv")

# Produce summary statistics table with Stargazer
library(stargazer)
stargazer(small, type = "html", title="Table 1: Summary Statistics", 
          out="Summary statistics.html")
