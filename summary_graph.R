setwd("~/Desktop")

# Produce summary statistics table with Stargazer
library(stargazer)
stargazer(small, type = "html", title="Table 1: Summary Statistics", 
          out="Summary statisticss.html")
stargazer(large, type = "html", title="Table 2: Summary Statistics", 
          out="Summary statisticsl.html")
