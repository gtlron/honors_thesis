setwd("~/Desktop/thesis")

library(plm)
large <- read.csv("large.csv")

# Detect duplicate id-year pairs
large[duplicated(large[,1:2]),]

library(multiwayvcov)
library(lmtest)
library(car)

reg2 <- plm(gdpgr~prevgdpgr + polity, data = large, index = c("country", "year"),
            model="pooling")
cl_se2 <- sqrt(diag(vcovHC(reg2,type="HC0",cluster="group")))

reg3 <- plm(gdpgr~prevgdpgr + polity, data = large, index = c("country", "year"),
            model="within")
cl_se3 <- sqrt(diag(vcovHC(reg3,type="HC0",cluster="group")))

reg4 <- plm(gdpgr~prevgdpgr + polity, data = large, index = c("country", "year"),
            model="within", effect = "twoways")
cl_se4 <- sqrt(diag(vcovHC(reg4,type="HC0",cluster="group")))

reg5 <- plm(gdpgr~prevgdpgr + polity + lgdppc, data = large, index = c("country", "year"),
            model="within", effect = "twoways")
cl_se5 <- sqrt(diag(vcovHC(reg5,type="HC0",cluster="group")))

reg6 <- plm(gdpgr~prevgdpgr + polity + lgdppc + state_fail, data = large, 
            index = c("country", "year"), model="within", effect = "twoways")
cl_se6 <- sqrt(diag(vcovHC(reg6,type="HC0",cluster="group")))

reg7 <- plm(gdpgr~prevgdpgr + polity + lgdppc + state_fail | prevgdpgr + liec + 
              lgdppc + state_fail, data = large, index = c("country", "year"),
            model="within", effect = "twoways")
cl_se7 <- sqrt(diag(vcovHC(reg7,type="HC0",cluster="group")))

# Example with html output. Use LaTex tables instead.
stargazer(reg2, reg3, reg4, reg5, reg6, reg7, 
          se=list(cl_se2, cl_se3, cl_se4, cl_se5, cl_se6, cl_se7),
          title="Fixed Effects Results Regressing Economic Growth on Polity Measure of Democracy",
          no.space=FALSE,
          column.labels=c("Pooled OLS", "Fixed Effects", "IV"),
          column.separate = c(1, 4, 1),
          covariate.labels = c("Previous GDP Growth", "Polity Score", "log GDP per capita",
                               "State Failure"),
          dep.var.caption  = "Large Sample, 1975-2017",
          dep.var.labels = "Annual GDP Growth", omit.stat = "f",
          add.lines = list(c("Country dummy?", "No", "Yes", "Yes","Yes", "Yes","Yes"),
                           c("Year dummy?", "No", "No", "Yes","Yes", "Yes","Yes")),
          notes = "I make this look good!",
          align=TRUE, notes.align = "l", digits=3)

#FH

reg2 <- plm(gdpgr~prevgdpgr + fh_score, data = large, index = c("country", "year"),
            model="pooling")
cl_se2 <- sqrt(diag(vcovHC(reg2,type="HC0",cluster="group")))

reg3 <- plm(gdpgr~prevgdpgr + fh_score, data = large, index = c("country", "year"),
            model="within")
cl_se3 <- sqrt(diag(vcovHC(reg3,type="HC0",cluster="group")))

reg4 <- plm(gdpgr~prevgdpgr + fh_score, data = large, index = c("country", "year"),
            model="within", effect = "twoways")
cl_se4 <- sqrt(diag(vcovHC(reg4,type="HC0",cluster="group")))

reg5 <- plm(gdpgr~prevgdpgr + fh_score + lgdppc, data = large, index = c("country", "year"),
            model="within", effect = "twoways")
cl_se5 <- sqrt(diag(vcovHC(reg5,type="HC0",cluster="group")))

reg6 <- plm(gdpgr~prevgdpgr + fh_score + lgdppc + state_fail, data = large, 
            index = c("country", "year"), model="within", effect = "twoways")
cl_se6 <- sqrt(diag(vcovHC(reg6,type="HC0",cluster="group")))

reg7 <- plm(gdpgr~prevgdpgr + fh_score + lgdppc + state_fail | prevgdpgr + liec + 
              lgdppc + state_fail, data = large, index = c("country", "year"),
            model="within", effect = "twoways")
cl_se7 <- sqrt(diag(vcovHC(reg7,type="HC0",cluster="group")))

stargazer(reg2, reg3, reg4, reg5, reg6, reg7, 
          se=list(cl_se2, cl_se3, cl_se4, cl_se5, cl_se6, cl_se7),
          title="Fixed Effects Results Regressing Economic Growth on Freedom House Measure of Democracy",
          no.space=FALSE,
          column.labels=c("Pooled OLS", "Fixed Effects", "IV"),
          column.separate = c(1, 4, 1),
          covariate.labels = c("Previous GDP Growth", "Freedom House Score", "log GDP per capita",
                               "State Failure"),
          dep.var.caption  = "Large Sample, 1975-2017",
          dep.var.labels = "Annual GDP Growth", omit.stat = "f",
          add.lines = list(c("Country dummy?", "No", "Yes", "Yes","Yes", "Yes","Yes"),
                           c("Year dummy?", "No", "No", "Yes","Yes", "Yes","Yes")),
          notes = "I make this look good!",
          align=TRUE, notes.align = "l", digits=3)

#UDS

reg2 <- plm(gdpgr~prevgdpgr + uds_mean, data = large, index = c("country", "year"),
            model="pooling")
cl_se2 <- sqrt(diag(vcovHC(reg2,type="HC0",cluster="group")))

reg3 <- plm(gdpgr~prevgdpgr + uds_mean, data = large, index = c("country", "year"),
            model="within")
cl_se3 <- sqrt(diag(vcovHC(reg3,type="HC0",cluster="group")))

reg4 <- plm(gdpgr~prevgdpgr + uds_mean, data = large, index = c("country", "year"),
            model="within", effect = "twoways")
cl_se4 <- sqrt(diag(vcovHC(reg4,type="HC0",cluster="group")))

reg5 <- plm(gdpgr~prevgdpgr + uds_mean + lgdppc, data = large, index = c("country", "year"),
            model="within", effect = "twoways")
cl_se5 <- sqrt(diag(vcovHC(reg5,type="HC0",cluster="group")))

reg6 <- plm(gdpgr~prevgdpgr + uds_mean + lgdppc + state_fail, data = large, 
            index = c("country", "year"), model="within", effect = "twoways")
cl_se6 <- sqrt(diag(vcovHC(reg6,type="HC0",cluster="group")))

reg7 <- plm(gdpgr~prevgdpgr + uds_mean + lgdppc + state_fail | prevgdpgr + liec + 
              lgdppc + state_fail, data = large, index = c("country", "year"),
            model="within", effect = "twoways")
cl_se7 <- sqrt(diag(vcovHC(reg7,type="HC0",cluster="group")))

stargazer(reg2, reg3, reg4, reg5, reg6, reg7, 
          se=list(cl_se2, cl_se3, cl_se4, cl_se5, cl_se6, cl_se7),
          title="Fixed Effects Results Regressing Economic Growth on Unified Democracy Scores",
          no.space=FALSE,
          column.labels=c("Pooled OLS", "Fixed Effects", "IV"),
          column.separate = c(1, 4, 1),
          covariate.labels = c("Previous GDP Growth", "Unified Democracy Scores", "log GDP per capita",
                               "State Failure"),
          dep.var.caption  = "Large Sample, 1975-2017",
          dep.var.labels = "Annual GDP Growth", omit.stat = "f",
          add.lines = list(c("Country dummy?", "No", "Yes", "Yes","Yes", "Yes","Yes"),
                           c("Year dummy?", "No", "No", "Yes","Yes", "Yes","Yes")),
          notes = "I make this look good!",
          align=TRUE, notes.align = "l", digits=3)




