setwd("~/Desktop/thesis")

library(plm)
large <- read.csv("large.csv")

library(multiwayvcov)
library(lmtest)
library(car)

#polity
reg2 <- plm(deltadebt~polity, data = large, index = c("country", "year"),
            model="pooling")
cl_se2 <- sqrt(diag(vcovHC(reg2,type="HC0",cluster="group")))

reg3 <- plm(deltadebt~ polity, data = large, index = c("country", "year"),
            model="within")
cl_se3 <- sqrt(diag(vcovHC(reg3,type="HC0",cluster="group")))

reg4 <- plm(deltadebt~polity, data = large, index = c("country", "year"),
            model="within", effect = "twoways")
cl_se4 <- sqrt(diag(vcovHC(reg4,type="HC0",cluster="group")))

reg5 <- plm(deltadebt~ polity + lgdppc, data = large, index = c("country", "year"),
            model="within", effect = "twoways")
cl_se5 <- sqrt(diag(vcovHC(reg5,type="HC0",cluster="group")))

reg6 <- plm(deltadebt~ polity + lgdppc + state_fail, data = large, 
            index = c("country", "year"), model="within", effect = "twoways")
cl_se6 <- sqrt(diag(vcovHC(reg6,type="HC0",cluster="group")))

reg7 <- plm(deltadebt~ polity + lgdppc + state_fail |  liec + 
              lgdppc + state_fail, data = large, index = c("country", "year"),
            model="within", effect = "twoways")
cl_se7 <- sqrt(diag(vcovHC(reg7,type="HC0",cluster="group")))

# Example with html output. Use LaTex tables instead.
stargazer(reg2, reg3, reg4, reg5, reg6, reg7, 
          se=list(cl_se2, cl_se3, cl_se4, cl_se5, cl_se6, cl_se7),
          title="Fixed Effects Results Regressing Debt Accumulation on Polity Measure of Democracy",
          column.labels=c("OLS", "Fixed Effects", "IV"),
          column.separate = c(1, 4, 1), no.space=FALSE,
          covariate.labels = c("Polity Score", "log GDP per capita","State Failure"),
          dep.var.caption  = "Large Sample, 1975-2017",
          dep.var.labels = "Change in Debt-GDP Ratio from Last Year", omit.stat = "f",
          add.lines = list(c("Country dummy?", "No", "Yes", "Yes","Yes", "Yes","Yes"),
                           c("Year dummy?", "No", "No", "Yes","Yes", "Yes","Yes")),
          notes = "I make this look good!",
          align=TRUE, notes.align = "l", digits=3)

#freedom house
reg8 <- plm(deltadebt~fh_score, data = large, index = c("country", "year"),
            model="pooling")
cl_se8 <- sqrt(diag(vcovHC(reg8,type="HC0",cluster="group")))

reg9 <- plm(deltadebt~ fh_score, data = large, index = c("country", "year"),
            model="within")
cl_se9 <- sqrt(diag(vcovHC(reg9,type="HC0",cluster="group")))

reg10 <- plm(deltadebt~fh_score, data = large, index = c("country", "year"),
            model="within", effect = "twoways")
cl_se10 <- sqrt(diag(vcovHC(reg10,type="HC0",cluster="group")))

reg11 <- plm(deltadebt~ fh_score + lgdppc, data = large, index = c("country", "year"),
            model="within", effect = "twoways")
cl_se11 <- sqrt(diag(vcovHC(reg11,type="HC0",cluster="group")))

reg12 <- plm(deltadebt~ fh_score + lgdppc + state_fail, data = large, 
            index = c("country", "year"), model="within", effect = "twoways")
cl_se12 <- sqrt(diag(vcovHC(reg12,type="HC0",cluster="group")))

reg13 <- plm(deltadebt~ fh_score + lgdppc + state_fail |  liec + 
              lgdppc + state_fail, data = large, index = c("country", "year"),
            model="within", effect = "twoways")
cl_se13 <- sqrt(diag(vcovHC(reg13,type="HC0",cluster="group")))

stargazer(reg8, reg9, reg10, reg11, reg12, reg13, 
          se=list(cl_se8, cl_se9, cl_se10, cl_se11, cl_se12, cl_se13),
          title="Fixed Effects Results Regressing Debt Accumulation on Freedom House Measure of Democracy",
          column.labels=c("OLS", "Fixed Effects", "IV"),
          column.separate = c(1, 4, 1), no.space=FALSE,
          covariate.labels = c("Freedom House Score", "log GDP per capita", "State Failure"),
          dep.var.caption  = "Large Sample, 1975-2017",
          dep.var.labels = "Change in Debt-GDP Ratio from Last Year", omit.stat = "f",
          add.lines = list(c("Country dummy?", "No", "Yes", "Yes","Yes", "Yes","Yes"),
                           c("Year dummy?", "No", "No", "Yes","Yes", "Yes","Yes")),
          notes = "I make this look good!",
          align=TRUE, notes.align = "l", digits=3)

#UDS

reg8 <- plm(deltadebt~uds_mean, data = large, index = c("country", "year"),
            model="pooling")
cl_se8 <- sqrt(diag(vcovHC(reg8,type="HC0",cluster="group")))

reg9 <- plm(deltadebt~ uds_mean, data = large, index = c("country", "year"),
            model="within")
cl_se9 <- sqrt(diag(vcovHC(reg9,type="HC0",cluster="group")))

reg10 <- plm(deltadebt~uds_mean, data = large, index = c("country", "year"),
             model="within", effect = "twoways")
cl_se10 <- sqrt(diag(vcovHC(reg10,type="HC0",cluster="group")))

reg11 <- plm(deltadebt~ uds_mean + lgdppc, data = large, index = c("country", "year"),
             model="within", effect = "twoways")
cl_se11 <- sqrt(diag(vcovHC(reg11,type="HC0",cluster="group")))

reg12 <- plm(deltadebt~ uds_mean + lgdppc + state_fail, data = large, 
             index = c("country", "year"), model="within", effect = "twoways")
cl_se12 <- sqrt(diag(vcovHC(reg12,type="HC0",cluster="group")))

reg13 <- plm(deltadebt~ uds_mean + lgdppc + state_fail |  liec + 
               lgdppc + state_fail, data = large, index = c("country", "year"),
             model="within", effect = "twoways")
cl_se13 <- sqrt(diag(vcovHC(reg13,type="HC0",cluster="group")))

stargazer(reg8, reg9, reg10, reg11, reg12, reg13, 
          se=list(cl_se8, cl_se9, cl_se10, cl_se11, cl_se12, cl_se13),
          title="Fixed Effects Results Regressing Debt Accumulation on Unified Democracy Scores",
          column.labels=c("OLS", "Fixed Effects", "IV"),
          column.separate = c(1, 4, 1), no.space=FALSE,
          covariate.labels = c("Unified Democracy Scores", "log GDP per capita", "State Failure"),
          dep.var.caption  = "Large Sample, 1975-2017",
          dep.var.labels = "Change in Debt-GDP Ratio from Last Year", omit.stat = "f",
          add.lines = list(c("Country dummy?", "No", "Yes", "Yes","Yes", "Yes","Yes"),
                           c("Year dummy?", "No", "No", "Yes","Yes", "Yes","Yes")),
          notes = "I make this look good!",
          align=TRUE, notes.align = "l", digits=3)
