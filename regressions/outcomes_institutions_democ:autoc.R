setwd("~/Desktop/thesis")

library(plm)
large <- read.csv("large.csv")

library(multiwayvcov)
library(lmtest)
library(car)

# Create interaction terms

large$dem_system <- large$DEM_polity*large$system
large$dem_housesys <- large$DEM_polity*large$housesys
large$dem_allhouse <- large$DEM_polity*large$allhouse

# Regress economic growth on political institutions, plus interaction terms.

reg1 <- plm(gdpgr~system + housesys + allhouse + prevgdpgr
            + dem_system + dem_housesys + dem_allhouse
            + lgdppc + state_fail, 
            data = large, index = c("country", "year"), model="pooling")
cl_se1 <- sqrt(diag(vcovHC(reg1,type="HC0",cluster="group")))

reg2 <- plm(gdpgr~system + housesys + allhouse + prevgdpgr 
            + dem_system + dem_housesys + dem_allhouse
            + lgdppc + state_fail, 
            data = large, index = c("country", "year"),
            model="within", effect = "time")
cl_se2 <- sqrt(diag(vcovHC(reg2,type="HC0",cluster="group")))

reg3 <- plm(gdpgr~system + housesys + allhouse 
            + dem_system + dem_housesys + dem_allhouse
            + prevgdpgr + lgdppc + state_fail, data = large, 
            index = c("country", "year"), model="within", effect = "twoways")
cl_se3 <- sqrt(diag(vcovHC(reg3,type="HC0",cluster="group")))

# Outcome = deltadebt, plus interaction terms

reg4 <- plm(deltadebt~system + housesys + allhouse 
            + dem_system + dem_housesys + dem_allhouse
            + lgdppc + state_fail, 
            data = large, index = c("country", "year"), model="pooling")
cl_se4 <- sqrt(diag(vcovHC(reg4,type="HC0",cluster="group")))

reg5 <- plm(deltadebt~system + housesys + allhouse
            + dem_system + dem_housesys + dem_allhouse
            + lgdppc + state_fail, 
            data = large, index = c("country", "year"),
            model="within", effect = "time")
cl_se5 <- sqrt(diag(vcovHC(reg5,type="HC0",cluster="group")))

reg6 <- plm(deltadebt~system + housesys + allhouse 
            + dem_system + dem_housesys + dem_allhouse
            + lgdppc + state_fail, data = large, 
            index = c("country", "year"), model="within", effect = "twoways")
cl_se6 <- sqrt(diag(vcovHC(reg6,type="HC0",cluster="group")))

# Outcome = deficit, plus interaction terms

reg7 <- plm(deficit~system + housesys + allhouse 
            + dem_system + dem_housesys + dem_allhouse
            + lgdppc + state_fail, 
            data = large, index = c("country", "year"), model="pooling")
cl_se7 <- sqrt(diag(vcovHC(reg7,type="HC0",cluster="group")))

reg8 <- plm(deficit~system + housesys + allhouse
            + dem_system + dem_housesys + dem_allhouse
            + lgdppc + state_fail, 
            data = large, index = c("country", "year"),
            model="within", effect = "time")
cl_se8 <- sqrt(diag(vcovHC(reg8,type="HC0",cluster="group")))

reg9 <- plm(deficit~system + housesys + allhouse 
            + dem_system + dem_housesys + dem_allhouse
            + lgdppc + state_fail, data = large, 
            index = c("country", "year"), model="within", effect = "twoways")
cl_se9 <- sqrt(diag(vcovHC(reg9,type="HC0",cluster="group")))

stargazer(reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8, reg9, 
          se=list(cl_se1, cl_se2, cl_se3, cl_se4, cl_se5, cl_se6, cl_se7, cl_se8, cl_se9),
          title="Fixed Effects Results on Politcal Institutions, with Interaction Terms from Polity",
          no.space=FALSE,
          column.labels=c("Annual GDP Growth", "Change in Debt-GDP Ratio", "Annual Budget Deficit"),
          column.separate = c(3, 3, 3),
          covariate.labels = c("Parliamentary System", "Plurality Representation",
                               "Control All Houses", "DEM*Parliamentary System",
                               "DEM*Plurality Representation","DEM*Control All Houses",
                               "log GDP per capita", "State Failure"),
          dep.var.caption  = "Large Sample, 1975-2017",
          dep.var.labels = c("","",""),
          omit.stat = "f", omit = "prevgdpgr", omit.labels = "Previous GDP Growth?",
          add.lines = list(c("Year dummy?", "No", "Yes", "Yes","No", "Yes", "Yes", "No", "Yes", "Yes"),
                           c("Country dummy?", "No", "No", "Yes","No", "No", "Yes", "No", "No", "Yes")),
          notes = "I make this look good!",
          align=TRUE, notes.align = "l", digits=3)




# FH data


large$dem_system2 <- large$DEM_fh*large$system
large$dem_housesys2 <- large$DEM_fh*large$housesys
large$dem_allhouse2 <- large$DEM_fh*large$allhouse

# Regress economic growth on political institutions, plus interaction terms.

reg1 <- plm(gdpgr~system + housesys + allhouse + prevgdpgr
            + dem_system2 + dem_housesys2 + dem_allhouse2
            + lgdppc + state_fail, 
            data = large, index = c("country", "year"), model="pooling")
cl_se1 <- sqrt(diag(vcovHC(reg1,type="HC0",cluster="group")))

reg2 <- plm(gdpgr~system + housesys + allhouse + prevgdpgr 
            + dem_system2 + dem_housesys2 + dem_allhouse2
            + lgdppc + state_fail, 
            data = large, index = c("country", "year"),
            model="within", effect = "time")
cl_se2 <- sqrt(diag(vcovHC(reg2,type="HC0",cluster="group")))

reg3 <- plm(gdpgr~system + housesys + allhouse 
            + dem_system2 + dem_housesys2 + dem_allhouse2
            + prevgdpgr + lgdppc + state_fail, data = large, 
            index = c("country", "year"), model="within", effect = "twoways")
cl_se3 <- sqrt(diag(vcovHC(reg3,type="HC0",cluster="group")))

# Outcome = deltadebt, plus interaction terms

reg4 <- plm(deltadebt~system + housesys + allhouse 
            + dem_system2 + dem_housesys2 + dem_allhouse2
            + lgdppc + state_fail, 
            data = large, index = c("country", "year"), model="pooling")
cl_se4 <- sqrt(diag(vcovHC(reg4,type="HC0",cluster="group")))

reg5 <- plm(deltadebt~system + housesys + allhouse
            + dem_system2 + dem_housesys2 + dem_allhouse2
            + lgdppc + state_fail, 
            data = large, index = c("country", "year"),
            model="within", effect = "time")
cl_se5 <- sqrt(diag(vcovHC(reg5,type="HC0",cluster="group")))

reg6 <- plm(deltadebt~system + housesys + allhouse 
            + dem_system2 + dem_housesys2 + dem_allhouse2
            + lgdppc + state_fail, data = large, 
            index = c("country", "year"), model="within", effect = "twoways")
cl_se6 <- sqrt(diag(vcovHC(reg6,type="HC0",cluster="group")))

# Outcome = deficit, plus interaction terms

reg7 <- plm(deficit~system + housesys + allhouse 
            + dem_system2 + dem_housesys2 + dem_allhouse2
            + lgdppc + state_fail, 
            data = large, index = c("country", "year"), model="pooling")
cl_se7 <- sqrt(diag(vcovHC(reg7,type="HC0",cluster="group")))

reg8 <- plm(deficit~system + housesys + allhouse
            + dem_system2 + dem_housesys2 + dem_allhouse2
            + lgdppc + state_fail, 
            data = large, index = c("country", "year"),
            model="within", effect = "time")
cl_se8 <- sqrt(diag(vcovHC(reg8,type="HC0",cluster="group")))

reg9 <- plm(deficit~system + housesys + allhouse 
            + dem_system2 + dem_housesys2 + dem_allhouse2
            + lgdppc + state_fail, data = large, 
            index = c("country", "year"), model="within", effect = "twoways")
cl_se9 <- sqrt(diag(vcovHC(reg9,type="HC0",cluster="group")))

stargazer(reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8, reg9, 
          se=list(cl_se1, cl_se2, cl_se3, cl_se4, cl_se5, cl_se6, cl_se7, cl_se8, cl_se9),
          title="Fixed Effects Results on Politcal Institutions, with Interaction Term from Freedom House",
          no.space=FALSE,
          column.labels=c("Annual GDP Growth", "Change in Debt-GDP Ratio", "Annual Budget Deficit"),
          column.separate = c(3, 3, 3),
          covariate.labels = c("Parliamentary System", "Plurality Representation",
                               "Control All Houses", "DEM*Parliamentary System",
                               "DEM*Plurality Representation","DEM*Control All Houses",
                               "log GDP per capita", "State Failure"),
          dep.var.caption  = "Large Sample, 1975-2017",
          dep.var.labels = c("","",""),
          omit.stat = "f", omit = "prevgdpgr", omit.labels = "Previous GDP Growth?",
          add.lines = list(c("Year dummy?", "No", "Yes", "Yes","No", "Yes", "Yes", "No", "Yes", "Yes"),
                           c("Country dummy?", "No", "No", "Yes","No", "No", "Yes", "No", "No", "Yes")),
          notes = "I make this look good!",
          align=TRUE, notes.align = "l", digits=3)

