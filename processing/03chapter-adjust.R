pacman::p_load(tidyverse, # Manage data
               ecm,# Error correction model
               tseries,
               vars, # augmented Dickey-Fuller test
               urca, # Philips y Ouliaris test cointegration
               panelvar, # PVAL
               plm, #poleed data model
               lmtest,  
               texreg,
               pcse) #BBeck and Katz -Baily and Katz  

# 2. Cargar base de datos
options(scipen=999)
load(file = "../output/data/data-model.RData")


# Unconditional estimate of standard error (labeled BK) (p.25)
vcovs <- c("vcov", "Vw", "Vcx", "Vct", "Vcxt", "Vct.L", "Vnw.L", "Vscc.L","Vcxt.L", "vcovBK")

names(vcovs) <- c("OLS", "Vw", "Vcx", "Vct", "Vcxt", "Vct.L", "Vnw.L",
                  "Vscc.L", "Vcxt.L", "BK")
cfrtab <- matrix(nrow = length(coef(model0)), ncol = 1+length(vcovs))
dimnames(cfrtab) <- list(names(coef(model0)),
                         c("Coefficient", paste("s.e.", names(vcovs))))
cfrtab[ , 1] <- coef(model0)

for(i in 1:length(vcovs)) {
  cfrtab[ , 1 + i] <- coeftest(model0, vcov = vcovs[[i]])[ , 2]
  }

print(t(round(cfrtab, 4)))

#table of significance diagnostics, BK is reported last

coeftest(model0, vcov=function(x)vcovBK(x, type="HC1", cluster=c("time")))
waldtest(model0, vcov=function(x)vcovBK(x, type="HC1", cluster="time"))
         

library("pcse")


model0 <-plm(
  fudi ~ diff(f_UR) + f_LFPR + f_sector_SER + f_SH_PT + Coord +
    T_GDPHRS_V  + rmw,
  index = c("country", "year"),
  model = "w",
  effect = "time",
  data = db_model)
summary(model0)

