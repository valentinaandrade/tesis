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

# Granger causality -----------------------

grangertest(fudi ~ f_LFPR, order = 8, data = db)
# Lag order in adf (Augmented Dikey Fuller)
db1 <- db_model %>% group_by(country) %>%  filter(n() >= 8)

# Granger with panel dat
pgrangertest(fudi ~ f_UR, order = 1, index = c("country", "year"),  data = db1)
# Ztilde = 5.3327, p-value = 0.00000009675

pgrangertest(fudi ~ T_GDPHRS_V, order = 1, index = c("country", "year"),
             test = c("Ztilde", "Zbar", "Wbar"), data = db1)
# Ztilde = 0.81013, p-value = 0.4179
pgrangertest(fudi ~ rmw, order = 1, index = c("country", "year"),
             test = c("Ztilde"), data = db1)

pgrangertest(fudi ~ f_LFPR, order = 1, index = c("country", "year"),  data = db1)
##Panel Granger (Non-)Causality Test - (Dumitrescu/Hurlin (2012))
### Ztilde = 3.4782, p-value = 0.0005049
## Granger causality for at least one individual

pgrangertest(fudi ~ f_sector_SER, order = 1, index = c("country", "year"),
             test = c("Ztilde", "Zbar", "Wbar"), data = db1)
# Ztilde = 1.4866, p-value = 0.1371

pgrangertest(fudi ~ f_SH_PT, order = 1, index = c("country", "year"),
             test = c("Ztilde", "Zbar", "Wbar"), data = db1)
# Ztilde = 1.1482, p-value = 0.2509

pgrangertest(fudi ~ Coord, order = 1, index = c("country", "year"),
             test = c("Ztilde", "Zbar", "Wbar"), data = db1)

# No, al revez

pgrangertest(fudi ~ AdjCov, order = 1, index = c("country", "year"),
             test = c("Ztilde", "Zbar", "Wbar"), data = db1)

# Ztilde = 47.692, p-value < 0.00000000000000022

pgrangertest(fudi ~ ud, order = 1, index = c("country", "year"),
             test = c("Ztilde", "Zbar", "Wbar"), data = db1)

# Ztilde = 4.3392, p-value = 0.0000143


## Durbin Watson ----
# Durbin--Watson Test for Panel Models
pdwtest(model0)
# DW = 0.31426, p-value < 0.00000000000000022
# the null hypothesis that the errors are serially uncorrelated
#against the alternative that they follow a first order autoregressive process.


pdwtest(model01)
# DW = 1.9295, p-value = 0.1611

pdwtest(model02)
pdwtest(model03)
pdwtest(model04)
