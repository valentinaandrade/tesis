# Chapter 3:  ECM

# 1. Cargar base de datos
pacman::p_load(tidyverse, # Manage data
               ecm,# Error correction model
               tseries,
               vars, # augmented Dickey-Fuller test
               urca, # Philips y Ouliaris test cointegration
               panelvar, # PVAL
               plm, #poleed data model
               lmtest)  #panel corrected standar errors

# 2. Cargar base de datos
options(scipen=999)
rm(list = ls())
load(file = "../output/data/data-model.RData")
db <- db_model


# Model 0 ---------------
#Models correct for panel-specific, first-order autocorrelation
model0 <-plm(
    fudi ~ diff(f_UR) + f_LFPR + f_sector_SER + f_SH_PT + Coord +
      T_GDPHRS_V + ud ,
    index = c("country", "year"),
    model = "within",
    effect = "twoways",
    data = db_model)

summary(model0)

# Cluster errores ---
coeftest(model0, vcov=function(x)vcovBK(x, type="HC1", cluster="time"))

# Modelo 1 Panel Vector Autorregresive model  ----------
db_model$country <- as.factor(db_model$country)
db_model$country <- as.numeric(db_model$country)
db_model$year <- as.numeric(db_model$year)
db_model <- as.data.frame(db_model)

model1 <-pvargmm(
  dependent_vars = c("fudi"),
  lags = 1,
  exog_vars = c("f_UR", "f_LFPR","f_sector_SER",
                "f_SH_PT","Coord","AdjCov","rmw",
                "T_GDPHRS_V","ud"),
  transformation = "fd",
  data = db_model,
  panel_identifier = c("country", "year"),
  steps = c("onestep"),
  system_instruments = FALSE,
  max_instr_dependent_vars = 99,
  min_instr_dependent_vars = 2L,
  collapse = T)

summary(model1)
# Resultados: Coord negativa. 

# Modelo por países ----

# 3. Construir modelo por países segun resultados
db <- db %>% filter(country == "Sweden")

names(db)
model <- db 
xeq<-model[c('f_LFPR','f_sector_SER','f_SH_PT', 'Coord')]
xtr<- model[c('f_UR', 'rmw', 'T_GDPHRS_V', 'AdjCov')]
xeq<-as.data.frame(xeq)
xtr<-as.data.frame(xtr)
(ecm1 <- ecm(model$fudi, xeq, xtr, includeIntercept = TRUE))
summary(ecm1)

# Desempleo femenino tiene un efecto positivo al corto plazo sobre densidad sindical fem

# Si se pone en xtr (corto plazo) calcula deltas, sino, n


