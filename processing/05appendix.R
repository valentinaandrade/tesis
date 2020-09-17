# Code 4: Modelos Alternativos 
# 1. Cargar base de datos
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
rm(list = ls())
load(file = "../output/data/data-model.RData")
db <- db_model

# Modelo 2 Panel Vector Autorregresive model Coord  ----------
db_model$country <- as.factor(db_model$country)
db_model$country <- as.numeric(db_model$country)
db_model$year <- as.numeric(db_model$year)
db_model <- as.data.frame(db_model)
db_model2 <- filter(db_model, region == "Latin America & Caribbean")

model2 <-pvargmm(
  dependent_vars = c("fudi"),
  lags = 1,
  exog_vars = c("f_UR", "f_LFPR","f_sector_SER",
                "f_SH_PT","rmw", "AdjCov",
                "T_GDPHRS_V", "ud"),
  transformation = "fd",
  data = db_model2,
  panel_identifier = c("country", "year"),
  steps = c("onestep"),
  system_instruments = FALSE,
  max_instr_dependent_vars = 99,
  min_instr_dependent_vars = 2L,
  collapse = T)

summary(model2)

# Modelo 3 PVAR - Europa
db_model3 <- filter(db_model, region == "Europe & Central Asia")

model3 <-pvargmm(
  dependent_vars = c("fudi"),
  lags = 1,
  exog_vars = c("f_UR", "f_LFPR","f_sector_SER",
                "f_SH_PT","AdjCov","rmw",
                "T_GDPHRS_V", "Coord","ud"),
  transformation = "fd",
  data = db_model3,
  panel_identifier = c("country", "year"),
  steps = c("onestep"),
  system_instruments = FALSE,
  max_instr_dependent_vars = 99,
  min_instr_dependent_vars = 2L,
  collapse = T)

summary(model3)

screenreg(list(model2, model3),custom.model.names = c('PVAM - Latinamérica', 'PVAM-Europa'))
knit_print.summary.pvargmm(list(model2, model3))

# Resultados: Coord negativa. 1 = - coordinada, 5 = + coordinada
## Países con Negociación salarial fragmentada, limitada en gran medida a empresas o plantas individuales, sin coordinación
