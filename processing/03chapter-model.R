# Chapter 3:  ECM

# 1. Cargar base de datos
pacman::p_load(tidyverse, # Manage data
               lubridate, # Manage time series
               ecm,# Error correction model
               broom, #Residuals
               tseries,
               vars, # augmented Dickey-Fuller test
               urca) # Philips y Ouliaris test cointegration

# 2. Cargar base de datos
rm(list = ls())
load(file = "../output/data/data-model.RData")
db <- db_model; remove(db_model)

db <- db %>% filter(country == "Sweden")

# 3. Construir modelo
names(db)
model <- db 
xeq<-model[c('f_LFPR','f_sector_SER','f_SH_PT', 'Coord')]
xtr<- model[c('f_UR', 'rmw', 'T_GDPHRS_V', 'AdjCov')]
xeq<-as.data.frame(xeq)
xtr<-as.data.frame(xtr)
(ecm1 <- ecm(model$fudi, xeq, xtr, includeIntercept = TRUE))
summary(ecm1)

# Desempleo femenino tiene un efecto positivo al corto pzo sobre densidad sindical fem

# Si se pone en xtr (corto plazo) calcula deltas, sino, n