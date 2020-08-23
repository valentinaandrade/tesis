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
load(file = "../data/db-proc.RData")

# 3. Seleccionar variables
names(db)
db <- db %>% dplyr::select(country, year, ud_fem2, ud_male2, UR_WOMEN, rmw, LFPR_WOMEN, SH_PT_MW, sector_ser_FE,Coord, UD) %>% 
  mutate(fudi = (ud_fem2/ud_male2)) %>% dplyr::select(-c(ud_fem2, ud_male2))

model <- db %>% filter(!is.na(fudi)&!is.na(rmw)&!is.na(LFPR_WOMEN)&!is.na(UR_WOMEN)& !is.na(Coord))
xeq<-model[c('LFPR_WOMEN', 'Coord')]
xtr<- model['UR_WOMEN']
xeq<-as.data.frame(xeq)
xtr<-as.data.frame(xtr)
(ecm1 <- ecm(model$fudi, xeq, xtr, includeIntercept = TRUE))
summary(ecm1)

# Desempleo femenino tiene un efecto positivo al corto pzo sobre densidad sindical fem
# Sacar salarios reales. 
# Imputar ¿MNAR o MAR? 