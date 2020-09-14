# Chapter 3:  Test de cointegración en R
# Si esas dos variables no pasan la prueba de cointegración
# Se testeará si los residuales son estacionarios o no. Vemos descriptivamente que tienen tendencia 
# Es probable que sean estacionarios

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

# 3. Crear variables de series de tiempo
modelo1 <- lm(fudi ~  f_UR + f_LFPR + f_sector_SER + f_SH_PT + Coord + AdjCov + rmw + T_GDPHRS_V + ud, data = db)
summary(modelo1) # ¿? country y 90
residuales=modelo1$residuals
summary(residuales)
mod1 <- augment(modelo1)
ggplot(mod1, aes(x = .fitted, y = .resid)) + geom_point()

# 4. Probar si son estacionarios
# 4.1 
adf.test(residuales) # Dikey Fuller Test
## Estacionaria

#4.2 Dikey Fuller
dktest = ur.df(residuales, type = "trend", selectlags = "AIC")
summary(dktest)
## Se rechaza hipotesis nula: es estacionaria, no hay raiz unitaria, 
## Cointegración posible

# 4.3. Phillips y Ouliaris test cointegration
db <- db %>%  dplyr::select(fudi,f_LFPR)
test <- db[,-1]
tseries::po.test(test, demean = T, lshort = T) # p=0.01 #Rechazamos H0
urca::ca.po(test, demean = "constant", lag = "short", type = "Pz") # Rechazamos H0

# Es cointegrada 

# Conclusion
# 1. Hay que hacer un ECm
# 2. Hay que agregar la variable del

# Multicolinealidad
car::vif(modelo1)

# Multicolineal con ud