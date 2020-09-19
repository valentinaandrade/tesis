# Chapter 3:  ECM

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

# Model 0 ---------------
#Models correct for panel-specific, first-order autocorrelation
model0 <-plm(
  fudi ~ diff(f_UR) + f_LFPR + f_sector_SER + f_SH_PT + Coord +
    T_GDPHRS_V  + rmw,
  index = c("country", "year"),
  model = "w",
  effect = "time",
  data = db_model)
summary(model0)

# Cluster errores ---
coeftest(model0, vcov=function(x)vcovBK(x, type="HC1", cluster="time"))

screenreg(list(model0, coeftest(model0,
                                vcov=function(x)vcovBK(x, type="HC1", cluster="time"))),
               custom.model.names = c('Model 0 SE', 'Model 0 Panel-Corrected Standard Error'))

# Camban pues los errores son heterocedasticos 

# Panel Vector Autorregresive model -----
model01 <-plm(
  fudi ~ diff(f_UR) + f_LFPR + f_sector_SER + f_SH_PT + Coord +
    T_GDPHRS_V  + rmw + lag(fudi),
  index = c("country", "year"),
  model = "w",
  effect = "time",
  data = db_model)
summary(model01)

# Unconditional Robust covariance matrix estimators a la Beck and Katz for panel models (a.k.a. Panel Corrected Standard Errors (PCSE)). ----
coeftest(model01, vcov=function(x)vcovBK(x, type="HC1", cluster="time"))

screenreg(list(model01, coeftest(model01,
                                vcov=function(x)vcovBK(x, type="HC1", cluster="time"))),
          custom.model.names = c('PVAM - SE', 'PVAM-Corrected Standard Error'))


# Model 2:  Autorregresive model - Error Correction Model -----
model02 <-plm(
  diff(fudi) ~ diff(f_UR) + diff(T_GDPHRS_V)  + diff(rmw) + lag(f_LFPR) + lag(f_sector_SER) + lag(f_SH_PT) + lag(Coord)  + lag(fudi),
  index = c("country", "year"),
  model = "w",
  effect = "time",
  data = db_model)
summary(model02)

# Unconditional Robust covariance matrix estimators a la Beck and Katz for panel models (a.k.a. Panel Corrected Standard Errors (PCSE)). ----
coeftest(model02, vcov=function(x)vcovBK(x, type="HC1", cluster="time"))


screenreg(list(model02, coeftest(model02,
                                 vcov=function(x)vcovBK(x, type="HC1", cluster="time"))),
          custom.model.names = c('PVECM - SE', 'PVECM-Corrected Standard Error'))


# Modelo para AL y Europa
# Los paneles no balanceados no por razones aleatorias. 
db_model3 <- filter(db_model, region == "Latin America & Caribbean")
db_model4 <- filter(db_model, region == "Europe & Central Asia")

model03 <-plm(
  diff(fudi) ~ diff(f_UR) + diff(T_GDPHRS_V)  + diff(rmw) + lag(f_LFPR) + lag(f_sector_SER) + lag(f_SH_PT) + lag(Coord)  + lag(fudi),
  index = c("country", "year"),
  model = "w",
  effect = "twoways",
  data = db_model3)
summary(model03)

model04 <-plm(
  diff(fudi) ~ diff(f_UR) + diff(T_GDPHRS_V)  + diff(rmw) + lag(f_LFPR) + lag(f_sector_SER) + lag(f_SH_PT) + lag(Coord)  + lag(fudi),
  index = c("country", "year"),
  model = "w",
  effect = "twoways",
  data = db_model4)
summary(model04)

# Unconditional Robust covariance matrix estimators a la Beck and Katz for panel models (a.k.a. Panel Corrected Standard Errors (PCSE)). ----
screenreg(list(coeftest(model04,
                        vcov=function(x)vcovBK(x, type="HC1", cluster="time")), 
                        coeftest(model03,
                                 vcov=function(x)vcovBK(x, type="HC1", cluster="time"))),
          custom.model.names = c('Europa PVECM', 'América Latina PVECM'))


#Modelo por países ----
table(db$country)
# Países con mayor n 
## Liberales
# United Kingdom 42
# Chile 17
# Brazil

## Coordinados
# Suecia
# Netherlands
# Switzerland
# Austria
# Japon

# 3. Construir modelo por países segun resultados
# Coordinados-------
## Germany (ecm_g) ----
db1 <- db %>% filter(country == "Germany")
model <- db1 
xeq<-model[c('f_LFPR','f_sector_SER','f_SH_PT', 'Coord')]
xtr<- model[c('f_UR', 'rmw','T_GDPHRS_V', 'AdjCov')]
xeq<-as.data.frame(xeq)
xtr<-as.data.frame(xtr)
(ecm_g <- ecm(model$fudi, xeq, xtr, includeIntercept = TRUE))
summary(ecm_g)

## Sweden (ecm_s) ----
db1 <- db %>% filter(country == "Sweden")
model <- db1 
xeq<-model[c('f_LFPR','f_sector_SER','f_SH_PT', 'Coord')]
xtr<- model[c('f_UR', 'rmw','T_GDPHRS_V', 'AdjCov')]
xeq<-as.data.frame(xeq)
xtr<-as.data.frame(xtr)
(ecm_s <- ecm(model$fudi, xeq, xtr, includeIntercept = TRUE))
summary(ecm_s)

# Netherlands (ecm_n) -------
db1 <- db %>% filter(country == "Netherlands")
model <- db1 
xeq<-model[c('f_LFPR','f_sector_SER','f_SH_PT', 'Coord')]
xtr<- model[c('f_UR', 'rmw','T_GDPHRS_V', 'AdjCov')]
xeq<-as.data.frame(xeq)
xtr<-as.data.frame(xtr)
(ecm_n <- ecm(model$fudi, xeq, xtr, includeIntercept = TRUE))
summary(ecm_n)

# Switzerland (ecm_sw) ------------
db1 <- db %>% filter(country == "Switzerland")
model <- db1 
xeq<-model[c('f_LFPR','f_sector_SER','f_SH_PT', 'Coord')]
xtr<- model[c('f_UR', 'rmw','T_GDPHRS_V', 'AdjCov')]
xeq<-as.data.frame(xeq)
xtr<-as.data.frame(xtr)
(ecm_sw <- ecm(model$fudi, xeq, xtr, includeIntercept = TRUE))
summary(ecm_sw)

## Autria (ecm_au) ----
db1 <- db %>% filter(country == "Austria")
model <- db1 
xeq<-model[c('f_LFPR','f_sector_SER','f_SH_PT', 'Coord')]
xtr<- model[c('f_UR', 'rmw','T_GDPHRS_V', 'AdjCov')]
xeq<-as.data.frame(xeq)
xtr<-as.data.frame(xtr)
(ecm_au <- ecm(model$fudi, xeq, xtr, includeIntercept = TRUE))
summary(ecm_au)

## Japan (ecm_j)---
db1 <- db %>% filter(country == "Japan")
model <- db1 
xeq<-model[c('f_LFPR','f_sector_SER','f_SH_PT', 'Coord')]
xtr<- model[c('f_UR', 'rmw','T_GDPHRS_V', 'AdjCov')]
xeq<-as.data.frame(xeq)
xtr<-as.data.frame(xtr)
(ecm_j <- ecm(model$fudi, xeq, xtr, includeIntercept = TRUE))
summary(ecm_j)

## Denmark (ecm_dk) ----
table(db$country)
db1 <- db %>% filter(country == "Denmark")
model <- db1 
xeq<-model[c('f_LFPR','f_sector_SER','f_SH_PT', 'Coord')]
xtr<- model[c('f_UR', 'rmw','T_GDPHRS_V', 'AdjCov')]
xeq<-as.data.frame(xeq)
xtr<-as.data.frame(xtr)
(ecm_dk <- ecm(model$fudi, xeq, xtr, includeIntercept = TRUE))
summary(ecm_dk)

# Iceland (ecm_ic) -----
db1 <- db %>% filter(country == "Iceland")
model <- db1 
xeq<-model[c('f_LFPR','f_sector_SER','f_SH_PT', 'Coord')]
xtr<- model[c('f_UR', 'rmw','T_GDPHRS_V', 'AdjCov')]
xeq<-as.data.frame(xeq)
xtr<-as.data.frame(xtr)
(ecm_ic <- ecm(model$fudi, xeq, xtr, includeIntercept = TRUE))
summary(ecm_ic)
### f_LFPR

# Liberales -----------

## UK (ecm_uk) ----
db1 <- db %>% filter(country == "United Kingdom")
model <- db1 
xeq<-model[c('f_LFPR','f_sector_SER','f_SH_PT', 'Coord')]
xtr<- model[c('f_UR', 'rmw','T_GDPHRS_V', 'AdjCov')]
xeq<-as.data.frame(xeq)
xtr<-as.data.frame(xtr)
(ecm_uk <- ecm(model$fudi, xeq, xtr, includeIntercept = TRUE))
summary(ecm_uk)

# Ireland (ecm_ir) ------
db1 <- db %>% filter(country == "Ireland")
model <- db1 
xeq<-model[c('f_LFPR','f_sector_SER','f_SH_PT', 'Coord')]
xtr<- model[c('f_UR', 'rmw','T_GDPHRS_V', 'AdjCov')]
xeq<-as.data.frame(xeq)
xtr<-as.data.frame(xtr)
(ecm_ir <- ecm(model$fudi, xeq, xtr, includeIntercept = TRUE))
summary(ecm_ir)

# New Zealand (ecm_nw) ----
db1 <- db %>% filter(country == "New Zealand")
model <- db1 
xeq<-model[c('f_LFPR','f_sector_SER','f_SH_PT', 'Coord')]
xtr<- model[c('f_UR', 'rmw','T_GDPHRS_V', 'AdjCov')]
xeq<-as.data.frame(xeq)
xtr<-as.data.frame(xtr)
(ecm_nw <- ecm(model$fudi, xeq, xtr, includeIntercept = TRUE))
summary(ecm_nw)

# Chile (ecm_cl) ------
db1 <- db %>% filter(country == "Chile")
model <- db1 
xeq<-model[c('f_LFPR','f_sector_SER','f_SH_PT', 'Coord')]
xtr<- model[c('f_UR', 'rmw','T_GDPHRS_V', 'AdjCov')]
xeq<-as.data.frame(xeq)
xtr<-as.data.frame(xtr)
(ecm_cl <- ecm(model$fudi, xeq, xtr, includeIntercept = TRUE))
summary(ecm_cl)

# Brazil (ecm_br) ---
db1 <- db %>% filter(country == "Brazil")
model <- db1 
xeq<-model[c('f_LFPR','f_sector_SER','f_SH_PT', 'Coord')]
xtr<- model[c('f_UR', 'rmw','T_GDPHRS_V', 'AdjCov')]
xeq<-as.data.frame(xeq)
xtr<-as.data.frame(xtr)
(ecm_br <- ecm(model$fudi, xeq, xtr, includeIntercept = TRUE))
summary(ecm_br)

# Canada (ecm_cd) ----
db1 <- db %>% filter(country == "Canada")
model <- db1 
xeq<-model[c('f_LFPR','f_sector_SER','f_SH_PT', 'Coord')]
xtr<- model[c('f_UR', 'rmw','T_GDPHRS_V', 'AdjCov')]
xeq<-as.data.frame(xeq)
xtr<-as.data.frame(xtr)
(ecm_cd <- ecm(model$fudi, xeq, xtr, includeIntercept = TRUE))
summary(ecm_cd)

# Mexico (ecm_mx) ----
db1 <- db %>% filter(country == "Mexico")
model <- db1 
xeq<-model[c('f_LFPR','f_sector_SER','f_SH_PT', 'Coord')]
xtr<- model[c('f_UR', 'rmw','T_GDPHRS_V', 'AdjCov')]
xeq<-as.data.frame(xeq)
xtr<-as.data.frame(xtr)
(ecm_mx <- ecm(model$fudi, xeq, xtr, includeIntercept = TRUE))
summary(ecm_mx)

## No fem

#United States (ecm_us) ------
db1 <- db %>% filter(country == "United States")
model <- db1 
xeq<-model[c('f_LFPR','f_sector_SER','f_SH_PT', 'Coord')]
xtr<- model[c('f_UR', 'rmw','T_GDPHRS_V', 'AdjCov')]
xeq<-as.data.frame(xeq)
xtr<-as.data.frame(xtr)
(ecm_us <- ecm(model$fudi, xeq, xtr, includeIntercept = TRUE))
summary(ecm_us)

# Korea (ecm_k) -----
db1 <- db %>% filter(country == "Korea, Republic of")
model <- db1 
xeq<-model[c('f_LFPR','f_sector_SER','f_SH_PT', 'Coord')]
xtr<- model[c('f_UR', 'rmw','T_GDPHRS_V', 'AdjCov')]
xeq<-as.data.frame(xeq)
xtr<-as.data.frame(xtr)
(ecm_k <- ecm(model$fudi, xeq, xtr, includeIntercept = TRUE))
summary(ecm_k)

# South Africa (ecm_sa)-----
db1 <- db %>% filter(country == "South Africa")
model <- db1 
xeq<-model[c('f_LFPR','f_sector_SER','f_SH_PT', 'Coord')]
xtr<- model[c('f_UR', 'rmw','T_GDPHRS_V', 'AdjCov')]
xeq<-as.data.frame(xeq)
xtr<-as.data.frame(xtr)
(ecm_sa <- ecm(model$fudi, xeq, xtr, includeIntercept = TRUE))
summary(ecm_sa)


# Resumen ---
ecm_models<-grep("ecm",names(.GlobalEnv),value=TRUE)
ecm_models<-do.call("list",mget(ecm_models))
screenreg(l = list(ecm_g, ecm_s, ecm_n, ecm_sw, ecm_au, ecm_dk, ecm_ic), 
          custom.model.names = c("Alemania","Suecia","Paísez Bajos",
                                 "Suiza", "Austria", "Dinamarca", "Islandia"))

screenreg(l = list(ecm_uk, ecm_us, ecm_ir, ecm_nw, ecm_cd, ecm_cl, ecm_mx, ecm_sa, ecm_k), 
          custom.model.names = c("Reino Unido","Estados Unidos","Irlanda",
                                 "Nueva Zelanda", "Canadá", "Chile", "México", "Sudáfrica", "Korea"))

# Analisis por coef.
coefs<-lapply(ecm_models,function(x)coef(x))
unlist(coefs)
sig<-lapply(ecm_models,function(x)effects(x))
x <- as.data.frame(unlist(coefs))
x <- rownames_to_column(x, "variable")
y <- as.data.frame(unlist(sig))
y <- rownames_to_column(y, "variable")
z <- merge(x,y, by = "variable")
names(z)

f_UR <- z %>% 
  rename("variable"="variable", "sig" = "unlist(sig)","coef"="unlist(coefs)") %>% 
  filter(abs(sig)<= 0.05) %>% filter(str_detect(variable, "f_UR")) %>% filter(coef == max(coef))
f_rmw <- z %>% 
  rename("variable"="variable", "sig" = "unlist(sig)","coef"="unlist(coefs)") %>% 
  filter(abs(sig)<= 0.05) %>% filter(str_detect(variable, "rmw")) %>% filter(coef == max(coef))

f_T_GDPHRS_V <- z %>% 
  rename("variable"="variable", "sig" = "unlist(sig)","coef"="unlist(coefs)") %>% 
  filter(abs(sig)<= 0.05) %>% filter(str_detect(variable, "T_GDPHRS_V")) %>% filter(coef == max(coef))

f_LFPR <- z %>% 
  rename("variable"="variable", "sig" = "unlist(sig)","coef"="unlist(coefs)") %>% 
  filter(abs(sig)<= 0.05) %>% filter(str_detect(variable, "f_LFPR")) %>% filter(coef == max(coef))

f_sector_SER <- z %>% 
  rename("variable"="variable", "sig" = "unlist(sig)","coef"="unlist(coefs)") %>% 
  filter(abs(sig)<= 0.05) %>% filter(str_detect(variable, "f_sector_SER")) %>% filter(coef == max(coef))

f_SH_PT <- z %>% 
  rename("variable"="variable", "sig" = "unlist(sig)","coef"="unlist(coefs)") %>% 
  filter(abs(sig)<= 0.05) %>% filter(str_detect(variable, "f_SH_PT")) %>% filter(coef == max(coef))

f_Coord <- z %>% 
  rename("variable"="variable", "sig" = "unlist(sig)","coef"="unlist(coefs)") %>% 
  filter(abs(sig)<= 0.05) %>% filter(str_detect(variable, "Coord")) %>% filter(coef == max(coef))

f_AdjCov <- z %>% 
  rename("variable"="variable", "sig" = "unlist(sig)","coef"="unlist(coefs)") %>% 
  filter(abs(sig)<= 0.05) %>% filter(str_detect(variable, "AdjCov")) %>% filter(coef == max(coef))

f_ud <- z %>% 
  rename("variable"="variable", "sig" = "unlist(sig)","coef"="unlist(coefs)") %>% 
  filter(abs(sig)<= 0.1) %>% filter(str_detect(variable, "ud")) %>% filter(coef == max(coef))

ecm_models<-grep("f_",names(.GlobalEnv),value=TRUE)
ecm_models<-do.call("list",mget(ecm_models))
ecm_models_max <- bind_rows(ecm_models)

f_UR <- z %>% 
  rename("variable"="variable", "sig" = "unlist(sig)","coef"="unlist(coefs)") %>% 
  filter(abs(sig)<= 0.05) %>% filter(str_detect(variable, "f_UR")) %>% filter(coef == min(coef))
f_rmw <- z %>% 
  rename("variable"="variable", "sig" = "unlist(sig)","coef"="unlist(coefs)") %>% 
  filter(abs(sig)<= 0.05) %>% filter(str_detect(variable, "rmw")) %>% filter(coef == min(coef))

f_T_GDPHRS_V <- z %>% 
  rename("variable"="variable", "sig" = "unlist(sig)","coef"="unlist(coefs)") %>% 
  filter(abs(sig)<= 0.05) %>% filter(str_detect(variable, "T_GDPHRS_V")) %>% filter(coef == min(coef))

f_LFPR <- z %>% 
  rename("variable"="variable", "sig" = "unlist(sig)","coef"="unlist(coefs)") %>% 
  filter(abs(sig)<= 0.05) %>% filter(str_detect(variable, "f_LFPR")) %>% filter(coef == min(coef))

f_sector_SER <- z %>% 
  rename("variable"="variable", "sig" = "unlist(sig)","coef"="unlist(coefs)") %>% 
  filter(abs(sig)<= 0.05) %>% filter(str_detect(variable, "f_sector_SER")) %>% filter(coef == min(coef))

f_SH_PT <- z %>% 
  rename("variable"="variable", "sig" = "unlist(sig)","coef"="unlist(coefs)") %>% 
  filter(abs(sig)<= 0.05) %>% filter(str_detect(variable, "f_SH_PT")) %>% filter(coef == min(coef))

f_Coord <- z %>% 
  rename("variable"="variable", "sig" = "unlist(sig)","coef"="unlist(coefs)") %>% 
  filter(abs(sig)<= 0.05) %>% filter(str_detect(variable, "Coord")) %>% filter(coef == min(coef))

f_AdjCov <- z %>% 
  rename("variable"="variable", "sig" = "unlist(sig)","coef"="unlist(coefs)") %>% 
  filter(abs(sig)<= 0.05) %>% filter(str_detect(variable, "AdjCov")) %>% filter(coef == min(coef))

f_ud <- z %>% 
  rename("variable"="variable", "sig" = "unlist(sig)","coef"="unlist(coefs)") %>% 
  filter(abs(sig)<= 0.1) %>% filter(str_detect(variable, "ud")) %>% filter(coef == min(coef))

ecm_models<-grep("f_",names(.GlobalEnv),value=TRUE)
ecm_models<-do.call("list",mget(ecm_models))

# Guardar modelos
save(model0, model01,model02,model03,model04, ecm_g, ecm_s, ecm_n, ecm_sw, ecm_au, ecm_dk, ecm_ic,
     ecm_uk, ecm_us, ecm_ir, ecm_nw, ecm_cd, ecm_cl, ecm_mx, ecm_sa, ecm_k, ecm_models_max, ecm_models_min, 
     file = "../output/data/models.RData")
