# Procesamiento ----
# Valentina Andrade

#1. Packages
pacman::p_load(dplyr, ggplot2, ggsci, tidyr, readxl)


#lead() next and lag() past; first() el primero

#2. Cargar bases de datos
load(file = "../input/data/database2.RData")

# 3. Explorar base
names(db)

## Limpiar duplicados
db <- db %>% distinct(country,year, .keep_all=T)

## Crear variables -------------

# 1. Crear variable continente
db <- db %>%
  mutate(continent = ifelse(country %in% c("Argentina", "Brazil", "Chile", "Colombia", "Costa Rica", "Mexico", "Uruguay"),"Latinoamérica",
                                       ifelse(country  %in% c("United States","Canada"),"Norte América",
                                              ifelse(country %in% c("Austria", "Belgium","Bulgaria","Croatia",
                                                                    "Cyprus", "Czech Republic", "Denmark",
                                                                    "Estonia", "Finland", "France", "Germany",
                                                                    "Greece","Hungary", "Ireland", "Italy",
                                                                    "Latvia", "Lithuania", "Luxembourg","Malta",
                                                                    "Netherlands", "Norway", "Poland","Portugal","Romania", "Slovak Republic",
                                                                    "Slovenia", "Spain", "Sweden", "Switzerland",
                                                                    "United Kingdom"), "Europa",
                                                     ifelse(country %in% c("Australia", "Iceland","New Zealand"), "Oceanía",
                                                            ifelse(country %in% c("China", "Hong Kong, China",
                                                                                  "India", "Israel", "Japan",
                                                                                  "Korea, Republic of",
                                                                                  "Malaysia", "Russian Federation",
                                                                                  "Singapore", "Taiwan, China","Turkey", "Indonesia", "Philippines"), "Asia", "África"))))))

# 2. Crear variable OCDE
db <- db %>% 
  mutate(ocde = ifelse(country %in% c("Australia", "Austria", "Belgium", "Canada", "Chile", "Colombia", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany",
                                                      "Greece","Hungary", "Iceland", "Ireland", "Israel", "Italy", "Japan", "Korea", "Latvia", "Lithuania", "Luxembourg",
                                                      "Mexico", "Netherlands", "Norway", "Poland","Portugal", "Slovak Republic",
                                                      "Slovenia", "Spain", "Sweden", "Switzerland",
                                                      "United Kingdom","Turkey", "United States"), "OCDE", "NO OCDE"))
db[, c("country", "ocde")]

#3. Variables lag
db <- db %>%  group_by(country) %>%  
  mutate(growth_ud = UD - lag(UD),
         growth_udpercent = growth_ud/lag(UD) * 100,
         growth_udfem = ud_fem2 - lag(ud_fem2),
         growth_udfempercent = growth_udfem/lag(ud_fem2) * 100,
         growth_udmale = ud_male2 - lag(ud_male2),
         growth_udmalepercent = growth_udmale/lag(ud_male2) * 100,
         growth_e = EPR_MW - lag(EPR_MW),
         growth_epercent = growth_e/lag(EPR_MW) * 100,
         growth_p = T_GDPHRS_V - lag(T_GDPHRS_V),
         growth_ppercent = growth_p/lag(T_GDPHRS_V) * 100,
         growth_rmw = rmw - lag(rmw),
         growth_rmwpercent = growth_rmw/lag(rmw) * 100)

# 4. Variables VoC
# En base a Hall y Soskice (2001). Estados Bálticos (LMEs), Mediterraneos, Nórdicos, Centrales
db <- db %>%
  mutate(voc = ifelse(country %in% c("Argentina", "Brazil", "Chile", "Colombia", "Costa Rica", "Mexico", "Uruguay"),"HLEs",
                            ifelse(country  %in% c("United States","Canada", "United Kingdom", "Australia", "Canada", "New Zealand", "Ireland", "Estonia",
                                                   "Hungary", "Romania", "Poland", "Russian Federation", "Israel", "Slovak Republic", "Korea, Republic of", "Latvia", "Lithuania"),"LMEs",
                                   ifelse(country %in% c("Austria", "Belgium", "Denmark", "Finland", "Iceland","Japan", "Germany",
                                                         "Netherlands", "Norway",  "Sweden", "Switzerland", "Slovenia", "Luxembourg", "Czech Republic"), "CMEs",
                                                 ifelse(country %in% c("France", "Italy", "Spain","Portugal","Greece","Turkey"), "Ambiguos", "No-Se")))))

## Imputar ----------
# 6. Imputar datos
ud_country <- read_excel("../input/data/excel/ud-country.xlsx", 
                         sheet = "USA", skip=1)

db <- db %>% left_join(ud_country, by = c("country", "year"), all.x = T) %>% 
  mutate(UD = if_else(is.na(UD), ratio...12, UD)) %>% dplyr::select(iso3c:growth_rmwpercent)

remove(ud_country)

# NA in isoc3
db <- db %>% mutate(iso3c = if_else(is.na(iso3c)&country=="Uruguay", "URY",
                              if_else(is.na(iso3c)&country=="Slovak Republic", "SVK",
                                      if_else(is.na(iso3c)&country=="Czech Republic", "CZE",iso3c))))
# 8. Guardar ---
save(db, file ="../input/data/db-proc.RData")
rm(list = ls())
