# Code 00: Proc-Model

#1. Packages
pacman::p_load(WDI,dplyr, tidyverse, zoo, googledrive, googlesheets4)

#2. Cargar base de datos
load(file = "../input/data/database2.RData")

#3. Seleccionar variables
names(db)
db1<- db %>% dplyr::select(country, year,ud=UD,ud_fem2,ud_male2,rmw, T_GDPHRS_V,UR_WOMEN,UR_MEN, LFPR_WOMEN,LFPR_MEN, sector_ser_TT,sector_ser_FE, sector_ser_MA,sector_SER_SEX_T,sector_SER_SEX_F,sector_SER_SEX_M,
              SH_PT_MW,SH_PT_WOMEN, SH_PT_MEN,Coord,AdjCov) %>% mutate(fudi= ud_fem2/ud_male2, f_UR=UR_WOMEN/UR_MEN, f_LFPR=LFPR_WOMEN/LFPR_MEN,f_sector_ser = sector_ser_FE/sector_ser_MA,
                                       f_sector_SER = sector_SER_SEX_F/sector_SER_SEX_M,
                                       f_SH_PT = SH_PT_WOMEN/SH_PT_MEN) %>% filter(year >= 1970, !is.na(fudi)) 


# 4. Analisis casos perdidos
summarytools::view(summarytools::dfSummary(db1), method = "viewer")

# sector_ser_TT (Con más NA) --> sector_SER_SEX_T
# AdjCov muchos NA
#fudi 661 (corregir, deberían ser 661)
# 1528 obs 

#5. Imputation
# impute multiple fudi by ud y LOFC

db1 <- db1 %>% select(country, year, fudi, f_UR, f_LFPR, f_sector_SER, f_SH_PT, Coord, AdjCov, rmw, T_GDPHRS_V, ud)

db1 <- db1 %>%  group_by(country) %>% 
  mutate(f_UR = ifelse(!is.na(fudi)&is.na(f_UR), na.locf(f_UR), f_UR),
         f_LFPR = ifelse(!is.na(fudi)&is.na(f_LFPR), na.locf(f_LFPR), f_LFPR),
         f_SH_PT = ifelse(!is.na(fudi)&is.na(f_SH_PT), na.locf(f_SH_PT), f_SH_PT),
         f_sector_SER = ifelse(!is.na(fudi)&is.na(f_sector_SER), na.locf(f_sector_SER), f_sector_SER),
         Coord = ifelse(!is.na(fudi)&is.na(Coord), na.locf(Coord), Coord),
         AdjCov = ifelse(!is.na(fudi)&is.na(AdjCov), na.locf(AdjCov), AdjCov),
         rmw = ifelse(!is.na(fudi)&is.na(rmw), na.locf(rmw), rmw),
         T_GDPHRS_V = ifelse(!is.na(fudi)&is.na(T_GDPHRS_V), na.locf(T_GDPHRS_V), T_GDPHRS_V),
         ud = ifelse(!is.na(fudi)&is.na(ud), na.locf(ud), ud)) %>%  filter(!is.na(fudi))

impute <- function(x) mutate(x = ifelse(!is.na(fudi)&is.na(x), na.locf(x), x))         


# 6. World Bank source

# World Bank
# Union density and sectors
new_wdi_cache <- WDIcache() 
WDIsearch("female.*unemployment.", cache = new_wdi_cache)

db_aux <- WDI(indicator = c("SL.TLF.PART.FE.ZS",
                            "SL.TLF.PART.MA.ZS",
                            "SL.UEM.TOTL.FE.ZS",
                            "SL.UEM.TOTL.MA.ZS",
                            "SL.TLF.CACT.FE.ZS",
                            "SL.TLF.CACT.MA.ZS",
                            "SL.TLF.PART.FE.ZS", 
                            "SL.TLF.PART.MA.ZS"), start = 1980, end = 2019, extra = TRUE)

names(db_aux)
db_aux <- db_aux %>% mutate(f_SH_PT_a= SL.TLF.PART.FE.ZS/SL.TLF.PART.MA.ZS,
                            f_UR_a= SL.UEM.TOTL.FE.ZS/SL.UEM.TOTL.MA.ZS,
                            f_sector_SER_a= SL.TLF.CACT.FE.ZS/SL.TLF.CACT.MA.ZS,
                            f_LFPR_a= SL.TLF.PART.FE.ZS/SL.TLF.PART.MA.ZS) %>% select(country, year, region,f_SH_PT_a:f_LFPR_a)

db1 <- db1 %>% merge(db_aux, by = c("country", "year"), all.x = T)  %>%
  mutate(f_SH_PT = if_else(is.na(f_SH_PT), f_SH_PT_a, f_SH_PT),
         f_UR = if_else(is.na(f_UR), f_UR_a, f_UR),
         f_sector_SER = if_else(is.na(f_sector_SER), f_sector_SER_a, f_sector_SER),
         f_LFPR = if_else(is.na(f_LFPR), f_LFPR_a, f_LFPR)) %>% select(-c(f_SH_PT_a:f_LFPR_a)) 

db1 <- db1 %>% group_by(country) %>% fill(region, .direction= "up")
# 7. Manual source

db <- db1[!complete.cases(db1),]

# --> To google sheets
#(ss <- gs4_create("tesis", sheets = list(panel = db)))
# head(db) %>% 
#    sheet_write(ss, sheet = "complete")

# 6.1 Read complete

db <- read_sheet("https://docs.google.com/spreadsheets/d/1O4vdhArt6qPR3cV76cuTROomX7c97dbBK3w5Rq6Di2c/edit#gid=353438270")


##6.2 Merge local with googleshetts
db_model <- db1 %>% merge(db, by = c("country", "year", "fudi"), all.x = T)  %>%
  mutate(f_SH_PT = if_else(is.na(f_SH_PT), f_SH_PT_a, na.locf(f_SH_PT)),
         Coord = if_else(is.na(Coord), Coord_a, Coord),
         f_UR = if_else(is.na(f_UR), f_UR_a, f_UR),
         f_sector_SER = if_else(is.na(f_sector_SER), f_sector_SER_a, f_sector_SER),
         f_LFPR = if_else(is.na(f_LFPR), f_LFPR_a, f_LFPR),
         rmw = if_else(is.na(rmw), rmw_a, rmw),
         T_GDPHRS_V = if_else(is.na(T_GDPHRS_V), T_GDPHRS_V_a, T_GDPHRS_V),
         ud = if_else(is.na(ud), ud_a, ud),
         AdjCov = if_else(is.na(AdjCov), AdjCov_a, AdjCov),
         region = if_else(is.na(region), region_a, region)) %>%  select(-c(f_UR_a:region_a))  %>% group_by(country) %>% 
  fill(f_SH_PT, .direction = "up")

# Guardar para ocupar en modelos
save(db_model, file = "../output/data/data-model.RData")
