# Code 00: Proc-Model

#1. Packages
pacman::p_load(dplyr, tidyverse, VIM)

#2. Cargar base de datos
load(file = "../input/data/database2.RData")

#3. Seleccionar variables
names(db)
db1<- db %>% select(country, year,ud=UD,ud_fem2,ud_male2,rmw,gwg, T_GDPHRS_V,UR_WOMEN,UR_MEN, 
              LFPR_WOMEN,LFPR_MEN, sector_ser_TT,sector_ser_FE, sector_ser_MA,sector_SER_SEX_T,sector_SER_SEX_F,sector_SER_SEX_M,
              SH_PT_MW,SH_PT_WOMEN, SH_PT_MEN,
              Coord,AdjCov) %>% mutate(fudi= ud_fem2/ud_male2, f_UR=UR_WOMEN/UR_MEN, f_LFPR=LFPR_WOMEN/LFPR_MEN,f_sector_ser = sector_ser_FE/sector_ser_MA,
                                       f_sector_SER = sector_SER_SEX_F/sector_SER_SEX_M,
                                       f_SH_PT = SH_PT_WOMEN/SH_PT_MEN) %>% filter(year >= 1970)


# 4. Analisis casos perdidos
summarytools::view(summarytools::dfSummary(db1), method = "viewer")

# sector_ser_TT (Con más NA) --> sector_SER_SEX_T
# AdjCov muchos NA