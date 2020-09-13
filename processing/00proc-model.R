# Code 00: Proc-Model

#1. Packages
pacman::p_load(dplyr, tidyverse, zoo, googledrive, googlesheets4)

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

a <- db1[!complete.cases(db1),]

gs_auth(new_user = TRUE)


gs_new(title = "tesis", input = a)

drive_mv("tesis", path = "~/R/Demo/")

(ss <- gs4_create("fluffy-bunny", sheets = list(flowers = head(iris))))
