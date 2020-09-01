# Code 00: Proc-Model

#1. Packages
pacman::p_load(dplyr, tidyverse, VIM)

#2. Cargar base de datos
load(file = "../input/data/database2.RData")

#3. Seleccionar variables
names(db)
db %>% select(country, year, fudi,ud,ud_fem2,ud_male2,rmw,gwg, T_GDPHRS_V, Coord,
              sector_ser_TT)