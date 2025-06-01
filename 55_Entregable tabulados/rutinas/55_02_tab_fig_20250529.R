rm(list = ls())

library(tidyverse)
library(rio)
library(sf)

cites <- readRDS("55_Entregable tabulados/intermedios/03_cites.rds")
maate <- import("05_Entregable 3/maate_extra/intermedios/recopilacion_Retenciones_9mayo2025_v2.0.xlsx")

tab_01_con_fau_eve_maate <- maate %>% 
  group_by(anio = substr(fecha_retencion, 1, 4)) %>% 
  summarise(eventos = n_distinct(codigo_acta))


