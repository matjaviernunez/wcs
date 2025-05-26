#
rm(list = ls())

library(readxl)
library(rio)
library(tidyverse)
library(chron)
#
nombres <- excel_sheets("05_Entregable 3/maate_extra/intermedios/DdV_19_MAATE_recopilacion_Retenciones_res.xlsx") 
nl <- gsub("_Recopilacion", "", nombres)
nl

retencion <- import("02_Bases de Datos 2025/MAATE/2025_05_13/recopilacion_Retenciones.xlsx",
                  range = "A4:AM621", )

nor <- names(retencion)

retencion <- retencion |> 
  mutate(hora_retencion = format(hora_retencion, "%H:%M"))

for(i in 1:12){
  
  lf <- import("05_Entregable 3/maate_extra/intermedios/DdV_19_MAATE_recopilacion_Retenciones_res.xlsx",
               which = nombres[i]) %>% 
    group_by(!!sym(nl[i]), c25 = `Categoría 2025`) %>% 
    summarise() %>% 
    ungroup()
  
  retencion <- retencion %>% 
    left_join(lf, by = nl[i]) %>% 
    select(-!!nl[i]) %>% 
    rename(!!nl[i] := c25)
  # mutate(!!nl[i] := ifelse(is.na(!!sym(nl[i])), "no_declarado", !!sym(nl[i])))
  
  print(i)
  
}

retencion <- retencion %>% 
  select(any_of(nor)) |> 
  mutate(cantidad = case_when(cantidad == 0 & is.na(nro_total) ~ 1,
                              is.na(cantidad) & is.na(nro_total) ~ 1,
                              T ~ cantidad))

table(retencion$clase)
table(retencion$nombre_cientifico)

rm(lf, i, nl, nombres, nor)

export(retencion, "05_Entregable 3/maate_extra/intermedios/recopilacion_Retenciones_9mayo2025_v2.0.xlsx")
