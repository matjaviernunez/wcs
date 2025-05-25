#
rm(list = ls())
#
library(readxl)
library(rio)
library(tidyverse)
#
nombres <- excel_sheets("05_Entregable 3/maate_extra/intermedios/DdV_20_MAATE_Rescates_9mayo2025_res.xlsx") 
nl <- gsub("_Rec.*", "", nombres)
nl

rescate <- import("02_Bases de Datos 2025/MAATE/2025_05_13/Rescates_9mayo2025.xlsx",
                  range = "A2:AA3424")

nor <- names(rescate)
  
for(i in 1:9){
  
  lf <- import("05_Entregable 3/maate_extra/intermedios/DdV_20_MAATE_Rescates_9mayo2025_res.xlsx",
               which = nombres[i]) %>% 
    group_by(!!sym(nl[i]), c25 = `Categoría 2025`) %>% 
    summarise() %>% 
    ungroup()
  
  rescate <- rescate %>% 
    left_join(lf, by = nl[i]) %>% 
    select(-!!nl[i]) %>% 
    rename(!!nl[i] := c25)
    # mutate(!!nl[i] := ifelse(is.na(!!sym(nl[i])), "no_declarado", !!sym(nl[i])))
    
  print(i)
  
}

rescate <- rescate %>% 
  select(any_of(nor))

table(rescate$institucion_rescatista, useNA = "ifany")

rm(lf, i, nl, nombres, nor)

export(rescate, "05_Entregable 3/maate_extra/intermedios/Rescates_9mayo2025_v2.0.xlsx")


