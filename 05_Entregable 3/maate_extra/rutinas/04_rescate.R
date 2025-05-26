#
rm(list = ls())
#
library(readxl)
library(rio)
library(lubridate)
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

## Tratamiento de la variable hora

table(nchar(rescate$hora_rescate), useNA = "ifany")

rescate2 <- rescate %>%
  mutate(hora_rescate = str_to_lower(hora_rescate),
         hora_rescate = str_replace_all(hora_rescate, "h", ":"),
         hora_rescate = str_replace_all(hora_rescate, "\\s+", ""),
         hora_rescate = str_replace_all(hora_rescate, "\\.", ""),
         hora_rescate = parse_date_time(hora_rescate,
                         orders = c("I:M:S p", "I:M p", "H:M:S", "HMS", "HM", "I:Mp", "I:Mp", "H:M"),
                         tz = "UTC"),
         hora_rescate = format(hora_rescate, "%H:%M"),
         # fecha rescate
         fecha_rescate = str_replace_all(fecha_rescate, "\\s+", ""),
         fecha_rescate = str_replace_all(fecha_rescate, "\\.", ""),
         fecha_rescate = str_replace_all(fecha_rescate, "ene", "01"),
         fecha_rescate = str_replace_all(fecha_rescate, "feb", "02"),
         fecha_rescate = str_replace_all(fecha_rescate, "mar", "03"),
         fecha_rescate = str_replace_all(fecha_rescate, "abr", "04"),
         fecha_rescate = str_replace_all(fecha_rescate, "may", "05"),
         fecha_rescate = str_replace_all(fecha_rescate, "jun", "06"),
         fecha_rescate = str_replace_all(fecha_rescate, "jul", "07"),
         fecha_rescate = str_replace_all(fecha_rescate, "ago", "08"),
         fecha_rescate = str_replace_all(fecha_rescate, "sept", "09"),
         fecha_rescate = str_replace_all(fecha_rescate, "oct", "10"),
         fecha_rescate = str_replace_all(fecha_rescate, "nov", "11"),
         fecha_rescate = str_replace_all(fecha_rescate, "dic", "12"),
         fecha_rescate = str_replace_all(fecha_rescate, "26sep9/2022", "26/09/2022"),
         fecha_rescate = dmy(fecha_rescate),
         # fecha destino
         fecha_destino = str_replace_all(fecha_destino, "il", ""),
         fecha_destino = str_replace_all(fecha_destino, "\\s+", ""),
         fecha_destino = str_replace_all(fecha_destino, "\\.", ""),
         fecha_destino = str_replace_all(fecha_destino, "ene", "01"),
         fecha_destino = str_replace_all(fecha_destino, "feb", "02"),
         fecha_destino = str_replace_all(fecha_destino, "mar", "03"),
         fecha_destino = str_replace_all(fecha_destino, "abr", "04"),
         fecha_destino = str_replace_all(fecha_destino, "may", "05"),
         fecha_destino = str_replace_all(fecha_destino, "jun", "06"),
         fecha_destino = str_replace_all(fecha_destino, "jul", "07"),
         fecha_destino = str_replace_all(fecha_destino, "ago", "08"),
         fecha_destino = str_replace_all(fecha_destino, "sept", "09"),
         fecha_destino = str_replace_all(fecha_destino, "oct", "10"),
         fecha_destino = str_replace_all(fecha_destino, "nov", "11"),
         fecha_destino = str_replace_all(fecha_destino, "dic", "12"),
         fecha_destino = str_replace_all(fecha_destino, "15/ju/2023", "15/07/2023"),
         fecha_destino = dmy(fecha_destino),
         # codigo acta perdido
         apoyo = ifelse(is.na(codigo_acta), 1, 99),
         rno = row_number()) %>%
  arrange(apoyo, rno) %>% 
  mutate(nnn = row_number()) %>% 
  ungroup() %>% 
  mutate(codigo_acta = ifelse(!is.na(codigo_acta), codigo_acta,
                              paste0("sin-codigo-", str_pad(nnn, 3, "left", "0")))) %>% 
  select(-apoyo, -rno, -nnn)


export(rescate, "05_Entregable 3/maate_extra/intermedios/Rescates_9mayo2025_v2.0.xlsx")


