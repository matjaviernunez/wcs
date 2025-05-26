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
  mutate(hora_rescate_imp = str_to_lower(hora_rescate),
         hora_rescate_imp = str_replace_all(hora_rescate_imp, "h", ":"),
         hora_rescate_imp = str_replace_all(hora_rescate_imp, "\\s+", ""),
         hora_rescate_imp = str_replace_all(hora_rescate_imp, "\\.", ""),
         hora_rescate_imp = parse_date_time(hora_rescate_imp,
                         orders = c("I:M:S p", "I:M p", "H:M:S", "HMS", "HM", "I:Mp", "I:Mp", "H:M"),
                         tz = "UTC"),
         hora_rescate_imp = format(hora_rescate_imp, "%H:%M"),
         # fecha rescate
         fecha_rescate_imp = str_replace_all(fecha_rescate, "\\s+", ""),
         fecha_rescate_imp = str_replace_all(fecha_rescate_imp, "\\.", ""),
         fecha_rescate_imp = str_replace_all(fecha_rescate_imp, "ene", "01"),
         fecha_rescate_imp = str_replace_all(fecha_rescate_imp, "feb", "02"),
         fecha_rescate_imp = str_replace_all(fecha_rescate_imp, "mar", "03"),
         fecha_rescate_imp = str_replace_all(fecha_rescate_imp, "abr", "04"),
         fecha_rescate_imp = str_replace_all(fecha_rescate_imp, "may", "05"),
         fecha_rescate_imp = str_replace_all(fecha_rescate_imp, "jun", "06"),
         fecha_rescate_imp = str_replace_all(fecha_rescate_imp, "jul", "07"),
         fecha_rescate_imp = str_replace_all(fecha_rescate_imp, "ago", "08"),
         fecha_rescate_imp = str_replace_all(fecha_rescate_imp, "sept", "09"),
         fecha_rescate_imp = str_replace_all(fecha_rescate_imp, "oct", "10"),
         fecha_rescate_imp = str_replace_all(fecha_rescate_imp, "nov", "11"),
         fecha_rescate_imp = str_replace_all(fecha_rescate_imp, "dic", "12"),
         fecha_rescate_imp = str_replace_all(fecha_rescate_imp, "26sep9/2022", "26/09/2022"),
         fecha_rescate_imp = dmy(fecha_rescate_imp),
         # fecha destino
         fecha_destino_imp = str_replace_all(fecha_destino, "il", ""),
         fecha_destino_imp = str_replace_all(fecha_destino_imp, "\\s+", ""),
         fecha_destino_imp = str_replace_all(fecha_destino_imp, "\\.", ""),
         fecha_destino_imp = str_replace_all(fecha_destino_imp, "ene", "01"),
         fecha_destino_imp = str_replace_all(fecha_destino_imp, "feb", "02"),
         fecha_destino_imp = str_replace_all(fecha_destino_imp, "mar", "03"),
         fecha_destino_imp = str_replace_all(fecha_destino_imp, "abr", "04"),
         fecha_destino_imp = str_replace_all(fecha_destino_imp, "may", "05"),
         fecha_destino_imp = str_replace_all(fecha_destino_imp, "jun", "06"),
         fecha_destino_imp = str_replace_all(fecha_destino_imp, "jul", "07"),
         fecha_destino_imp = str_replace_all(fecha_destino_imp, "ago", "08"),
         fecha_destino_imp = str_replace_all(fecha_destino_imp, "sept", "09"),
         fecha_destino_imp = str_replace_all(fecha_destino_imp, "oct", "10"),
         fecha_destino_imp = str_replace_all(fecha_destino_imp, "nov", "11"),
         fecha_destino_imp = str_replace_all(fecha_destino_imp, "dic", "12"),
         fecha_destino_imp = str_replace_all(fecha_destino_imp, "15/ju/2023", "15/07/2023"),
         fecha_destino_imp = dmy(fecha_destino_imp),
         # codigo acta perdido
         apoyo = ifelse(is.na(codigo_acta), 1, 99),
         rno = row_number()) %>%
  arrange(apoyo, rno) %>% 
  mutate(nnn = row_number()) %>% 
  ungroup() %>% 
  mutate(codigo_acta = ifelse(!is.na(codigo_acta), codigo_acta,
                              paste0("sin-codigo-", str_pad(nnn, 3, "left", "0")))) %>% 
  arrange(fecha_rescate_imp) %>% 
  select(codigo_acta, fecha_rescate, fecha_rescate_imp, hora_rescate, hora_rescate_imp,
         latitud_rescate, longitud_rescate, nombres_rescatista, apellidos_rescatista,
         identificacion_rescatista, institucion_rescatista, forma_aviso, reino, clase,
         nombre_cientifico, nombre_comun, nro_machos, nro_hembras, nro_indeterminado,
         etapa_de_vida, estado_fisico, razon_social_destino, nombres_destino,
         apellidos_destino, fecha_destino, fecha_destino_imp, destino_final,
         acta_destino_final, provincia, observacion)


export(rescate2, "05_Entregable 3/maate_extra/intermedios/Rescates_9mayo2025_v2.0.xlsx")


