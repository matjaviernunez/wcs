rm(list = ls())

library(tidyverse)
library(rio)

bdd <- import("05_Entregable 3/maate_extra/intermedios/recopilacion_Retenciones_9mayo2025_v2.0.xlsx")

apoyo <- bdd |> 
  group_by(codigo_acta, direccion_infractor) |> 
  summarise() |> 
  mutate(provincia = case_when(substr(codigo_acta, nchar(codigo_acta) - 3, nchar(codigo_acta)) == "OTQU" ~ "17",
                               substr(codigo_acta, 1, 3) == "DPA" ~ substr(codigo_acta, 4, 5),
                               substr(codigo_acta, 1, 2) == "DP" ~ substr(codigo_acta, 3, 4),
                               substr(codigo_acta, 1, 10) == "sin-codigo" ~ "17",
                               substr(codigo_acta, 1, 8) == "DZ5-OTSE" ~ "24",
                               grepl("OTAM", codigo_acta) ~ "18",
                               grepl("OTQ", codigo_acta) ~ "17",
                               T ~ "lol")) |> 
  filter(provincia == "lol")

export(apoyo |> 
         select(-provincia),
       "05_Entregable 3/maate_extra/intermedios/codigos_sin_clasificar.xlsx")
