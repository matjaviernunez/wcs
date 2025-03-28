rm(list = ls())

library(rio)
library(tidyverse)
library(janitor)
library(openxlsx)

resumen <- vector("list", 0)

for (i in 2019:2024){
  bdd <- import("02_Bases de Datos 2025/UPMA/FAUNA SILVESTRE CON CARNE DE MONTE_ag.xlsx",
                sheet = paste0(i)) |> 
    names() |> 
    data.frame() |> 
    mutate(anio = i)
  
  names(bdd) <- c("variables", "anio")
  
  resumen[[i]] <- bdd
}

variables_carne <- do.call("rbind", resumen) |>
  mutate(frecuencia = 1) |> 
  pivot_wider(names_from = anio, values_from = frecuencia, 
              names_prefix = "anio_") |> 
  adorn_totals("col")

write.xlsx(variables_carne, "04_entregable 2/previo/metadatos y diccionario_16.xlsx")
