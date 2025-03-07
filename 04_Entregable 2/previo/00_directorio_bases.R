library(tidyverse)
library(rio)

rm(list = ls())

archivos <- list.files("02_Bases de Datos 2025", recursive = T, full.names = T)

max(str_count(archivos, "/"))

#Si el valor anterior es igual a 4 no haría falta alterar la siguiente sintaxis

resumen_archivos <- data.frame(archivos) |> 
  separate(archivos, c("raiz", "fuente", "a3", "a4", "a5"), sep = "/") |> 
  mutate(subcarpeta1 = case_when(!is.na(a5) ~ a3,
                             !is.na(a4) & is.na(a5) ~ a3,
                             T ~ "-"),
         subcarpeta2 = case_when(!is.na(a5) ~ a4,
                                 T ~ "-"),
         archivo = case_when(!is.na(a5) ~ a5,
                             !is.na(a4) & is.na(a5) ~ a4,
                             T ~ a3),
         revisor = case_when(row_number() %% 2 == 1 ~ "jn",
                             T ~ "ag"),
         diccionario = "",
         observacion = "") |> 
  select(-a3, -a4, -a5)

export(resumen_archivos, paste0("04_Entregable 2/previo/resumen_bdd_", Sys.Date(), ".xlsx"))
