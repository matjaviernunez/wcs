rm(list = ls())

library(tidyverse)
library(import)

cites24 <- import("02_Bases de Datos 2025/MAATE/BIODOVERSIDAD/CITES/Informe CITES 2024.xlsx",
                  sheet = "Exportación - Reexportación")

cites22_23 <- import("55_Entregable tabulados/insumos/03_cites/Matriz permisos CITES 2022 y 2023 ag.xlsx",
                     sheet = 1, col_types = "text")
