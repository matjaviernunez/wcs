rm(list = ls())

library(tidyverse)
library(rio)
library(openxlsx)

cites24 <- import("02_Bases de Datos 2025/MAATE/BIODOVERSIDAD/CITES/Informe CITES 2024.xlsx",
                  sheet = "Exportación - Reexportación")


cites22_23 <- read.xlsx("02_Bases de Datos 2025/MAATE/BIODOVERSIDAD/CITES/Matriz permisos CITES 2022 y 2023.xlsx",
                     sheet = 1, detectDates = T, fillMergedCells = T)
