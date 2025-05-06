rm(list = ls())

library(tidyverse)
library(rio)
library(openxlsx)

cites24 <- import("02_Bases de Datos 2025/MAATE/BIODOVERSIDAD/CITES/Informe CITES 2024.xlsx",
                  sheet = "Exportación - Reexportación")

cites22_23 <- read.xlsx("02_Bases de Datos 2025/MAATE/BIODOVERSIDAD/CITES/Matriz permisos CITES 2022 y 2023.xlsx",
                     sheet = 1, detectDates = T, fillMergedCells = T)

np <- unique(cites22_23$Pais.de.importación)[!is.na(unique(cites22_23$Pais.de.importación))]
np <- sort(str_trim(np, "both"))

apoyo_paises <- data.frame(paises = np,
                           codigo = c("DE", "AU", "AT", "BR", "CA", "CL","CO",
                                      "CW", "DK", "EC", "US", "SV", "ES", "US",
                                      "PH", "PH", "NL", "HK", "HK", "IT", "KP",
                                      "PE", "PE", "US", "PT", "GB", "CZ", "KP", # Doral a US
                                      "SG", "ZA", "TW"))

nc <- unique(cites22_23$Nombre.cientifico)[!is.na(unique(cites22_23$Nombre.cientifico))]
nc <- sort(str_trim(nc, "both"))
nc <- gsub("[[]", "", nc)

apoyo_taxon <- data.frame(especie = nc,
                           grupo = rep("", 1166))

export(apoyo_taxon, "apoyo_taxon.xlsx")
