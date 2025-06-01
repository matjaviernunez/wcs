#
rm(list = ls())
#
library(readxl)
library(rio)
library(tidyverse)
#
hojas <- excel_sheets("55_Entregable tabulados/intermedios/04_upma_FAUNA SILVESTRE CON CARNE DE MONTE.xlsx")

a <- vector("list", 0)

for(i in 1:length(hojas[1:6])){
  
  a <- read_excel("55_Entregable tabulados/intermedios/04_upma_FAUNA SILVESTRE CON CARNE DE MONTE.xlsx",
                       sheet = hojas[i])
}

b <- do.call(rbind, a)

saveRDS(b, "55_Entregable tabulados/intermedios/04_upma_fauna_carne_de_monte.rds")
