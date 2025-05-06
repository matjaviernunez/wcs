rm(list = ls())

library(tidyverse)
library(rio)

bdd <- readRDS("55_Entregable tabulados/intermedios/03_cites.rds")

t2 <- bdd |> 
  group_by(tipo, unidades, anio) |> 
  summarise(cantidad = sum(as.numeric(cantidad), na.rm = T),
            casos = n())
