rm(list = ls())

library(tidyverse)
library(rio)

aux <- import("05_Entregable 3/previo/apoyo citees/tabla1.xlsx", sheet = "pais")

pais <- matrix(aux$info,
  ncol =  2, byrow = T) %>% 
  data.frame()

names(pais) <- c("cod_pais", "pais")

aux <- import("05_Entregable 3/previo/apoyo citees/tabla1.xlsx", sheet = "origen")

origen <- matrix(aux$info,
  ncol =  2, byrow = T) %>% 
  data.frame()

names(origen) <- c("cod_origen", "origen")

aux <- import("05_Entregable 3/previo/apoyo citees/tabla1.xlsx", sheet = "proposito")

proposito <- matrix(aux$info,
  ncol =  2, byrow = T) %>% 
  data.frame()

names(proposito) <- c("cod_proposito", "proposito")

export(list("pais" = pais, "proposito" = proposito, "origen" = origen),
  "05_Entregable 3/previo/apoyo citees/codigos_cites.xlsx")
