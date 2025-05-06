rm(list = ls())

library(tidyverse)
library(openxlsx)

bdd <- read.xlsx("05_Entregable 3/previo/Dominio de Valores/apoyo dpa/TABLA CIRCUITOS,SUBCIRCUITOS.xlsx",
                 sheet = "POR PROVINCIA", fillMergedCells = T)

bdd_01 <- bdd |> 
  filter(PROVINCIA != "PROVINCIA",
         PROVINCIA != "TOTAL",
         PROVINCIA != "ZONA",
         PROVINCIA != "* C = Construcción; R = Remodelación; MUN=Municipio, NUP = No requiere infraestructura Policial (Datos sujetos a variación)") |> 
  rename_all(tolower) |> 
  group_by(cod..subcircuito) |> 
  mutate(n= n()) |> 
  ungroup() |> 
  arrange(desc(n)) |> 
  select(nprovincia = provincia, ncanton = `cantón`, nparroquia = parroquia,
         cod_distrito = cod..distrito, ndistrito = nombre.distrito, 
         cod_circuito = cod..circuito, ncircuito = nombre.circuito,
         cod_subcircuito = cod..subcircuito, nsubcircuito = nombre.subcircuito)

load("05_Entregable 3/previo/Dominio de Valores/apoyo dpa/dpa_2022.RData")

parroquia_01 <- parroquia |>
  ungroup() |> 
  left_join(canton |> 
              ungroup(), 
            by = c("provin", "canton")) |> 
  left_join(provincia, 
            by = "provin") |> 
  mutate(cod_parroq = paste0(provin, canton, parroq),
         cod_canton = paste0(provin, canton)) |> 
  rename(cod_provincia = provin) |> 
  select(cod_provincia, nprovincia = nprovin, cod_canton, ncanton, 
         cod_parroquia = cod_parroq, nparroquia = nparroq)


provin <- parroquia_01 |> 
  group_by(cod_provincia, nprovincia) |> 
  summarise()

canton <- parroquia_01 |> 
  group_by(cod_provincia, nprovincia, cod_canton, ncanton) |> 
  summarise()

distrito <- bdd_01 |> 
  group_by(cod_distrito, ndistrito) |> 
  summarise() |> 
  mutate(cod_provincia = substr(cod_distrito, 1, 2)) |> 
  left_join(provin, by = "cod_provincia") |> 
  select(cod_provincia, nprovincia, cod_distrito, ndistrito)

circuito <- bdd_01 |> 
  group_by(cod_distrito, ndistrito, cod_circuito, ncircuito) |> 
  summarise() |> 
  mutate(cod_provincia = substr(cod_distrito, 1, 2)) |> 
  left_join(provin, by = "cod_provincia") |> 
  select(cod_provincia, nprovincia, cod_distrito, ndistrito, cod_circuito, ncircuito)

subcircuito <- bdd_01 |> 
  group_by(cod_distrito, ndistrito, cod_circuito, ncircuito, cod_subcircuito, nsubcircuito) |> 
  summarise() |> 
  mutate(cod_provincia = substr(cod_distrito, 1, 2)) |> 
  left_join(provin, by = "cod_provincia") |> 
  select(cod_provincia, nprovincia, cod_distrito, ndistrito, cod_circuito, ncircuito, cod_subcircuito, nsubcircuito)

wb <- createWorkbook()  

addWorksheet(wb, "provincias")
addWorksheet(wb, "cantones")
addWorksheet(wb, "parroquias")
addWorksheet(wb, "distritos")
addWorksheet(wb, "circuitos")
addWorksheet(wb, "subcircuitos")

writeData(wb, "provincias", provin)
writeData(wb, "cantones", canton)
writeData(wb, "parroquias", parroquia_01)
writeData(wb, "distritos", distrito)
writeData(wb, "circuitos", circuito)
writeData(wb, "subcircuitos", subcircuito)

saveWorkbook(wb, "05_Entregable 3/previo/Dominio de Valores/apoyo dpa/division_geografica.xlsx", overwrite = T)
