rm(list = ls())

library(tidyverse)
library(openxlsx)

bdd <- read.xlsx("05_Entregable 3/previo/apoyo dpa/TABLA CIRCUITOS,SUBCIRCUITOS.xlsx",
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

load("05_Entregable 3/previo/apoyo dpa/dpa_2022.RData")

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

wb <- createWorkbook()  

addWorksheet(wb, "subcircuitos")
addWorksheet(wb, "parroquias")

writeData(wb, "subcircuitos", bdd_01)
writeData(wb, "parroquias", parroquia_01)

saveWorkbook(wb, "05_Entregable 3/previo/apoyo dpa/division_geografica.xlsx")
