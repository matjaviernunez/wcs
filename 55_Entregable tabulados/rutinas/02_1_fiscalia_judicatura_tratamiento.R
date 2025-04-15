rm(list = ls())

library(tidyverse)
library(rio)
library(rmarkdown)

##### 
# BDD fiscalía

fiscalia <- import("02_Bases de Datos 2025/Fiscalía/Informe_estadistico_2025012322001116 FIscalia.xlsx",
                   sheet = "BDD")

#####
# BDD judicatura

judicatura_ci <- import("02_Bases de Datos 2025/Judicatura/CJ 0936 Causas Art 247 corte agosto 2024(1).xlsx",
                      sheet = "Causas Ingresadas", range = "B6:H250")

judicatura_cr <- import("02_Bases de Datos 2025/Judicatura/CJ 0936 Causas Art 247 corte agosto 2024(1).xlsx",
                        sheet = "Causas Resueltas", range = "B6:J202")

judicatura_cre <- import("02_Bases de Datos 2025/Judicatura/CJ 0936 Causas Art 247 corte agosto 2024(1).xlsx",
                        sheet = "Causas Razón de ejecutoría", range = "B6:I114")
judicatura <- judicatura_ci |> 
  left_join(judicatura_cr |> 
              select(- `ESTADO CAUSA`, - `DELITO/ACCIÓN`),
            by = c("IDJUICIO", "PROVINCIA", "CANTON", "INSTANCIA")) |> 
  left_join(judicatura_cre |> 
              select(- `ESTADO CAUSA`, - `DELITO/ACCIÓN`),
            by = c("IDJUICIO", "PROVINCIA", "CANTON", "INSTANCIA")) |> 
  mutate(causas_resueltas = ifelse(IDJUICIO %in% judicatura_cr$IDJUICIO, 1, 0),
         causas_ejecutoria = ifelse(IDJUICIO %in% judicatura_cre$IDJUICIO, 1, 0))

rm(judicatura_ci, judicatura_cr, judicatura_cre)
# prueba1 <- judicatura |> 
#   filter(!is.na(`FECHA PROVIDENCIA`))
# 
# sum(prueba1$`ESTADO CAUSA.x` == prueba1$`ESTADO CAUSA.y`)
# sum(prueba1$`DELITO/ACCIÓN.x` == prueba1$`DELITO/ACCIÓN.y`)
# 
# prueba2 <- judicatura |> 
#   filter(!is.na(`FECHA_PROVIDENCIAACTIVIDAD`))
# 
# sum(prueba2$`ESTADO CAUSA.x` == prueba2$`ESTADO CAUSA.y`)
# sum(prueba2$`DELITO/ACCIÓN.x` == prueba2$`DELITO/ACCIÓN.y`)
# 
# judicatura_ci_01 <- judicatura_ci |> 
#   group_by(IDJUICIO, PROVINCIA, CANTON, INSTANCIA) |> 
#   mutate(n = n())
# 
# judicatura_cr_01 <- judicatura_cr |> 
#   group_by(IDJUICIO) |> 
#   mutate(n = n())
# 
# judicatura_cre_01 <- judicatura_cre |> 
#   group_by(IDJUICIO) |> 
#   mutate(n = n())