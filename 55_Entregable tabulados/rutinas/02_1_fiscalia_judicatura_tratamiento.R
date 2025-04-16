rm(list = ls())

library(tidyverse)
library(rio)
library(janitor)
library(sf)
library(rmarkdown)

##### 
# BDD fiscalía

fiscalia <- import("02_Bases de Datos 2025/Fiscalía/Informe_estadistico_2025012322001116 FIscalia.xlsx",
                   sheet = "BDD") %>% 
  mutate(DPA_PROVIN = case_when(d_PROVINCIA_INCIDENTE == "GALAPAGOS" ~ "20",
                                d_PROVINCIA_INCIDENTE == "EL ORO" ~ "07",
                                d_PROVINCIA_INCIDENTE == "GUAYAS" ~ "09",
                                d_PROVINCIA_INCIDENTE == "CARCHI" ~ "04",
                                d_PROVINCIA_INCIDENTE == "SUCUMBIOS" ~ "21",
                                d_PROVINCIA_INCIDENTE == "PICHINCHA" ~ "17",
                                d_PROVINCIA_INCIDENTE == "MANABI" ~ "13",
                                d_PROVINCIA_INCIDENTE == "ESMERALDAS" ~ "08",
                                d_PROVINCIA_INCIDENTE == "AZUAY" ~ "01",
                                d_PROVINCIA_INCIDENTE == "SANTO DOMINGO DE LOS TSACHILAS" ~ "23",
                                d_PROVINCIA_INCIDENTE == "ORELLANA" ~ "22",
                                d_PROVINCIA_INCIDENTE == "LOJA" ~ "11",
                                d_PROVINCIA_INCIDENTE == "BOLIVAR" ~ "02",
                                d_PROVINCIA_INCIDENTE == "SANTA ELENA" ~ "24",
                                d_PROVINCIA_INCIDENTE == "IMBABURA" ~ "10",
                                d_PROVINCIA_INCIDENTE == "PASTAZA" ~ "06",
                                d_PROVINCIA_INCIDENTE == "MORONA SANTIAGO" ~ "14",
                                d_PROVINCIA_INCIDENTE == "COTOPAXI" ~ "05",
                                d_PROVINCIA_INCIDENTE == "LOS RIOS" ~ "12",
                                d_PROVINCIA_INCIDENTE == "NAPO" ~ "15",
                                d_PROVINCIA_INCIDENTE == "TUNGURAHUA" ~ "18",
                                d_PROVINCIA_INCIDENTE == "CANAR" ~ "03",
                                d_PROVINCIA_INCIDENTE == "ZAMORA CHINCHIPE" ~ "19",
                                T ~ "99"))
  

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
         causas_ejecutoria = ifelse(IDJUICIO %in% judicatura_cre$IDJUICIO, 1, 0),
         DPA_PROVIN = case_when(PROVINCIA == "GUAYAS" ~ "09",
                                PROVINCIA == "TUNGURAHUA" ~ "18",
                                PROVINCIA == "GALAPAGOS" ~ "20",                     
                                PROVINCIA == "EL ORO" ~ "07",
                                PROVINCIA == "PICHINCHA" ~ "17",
                                PROVINCIA == "SANTA ELENA" ~ "24",                   
                                PROVINCIA == "ORELLANA" ~ "22",
                                PROVINCIA == "SUCUMBIOS" ~ "21",
                                PROVINCIA == "ESMERALDAS" ~ "08",
                                PROVINCIA == "NAPO" ~ "15",
                                PROVINCIA == "IMBABURA" ~ "10",
                                PROVINCIA == "MORONA SANTIAGO" ~ "23",               
                                PROVINCIA == "COTOPAXI" ~ "05",
                                PROVINCIA == "MANABI" ~ "13",
                                PROVINCIA == "CARCHI" ~ "04",                        
                                PROVINCIA == "LOS RIOS" ~ "12",
                                PROVINCIA == "LOJA" ~ "11",
                                PROVINCIA == "AZUAY" ~ "01",                         
                                PROVINCIA == "PASTAZA" ~ "16",
                                PROVINCIA == "SANTO DOMINGO DE LOS TSACHILAS" ~ "23",
                                T ~ "99"))

rm(judicatura_ci, judicatura_cr, judicatura_cre)

provincias <- read_sf("55_Entregable tabulados/insumos/provincias.gpkg")

render(input="55_Entregable tabulados/rutinas/02_2_fiscalia_judicatura_rmarkdown.Rmd",
       output_format="pdf_document", 
       output_dir = "55_Entregable tabulados/productos/",
       output_file = "informe_02_fiscalia_judicatura.pdf",
       knit_root_dir = getwd())

render(input="55_Entregable tabulados/rutinas/02_2_fiscalia_judicatura_rmarkdown.Rmd",
       output_format="html_document", 
       output_dir = "55_Entregable tabulados/productos/",
       output_file = "informe_02_fiscalia_judicatura.html",
       knit_root_dir = getwd())

# render(input="55_Entregable tabulados/rutinas/02_2_fiscalia_judicatura_rmarkdown.Rmd",
#        output_format="pdf_document", 
#        output_dir = "55_Entregable tabulados/productos/",
#        output_file = "informe_02_fiscalia_judicatura.pdf",
#        knit_root_dir = getwd())

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