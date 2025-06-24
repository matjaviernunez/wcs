rm(list = ls())

library(tidyverse)
library(rio)
library(janitor)
library(sf)
library(rmarkdown)

#####
# BDD judicatura

judicatura_ci <- import("02_Bases de Datos 2025/Judicatura/CJ 0936 Causas Art 247 corte agosto 2024(1).xlsx",
                        sheet = "Causas Ingresadas", range = "B6:H250")

judicatura_cr <- import("02_Bases de Datos 2025/Judicatura/CJ 0936 Causas Art 247 corte agosto 2024(1).xlsx",
                        sheet = "Causas Resueltas", range = "B6:J202") |> 
  mutate(causas_resueltas = 1)

judicatura_cre <- import("02_Bases de Datos 2025/Judicatura/CJ 0936 Causas Art 247 corte agosto 2024(1).xlsx",
                         sheet = "Causas Razón de ejecutoría", range = "B6:I114") |> 
  mutate(causas_ejecutoria = 1)
judicatura <- judicatura_ci |> 
  left_join(judicatura_cr |> 
              select(- `ESTADO CAUSA`, - `DELITO/ACCIÓN`),
            by = c("IDJUICIO", "PROVINCIA", "CANTON", "INSTANCIA")) |> 
  left_join(judicatura_cre |> 
              select(- `ESTADO CAUSA`, - `DELITO/ACCIÓN`),
            by = c("IDJUICIO", "PROVINCIA", "CANTON", "INSTANCIA")) |> 
  mutate(causas_resueltas = ifelse(is.na(causas_resueltas), 0, causas_resueltas),
         causas_ejecutoria = ifelse(is.na(causas_ejecutoria), 0, causas_ejecutoria),
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
                                T ~ "99"),
         rnatura = case_when(DPA_PROVIN %in% c("01", "02", "03", "04", "05", 
                                               "06", "10", "11", "17", "18") ~ "Sierra",
                             DPA_PROVIN %in% c("07", "08", "09", "12", "13", 
                                               "20", "23", "24") ~ "Costa",
                             DPA_PROVIN %in% c("14", "15", "16", "19", "21", 
                                               "22") ~ "Amazonía",
                             T ~ "Niidea"))

rm(judicatura_ci, judicatura_cr, judicatura_cre)

provincias <- read_sf("55_Entregable tabulados/insumos/provincias.gpkg")

render(input="06_Entregable 4/previo/ag/02_2_judicatura_rmarkdown.Rmd",
       output_format="pdf_document", 
       output_dir = "06_Entregable 4/previo/ag/",
       output_file = "02_judicatura_informe.pdf",
       knit_root_dir = getwd())

saveRDS(judicatura,
     file = "06_Entregable 4/previo/ag/02_judicatura_bdd.rds")
