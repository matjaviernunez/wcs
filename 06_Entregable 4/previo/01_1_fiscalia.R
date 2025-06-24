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
                                T ~ "99"),
         rnatura = case_when(DPA_PROVIN %in% c("01", "02", "03", "04", "05", 
                                               "06", "10", "11", "17", "18") ~ "Sierra",
                             DPA_PROVIN %in% c("07", "08", "09", "12", "13", 
                                               "20", "23", "24") ~ "Costa",
                             DPA_PROVIN %in% c("14", "15", "16", "19", "21", 
                                               "22") ~ "Amazonía",
                             T ~ "Niidea")
  )

provincias <- read_sf("55_Entregable tabulados/insumos/provincias.gpkg")

render(input="06_Entregable 4/previo/01_2_fiscalia_rmarkdown.Rmd",
       output_format="pdf_document", 
       output_dir = "06_Entregable 4/previo/",
       output_file = "01_fiscalia_informe.pdf",
       knit_root_dir = getwd())

saveRDS(fiscalia,
        file = "06_Entregable 4/previo/01_fiscalia_bdd.rds")

export(fiscalia,
       file = "06_Entregable 4/previo/01_fiscalia_bdd.xlsx")
