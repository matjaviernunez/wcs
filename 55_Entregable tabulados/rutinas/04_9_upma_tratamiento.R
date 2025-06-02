rm(list = ls())

library(tidyverse)
library(rio)
library(sf)
library(janitor)
library(rmarkdown)


provincias <- read_sf("55_Entregable tabulados/insumos/provincias.gpkg")

upma_flora <- import("55_Entregable tabulados/intermedios/04_upma_FORMATO DGO UPMA  FLORA 2022, 2023 Y 2024.xlsx")
# la base de datos contempla dos columnas extra debido a un caso con un nombre y un número de cédula


aux_tipo_upma_flora <- upma_flora %>%  group_by(`TIPO I`) %>% 
  summarise(CANTIDAD = sum(CANTIDAD))

# export(aux_tipo_upma_flora, "55_Entregable tabulados/intermedios/04_aux_tipo_upma_flora.xlsx")

auxiliar_tipo <- import("55_Entregable tabulados/intermedios/04_aux_tipo_upma_flora.xlsx")

upma_carne <- readRDS("55_Entregable tabulados/intermedios/04_upma_fauna_carne_de_monte.rds") %>% 
  filter(tipo_delito == "CARNE DE FAUNA SILVESTRE RETENIDA") %>% 
  mutate(cantidad = as.numeric(cantidad),
         anio = substr(fecha_operativo, 1, 4),
         cod_pro = substr(cod_subcircuito, 1, 2)) 

upma_flora_01 <- upma_flora %>% 
  select(-`...23`, -`...22`) %>% 
  mutate(`SUB CATEGORIA` = case_when(`SUB CATEGORIA` %in% c("MADERA", "ROLLIZA") ~ "MADERABLE",
                                     T ~ `SUB CATEGORIA`)) %>% 
  left_join(auxiliar_tipo %>% 
              select(`TIPO I`, n_tipo_1), by = "TIPO I")

names(upma_flora_01) <- c("zona",
                          "subzona",
                          "canton",
                          "subcircuito",
                          "ndistrito",
                          "ncircuito",
                          "nsub_circuito",
                          "direccion_rescate",
                          "latitud",
                          "longitud",
                          "fecha_rescate",
                          "unidad_rescate",
                          "tipo_operativo",
                          "hora_inicio",
                          "hora_final",
                          "cantidad",
                          "categoria",
                          "sub_categoria",
                          "tipo_1",
                          "tipo_2",
                          "unidad",
                          "n_tipo_1"
)

upma_elementos <- readRDS("55_Entregable tabulados/intermedios/04_upma_fauna_carne_de_monte.rds") %>% 
  filter(tipo_delito %in% c("ELEMENTOS CONSTITUTIVOS RETENIDOS",
                            "ELEMENTOS CONSTITUTIVOS TRASLOCADOS")) %>% 
  mutate(cantidad = as.numeric(cantidad),
         anio = substr(fecha_operativo, 1, 4),
         cod_pro = substr(cod_subcircuito, 1, 2)) 

upma <- readRDS("55_Entregable tabulados/intermedios/04_upma_fauna_carne_de_monte.rds") %>% 
  mutate(cantidad = as.numeric(cantidad),
         anio = substr(fecha_operativo, 1, 4),
         cod_pro = substr(cod_subcircuito, 1, 2)) 

upma_rescate <- import("55_Entregable tabulados/intermedios/04_upma_RESCATE DE FAUNA SILVESTRE 2019 AL 2024.xlsx") %>% 
  mutate(anio = substr(`FECHA DE OPERATIVO`, 1, 4),
         subcategoria = case_when(`SUB CATEGORIA` %in% c("MAMÍFERO", "MAMIFRO") ~ "MAMIFERO",
                                  `SUB CATEGORIA` %in% c("PELICANO") ~ "AVE",
                                  `SUB CATEGORIA` %in% c("MAMIFERO", "AVE", "REPTIL") ~ `SUB CATEGORIA`,
                                  T ~ "OTROS"),
         cod_pro = substr(`CODIGO SUBCIRCUITO`, 1, 2))

render(input="55_Entregable tabulados/rutinas/04_upma_tablas_figuras.Rmd",
       output_format="pdf_document", 
       output_dir = "55_Entregable tabulados/intermedios/",
       output_file = "04_upma_layout.pdf", 
       knit_root_dir = getwd())
