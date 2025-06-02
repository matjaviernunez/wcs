#
rm(list = ls())
#
library(tidyverse)
library(rio)
library(janitor)
library(tidytext)
library(sf)
library(rmarkdown)
#
#
provincias <- read_sf("55_Entregable tabulados/insumos/provincias.gpkg") %>% 
  select(provincia = DPA_PROVIN, n_provincia = DPA_DESPRO)

retenciones <- import("05_Entregable 3/maate_extra/intermedios/recopilacion_Retenciones_9mayo2025_v2.0.xlsx")
respuesta <- import("05_Entregable 3/maate_extra/intermedios/codigos_sin_clasificar_respuesta.xlsx")

apoyo <- retenciones |> 
  group_by(codigo_acta, direccion_infractor) |> 
  summarise() |> 
  mutate(provincia = case_when(substr(codigo_acta, nchar(codigo_acta) - 3, nchar(codigo_acta)) == "OTQU" ~ "17",
                               substr(codigo_acta, 1, 3) == "DPA" ~ substr(codigo_acta, 4, 5),
                               substr(codigo_acta, 1, 2) == "DP" ~ substr(codigo_acta, 3, 4),
                               substr(codigo_acta, 1, 10) == "sin-codigo" ~ "17",
                               substr(codigo_acta, 1, 8) == "DZ5-OTSE" ~ "24",
                               grepl("OTAM", codigo_acta) ~ "18",
                               grepl("OTQ", codigo_acta) ~ "17",
                               T ~ "lol")) %>% 
  left_join(respuesta %>% 
              select(codigo_acta, direccion_infractor, pc = provincia),
            by = c("codigo_acta", "direccion_infractor")) %>% 
  mutate(provincia = ifelse(provincia == "lol", pc, provincia)) %>% 
  select(-pc)

r1 <- retenciones %>% 
  left_join(apoyo, by = c("codigo_acta", "direccion_infractor")) %>% 
  mutate(division = ifelse(provincia != "20", "continental", "galapagos"),
         anio_retencion = year(fecha_retencion),
         anio_retencion = ifelse(is.na(anio_retencion), "no_declarado", as.character(anio_retencion)))

rm(apoyo, respuesta, retenciones)
#
render(input="55_Entregable tabulados/rutinas/07_maate_upma_rmarkdown.Rmd",
       output_format = "pdf_document", 
       output_dir = "55_Entregable tabulados/productos/",
       output_file = "informe_07_maate_upma.pdf",
       knit_root_dir = getwd())
