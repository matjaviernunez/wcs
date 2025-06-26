#
rm(list = ls())
#
library(rio)
library(sf)
library(lubridate)
library(forcats)
library(tidytext)
library(janitor)
library(tidyverse)
library(rmarkdown)
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
         anio_retencion = ifelse(is.na(anio_retencion), "no_declarado", as.character(anio_retencion)),
         rnatura = case_when(provincia %in% c("01", "02", "03", "04", "05", 
                                              "06", "10", "11", "17", "18") ~ "Sierra",
                             provincia %in% c("07", "08", "09", "12", "13", 
                                              "20", "23", "24") ~ "Costa",
                             provincia %in% c("14", "15", "16", "19", "21", 
                                              "22") ~ "Amazonía",
                             T ~ "Niidea"),
         nro_total = case_when(is.na(nro_total) & !is.na(cantidad) ~ cantidad,
                               is.na(nro_total) ~ 1,
                               T ~ nro_total),
         nombre_cientifico = ifelse(is.na(nombre_cientifico), "no_declarado", nombre_cientifico),
         cantidad = case_when(elemento_constitutivo == "4 especímenes muertos" ~ 4,
                              is.na(cantidad) & !is.na(nro_indeterminados) ~ nro_indeterminados,
                              is.na(cantidad) ~ 1,
                              T ~ cantidad),
         elemento_constitutivo = ifelse(is.na(elemento_constitutivo), "No declarado", elemento_constitutivo),
         elemento_constitutivo= ifelse(elemento_constitutivo == "Especimen Disecado", "Especimen disecado", elemento_constitutivo),
         elemento_constitutivo= ifelse(elemento_constitutivo == "piel", "Piel", elemento_constitutivo)
         ) 

rm(apoyo, respuesta, retenciones)

#
render(input="06_Entregable 4/previo/06_2_maate_rmarkdown.Rmd",
       output_format = "pdf_document", 
       output_dir = "06_Entregable 4/previo/",
       output_file = "06_maate_informe.pdf",
       knit_root_dir = getwd())

