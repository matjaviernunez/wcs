rm(list = ls())

library(tidyverse)
library(rio)
library(openxlsx)
library(shadowtext)

#####
# Año 2024

cites24 <- import("02_Bases de Datos 2025/MAATE/BIODOVERSIDAD/CITES/Informe CITES 2024.xlsx",
                  sheet = "Exportación - Reexportación")

aux24 <- cites24 |> 
  mutate(descripcion = tolower(`Descripción del espécimen`),
         cantidad = Cantidad,
         unidades = case_when(Unidad %in% c("KGM", "MTQ", "NAR") ~ Unidad,
                              Unidad == "Muestras" ~ "MUE",
                              grepl("vivo", tolower(`Descripción del espécimen`)) ~ "NAR",
                              T ~ "SIN"),
         cod_pais_imp = case_when(`Pais de destino` == "COL" ~ "CO",
                                  `Pais de destino` == "UK" ~ "GB",
                                  !is.na(`Pais de destino`) ~ `Pais de destino`,
                                  T ~ "--"),
         tipo = case_when(grepl("vivo", descripcion) ~ "Vivo",
                          grepl("completo", descripcion) ~ "Muerto",
                          grepl("cuerpo", descripcion) ~ "Muerto",
                          descripcion %in% c("especimen en etanol",
                                             "especimen en formol",
                                             "espécimen en etanol") ~ "Muerto",
                          unidades %in% c("KGM", "MTQ", "MUE") ~ "Elemento",
                          !is.na(descripcion) ~ "Elemento",
                          T ~ "Sin tipo")) |> 
  select(cod_permiso = `Número del permiso de exportación o del certificado de reexportación`,
         apendice = `Apéndice`,
         origen = Origen,
         grupo = Grupo,
         especie = Especie,
         cod_comercio = `Codigo de comercio`,
         descripcion,
         proposito = `Propósito`,
         anio = `Año`,
         cantidad, 
         unidades,
         cod_pais_imp,
         tipo)

#####
# Años 2022 y 2023

cites22_23 <- read.xlsx("02_Bases de Datos 2025/MAATE/BIODOVERSIDAD/CITES/Matriz permisos CITES 2022 y 2023.xlsx",
                        sheet = 1, detectDates = T, fillMergedCells = T)

aux22_23 <- cites22_23 |> 
  mutate(
    cantidad = `#.de.especimenes` %>% str_extract('[:digit:]+'),
    auxiliar1 = `#.de.especimenes` %>%  str_extract('[:alpha:]+'),
    descripcion = tolower(`Descripción.especimenes`),
    unidades = case_when(grepl("vivo", descripcion) ~ "NAR",
                         grepl("completo", descripcion) ~ "NAR",
                         grepl("cuerpo", descripcion) ~ "NAR",
                         descripcion %in% c("especiemen cientifico", 
                                            "especimen en etanol",
                                            "especimen en formol",
                                            "espécimen en etanol") ~ "NAR",
                         auxiliar1 %in% c("kg", "Kg", "kilo", "kilos") ~ "KGM",
                         grepl("muestra", descripcion) ~ "MUE",
                         grepl("muestra", tolower(`#.de.especimenes`)) ~ "MUE",
                         T ~ "SIN"),
    tipo = case_when(grepl("vivo", descripcion) ~ "Vivo",
                     grepl("completo", descripcion) ~ "Muerto",
                     grepl("cuerpo", descripcion) ~ "Muerto",
                     descripcion %in% c("especiemen cientifico", 
                                        "especimen en etanol",
                                        "especimen en formol",
                                        "espécimen en etanol") ~ "Muerto",
                     auxiliar1 %in% c("kg", "Kg", "kilo", "kilos") ~ "Elemento",
                     !is.na(descripcion) ~ "Elemento",
                     T ~ "Sin tipo"),
    cod_pais_imp = case_when(`Pais.de.importación` == "EEUU" ~ "US",
                             `Pais.de.importación` == "Austria" ~ "AT",
                             `Pais.de.importación` == "Perú" ~ "PE",
                             `Pais.de.importación` == "España" ~ "ES",
                             `Pais.de.importación` == "Holanda" ~ "NL",
                             `Pais.de.importación` == "Hong Kong" ~ "HK",
                             `Pais.de.importación` == "Alemania" ~ "DE",
                             `Pais.de.importación` == "PL Doral FL33178" ~ "US",
                             `Pais.de.importación` == "Colombia" ~ "CO",
                             `Pais.de.importación` == "Hong kong" ~ "HK",
                             `Pais.de.importación` == "Ecuador" ~ "EC",
                             `Pais.de.importación` == "Canada" ~ "CA",
                             `Pais.de.importación` == "Dinamarca" ~ "DK",
                             `Pais.de.importación` == "Chile" ~ "CL",
                             `Pais.de.importación` == "Italia" ~ "IT",
                             `Pais.de.importación` == "Republica checa" ~ "CZ",
                             `Pais.de.importación` == "Reino unido" ~ "GB",
                             `Pais.de.importación` == "Singapur" ~ "SG",
                             `Pais.de.importación` == "El Salvador" ~ "SV",
                             `Pais.de.importación` == "Curazao" ~ "CW",
                             `Pais.de.importación` == "Filpinas" ~ "PH",
                             `Pais.de.importación` == "South Africa" ~ "ZA",
                             `Pais.de.importación` == "Taiwan" ~ "TW",
                             `Pais.de.importación` == "Republica de korea" ~ "KR",
                             `Pais.de.importación` == "Peru" ~ "PE",
                             `Pais.de.importación` == "Filipinas" ~ "PH",
                             `Pais.de.importación` == "Korea" ~ "Korea",
                             `Pais.de.importación` == "portugal" ~ "PT",
                             `Pais.de.importación` == "Australia" ~ "AU",
                             `Pais.de.importación` == "Brasil" ~ "BR",
                             `Pais.de.importación` == "Estados Unidos " ~ "US",
                             T ~ "--"),
    anio = substr(as.character(Fecha), 1, 4),
    apendice = "No declarado 22 23",
    origen = "No declarado 22 23",
    grupo = "En proceso",
    cod_comercio = "No declarado 22 23"
  ) |> 
  select(cod_permiso = `No..Permiso.CITES`,
         apendice,
         origen,
         grupo,
         especie = Nombre.cientifico,
         cod_comercio,
         descripcion,
         proposito = `Proposito`,
         anio,
         cantidad, 
         unidades,
         cod_pais_imp,
         tipo)

cites <- rbind(aux22_23, aux24)

saveRDS(cites, "06_Entregable 4/previo/04_cites.rds")

export(cites, "06_Entregable 4/previo/04_cites.xlsx")

npais <- import("05_Entregable 3/envio/Dominio de Valores/CITES/codigos_cites.xlsx",
                sheet = "pais")

apoyo01 <- cites |> 
  mutate(cantidad = as.numeric(cantidad)) |> 
  group_by(cod_pais_imp) |> 
  summarise(permisos = n_distinct(cod_permiso)) |> 
  ungroup() |> 
  left_join(npais, by = c("cod_pais_imp" = "cod_pais")) |> 
  mutate(cod_pais_imp = case_when(permisos <= 5 ~ "Otros",
                                  is.na(pais) ~ "--",
                                  T ~ pais)) |> 
  group_by(`País` = cod_pais_imp) |> 
  summarise(permisos = sum(permisos)) |> 
  ungroup() |> 
  arrange(desc(permisos))

verde_paleta <- colorRampPalette(c("#f0f0f0", "#74c476","#2a4d38"))

colores <- verde_paleta(dim(apoyo01)[1])

render(input="06_Entregable 4/previo/04_2_cites_rmarkdown.Rmd",
       output_format="pdf_document", 
       output_dir = "06_Entregable 4/previo/",
       output_file = "04_cites_informe.pdf", 
       knit_root_dir = getwd())
