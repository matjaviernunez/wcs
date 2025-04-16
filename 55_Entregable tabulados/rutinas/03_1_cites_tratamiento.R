rm(list = ls())

library(tidyverse)
library(rio)
library(openxlsx)

cites24 <- import("02_Bases de Datos 2025/MAATE/BIODOVERSIDAD/CITES/Informe CITES 2024.xlsx",
                  sheet = "Exportación - Reexportación")


cites22_23 <- read.xlsx("02_Bases de Datos 2025/MAATE/BIODOVERSIDAD/CITES/Matriz permisos CITES 2022 y 2023.xlsx",
                     sheet = 1, detectDates = T, fillMergedCells = T)

cites22_23_01 <- cites22_23 |> 
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
    pais_imp = case_when(`Pais.de.importación` == "EEUU",
                         `Pais.de.importación` == "Austria",            
                         `Pais.de.importación` == "Perú",               
                         `Pais.de.importación` == "España",            
                         `Pais.de.importación` == "Holanda",            
                         `Pais.de.importación` == "Hong Kong",          
                         `Pais.de.importación` == "Alemania",           
                         `Pais.de.importación` == "PL Doral FL33178",   
                         `Pais.de.importación` == "Colombia",          
                         `Pais.de.importación` == "Hong kong",          
                         `Pais.de.importación` == "Ecuador",            
                         `Pais.de.importación` == "Canada",             
                         `Pais.de.importación` == "Dinamarca",          
                         `Pais.de.importación` == "Chile",             
                         `Pais.de.importación` == "Italia",             
                         `Pais.de.importación` == "Republica checa",    
                         `Pais.de.importación` == "Reino unido",        
                         `Pais.de.importación` == "Singapur",           
                         `Pais.de.importación` == "El Salvador",       
                         `Pais.de.importación` == "Curazao"           
                         `Pais.de.importación` == "Filpinas"           
                         `Pais.de.importación` == "South Africa"       
                         `Pais.de.importación` == "Taiwan"             
                         `Pais.de.importación` == "Republica de korea"
                         `Pais.de.importación` == "Peru"               
                         `Pais.de.importación` == "Filipinas"          
                         `Pais.de.importación` == "Korea"              
                         `Pais.de.importación` == "portugal"           
                         `Pais.de.importación` == "Australia"         
                         `Pais.de.importación` == "Brasil"             
                         `Pais.de.importación` == "Estados Unidos ")
  )

lol <- cites22_23_01 |> filter(is.na(cantidad))
lol <- cites22_23_01 |> filter(unidades == "SIN")
