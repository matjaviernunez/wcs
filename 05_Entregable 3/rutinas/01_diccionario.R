#
rm(list=ls())
#
library(openxlsx)
library(readxl)
library(rio)
library(tidyverse)
#
index <-c("01", "03", "05", "07", "09", "11",
          "17", "18") 

bdds <- import("04_Entregable 2/previo/resumen_bdd_2025-03-25.xlsx") %>% 
  filter(orden %in% index)

rutas <- paste(bdds$raiz, bdds$fuente, bdds$subcarpeta1, bdds$subcarpeta2, bdds$archivo, sep = "/")
rutas <- gsub("-/", "", rutas)

rm(bdds)

archivos <- list.files("05_Entregable 3/previo/Diccionarios/", full.names = T)
archivos <- archivos[substr(archivos, 40, 41) %in% index]

nbdd <- gsub("05_Entregable 3/previo/Diccionarios/MD", "", archivos)

for(i in 1:length(archivos)){
  
  hojas <- excel_sheets(archivos[i])
  dicc <- hojas[substr(hojas, 6, 6) == "d"]
  general <- hojas[substr(hojas, 6, 6) == "g"]
  
  # crear el nuevo libro
  wb <- createWorkbook()
  
  for(j in 1:length(dicc)){
    
    vn <- import(archivos[i], sheet = dicc[j]) %>% 
      filter(`Formato de Datos` == "Categórica") %>% 
      select(nom_act = `Nombre actual`,
             nom_pro = `Nombre propuesto`)
    vat <- vn[,1]
    vpr <- vn[,2]
    
    #nombre de la hoja a leer
    nh <- import(archivos[i], sheet = general[j])[[2, 2]]
    if(i %in% c(7,8)){
      rango <- import(archivos[i], sheet = general[j])[[4, 3]]
    }else{
      rango <- import(archivos[i], sheet = general[j])[[4, 2]]
      
    }
    
    if(i == 7){
      bvc <- import(rutas[i], sheet = 1, range = rango) %>% 
        select(any_of(vat))
    }else{
      bvc <- import(rutas[i], sheet = nh, range = rango) %>% 
        select(any_of(vat))
    }
    
    pp <- vat[vat %in% names(bvc)]
    npp <- vpr[vat %in% names(bvc)]
    
    for(k in 1:length(pp)){
      
      # añadir hoja
      addWorksheet(wb, substr(paste(npp[k], nh, sep = "_"), 1, 30))
      
      hh <- bvc %>% 
        group_by(.data[[pp[k]]]) %>% 
        summarise(`Nro. de apariciones` = n(),
                  `Descripción` = NA)
      
      # guardar datos en la hoja
      writeData(wb, sheet = substr(paste(npp[k], nh, sep = "_"), 1, 30)
                , x = hh)
      
    }
    
  }
  # Guardar el archivo
  saveWorkbook(wb, file = paste0("05_Entregable 3/previo/Dominio de Valores/",
                                 "DdV", nbdd[i]), overwrite = TRUE)
  
  rm(wb)
}




