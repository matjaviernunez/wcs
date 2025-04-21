rm(list = ls())

library(tidyverse)
library(openxlsx)
library(janitor)

cites <- readRDS("55_Entregable tabulados/intermedios/03_cites.rds")

# tabla número de registros versus numero de id unico

tabla01 <- cites |> 
  group_by(anio) |> 
  summarise(registro = n(),
            permisos = n_distinct(cod_permiso)) |> 
  adorn_totals()

# pastel por pais y barras cinco países más grandes

img01 <- cites |> 
  mutate(cantidad = as.numeric(cantidad)) |> 
  group_by(cod_pais_imp) |> 
  summarise(permisos = n_distinct(cod_permiso)) |> 
  mutate(cod_pais_imp = ifelse(permisos <= 5, "Otros", cod_pais_imp)) |> 
  group_by(`Código país` = cod_pais_imp) |> 
  summarise(permisos = sum(permisos)) |> 
  ggplot(aes(x="", y=permisos, fill = `Código país`)) + 
  geom_bar(stat='identity')+
  coord_polar("y", start = pi/2)+
  theme(panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background = element_rect(fill="white"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.title.x = element_blank(),
        legend.box = "vertical",
        legend.box.spacing = unit(0.5, "cm"),
        legend.direction = "vertical", 
        legend.position = "right",
        legend.text = element_text(size=7),
        legend.title = element_text(size=8),
        legend.title.align = 0.5,
        panel.border = element_rect(colour = "black", fill = NA))

a = 150*1.10
h = 150*1.20

ggsave(plot = img01,
       file ="img01.png",
       device = "png",
       path = paste0("55_Entregable tabulados/intermedios/03_cites_imagenes"),
       scale = 1, width = a, height = h, units = "mm",
       dpi = 300,
       limitsize = F)

# tabla cantidad de vivos por año y país

tabla02 <- cites |> 
  filter(tipo == "Vivo") |> 
  mutate(cantidad = as.numeric(cantidad)) |> 
  group_by(cod_pais_imp, anio) |> 
  summarise(cantidad = sum(cantidad, na.rm = T)) |> 
  pivot_wider(names_from = anio, values_from = cantidad, values_fill = 0)

tabla021 <- cites |> 
  filter(tipo == "Vivo") |> 
  mutate(cantidad = as.numeric(cantidad)) |> 
  group_by(cod_pais_imp, anio) |> 
  summarise(permisos = n_distinct(cod_permiso)) |> 
  pivot_wider(names_from = anio, values_from = permisos, values_fill = 0)

# cantidad de no vivos por unidades y tipo

tabla03 <- cites |> 
  filter(tipo != "Vivo") |> 
  mutate(cantidad = as.numeric(cantidad)) |> 
  group_by(tipo, unidades) |> 
  summarise(cantidad = sum(cantidad, na.rm = T)) |> 
  pivot_wider(names_from = tipo, values_from = cantidad, values_fill = 0)

export(list("tabla01" = tabla01, "tabla02" = tabla02, 
            "tabla021" = tabla021, "tabla03" = tabla03), 
       "55_Entregable tabulados/intermedios/03_cites_tablas.xlsx")
