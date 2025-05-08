rm(list = ls())

library(tidyverse)
library(rio)
library(scales)
library(shadowtext)

cites <- readRDS("55_Entregable tabulados/intermedios/03_cites.rds")

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

img01 <- apoyo01 |> 
  mutate(`País` = factor(`País`, levels = apoyo01$País[c(3, 11:4, 2, 1)],
                         labels = apoyo01$País[c(3, 11:4, 2, 1)])) |> 
  ggplot(aes(x="", y=permisos, fill = `País`)) + 
  geom_bar(stat='identity')+
  coord_polar("y", start = pi/2)+
  #geom_text(aes(label=permisos),
   #         position=position_stack(vjust=0.5),color="white",size=3.5, stroke = 0.2)+
  # geom_text(aes(label=permisos),
  #           position=position_stack(vjust=0.5),color="black",size=3)+
  geom_shadowtext(aes(label=permisos),
                  position=position_stack(vjust=0.5),size=3, stroke = 0.2,
                  col="black", bg.color="white") + 
  
  scale_color_manual(values = colores) +
  scale_fill_manual(values = colores) +
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
        legend.title = element_text(size=8, hjust = 0.5),
        panel.border = element_rect(colour = "black", fill = NA))

img01

a = 150*1.10
h = 100*1.20

ggsave(plot = img01,
       file ="img03_cites_permisos_por_pais.png",
       device = "png",
       path = paste0("55_Entregable tabulados/productos/"),
       scale = 1, width = a, height = h, units = "mm",
       dpi = 300,
       limitsize = F)


azul_paleta <- colorRampPalette(c("#f0f0f0", "#6baed6" ,"#08519c"))

colores <- azul_paleta(dim(apoyo01)[1])

img02 <- apoyo01 |> 
  ggplot(aes(x="", y=permisos, fill = `Código país`)) + 
  geom_bar(stat='identity')+
  coord_polar("y", start = pi/2)+
  scale_color_manual(values = colores) +
  scale_fill_manual(values = colores) +
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
        legend.title = element_text(size=8, hjust = 0.5),
        panel.border = element_rect(colour = "black", fill = NA))

