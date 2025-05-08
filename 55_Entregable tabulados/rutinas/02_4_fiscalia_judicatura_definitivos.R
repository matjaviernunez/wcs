rm(list = ls())

library(tidyverse)
library(rio)
library(scales)

load("55_Entregable tabulados/intermedios/02_fiscalia_judicatura.RData")

##### Número de casos por año y mes Fiscalía ####

img01f <- fiscalia %>% 
  group_by(anios = d_ANIO_REGISTRO, meses = d_MES_REGISTRO) %>% 
  summarise(`Número de casos` = n()) %>%  
  ggplot(aes(meses, anios)) +
  geom_raster(aes(fill = `Número de casos`), 
              hjust = 0.5, 
              vjust = 0.5, 
              interpolate = FALSE) +
  scale_fill_distiller(trans = "reverse") +
  scale_x_continuous(breaks = c(1:12)) +
  labs(x = "Mes",
       y = "Año") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA), legend.position = "bottom")

a = 150*1.10
h = 60*1.20

ggsave(plot = img01f,
       file ="img01_fiscalia_casos_año_mes.png",
       device = "png",
       path = paste0("55_Entregable tabulados/productos"),
       scale = 1, width = a, height = h, units = "mm",
       dpi = 300,
       limitsize = F)

##### Número de casos por año y mes Judicatura ####

img01j <- data.frame(anios = c(sort(rep(c("2019", "2020", "2021", "2022", "2023", "2024"), 12))),
                     meses = c(1:12)) |> 
  mutate(meses = str_pad(meses, 2, "left", "0")) |> 
  left_join(judicatura %>% 
              group_by(anios = substr(as.character(`FECHA INGRESO`), 1, 4),
                       meses = substr(as.character(`FECHA INGRESO`), 6, 7)) %>% 
              summarise(`Número de casos` = n()),
            by = c("anios", "meses")) |> 
  mutate(`Número de casos` = ifelse(is.na(`Número de casos`), 0, `Número de casos`)) |> 
  ggplot(aes(meses, anios)) +
  geom_raster(aes(fill = `Número de casos`), 
              hjust = 0.5, 
              vjust = 0.5, 
              interpolate = FALSE) +
  scale_fill_distiller(trans = "reverse", labels = label_number(accuracy = 1)) +
  labs(x = "Mes",
       y = "Año") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA), 
        legend.position = "bottom")

a = 150*1.10
h = 90*1.20

ggsave(plot = img01j,
       file ="img01_judicatura_casos_año_mes.png",
       device = "png",
       path = paste0("55_Entregable tabulados/productos"),
       scale = 1, width = a, height = h, units = "mm",
       dpi = 300,
       limitsize = F)

