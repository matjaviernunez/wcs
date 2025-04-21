rm(list = ls())

library(tidyverse)
library(rio)

load("55_Entregable tabulados/intermedios/02_fiscalia_judicatura.RData")
load("D:/MAG/ENDI2/insumos/03_muestra_usm/dpa_2022.RData")
##### FISCALIA

# Número de casos por año
tabla01f <- fiscalia %>% 
  group_by(anio = d_ANIO_REGISTRO) %>% 
  summarise(casos = n()) %>% 
  adorn_totals()

# Número de casos por provincia y año

tabla02f <- fiscalia %>% 
  left_join(provincia, by = c("DPA_PROVIN" = "provin")) |> 
  group_by(Provincia  = nprovin, anio = d_ANIO_REGISTRO) %>% 
  summarise(casos = n()) %>% 
  pivot_wider(names_from = anio, values_from = casos, values_fill = 0) %>% 
  adorn_totals("col") |> 
  mutate(Provincia = str_to_title(Provincia)) |> 
  arrange(desc(Total)) |> 
  adorn_totals()

# Gráfico de calor de número de casos por provincia

img01f <- provincias %>% 
  left_join(fiscalia %>% 
              group_by(DPA_PROVIN) %>% 
              summarise(casos = n()),
            by = "DPA_PROVIN") %>%
  mutate(casos = ifelse(is.na(casos), 0, casos)) |> 
  ggplot() + 
  geom_sf(aes(fill = casos)) +
  scale_fill_distiller(trans = "reverse")

# Número de casos por año y mes

img02f <- fiscalia %>% 
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
        panel.border = element_rect(colour = "black", fill = NA))


# Número de casos por año y mes Sierra

img03f <- fiscalia %>% 
  filter(rnatura == "Sierra") |> 
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
        panel.border = element_rect(colour = "black", fill = NA))

# Número de casos por año y mes Costa

img04f <- fiscalia %>% 
  filter(rnatura == "Costa") |> 
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
        panel.border = element_rect(colour = "black", fill = NA))

# Número de casos por año y mes Costa

img05f <- fiscalia %>% 
  filter(rnatura == "Amazonía") |> 
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
        panel.border = element_rect(colour = "black", fill = NA))

##### JUDICATURA
# Número de casos por año
tabla01j <- judicatura %>% 
  group_by(anio = substr(as.character(`FECHA INGRESO`), 1, 4)) %>% 
  summarise(casos = n()) %>% 
  adorn_totals()
# Número de casos por provincia y año
tabla02j <- judicatura %>% 
  left_join(provincia, by = c("DPA_PROVIN" = "provin")) |> 
  group_by(Provincia  = nprovin, anio = substr(as.character(`FECHA INGRESO`), 1, 4)) %>% 
  summarise(casos = n()) %>% 
  arrange(anio) |> 
  pivot_wider(names_from = anio, values_from = casos, values_fill = 0) %>% 
  adorn_totals("col") |> 
  mutate(Provincia = str_to_title(Provincia)) |> 
  arrange(desc(Total)) |> 
  adorn_totals()

## Gráfico de calor de número de casos por provincia

img01j <- provincias %>% 
  left_join(judicatura %>% 
              group_by(DPA_PROVIN) %>% 
              summarise(casos = n()),
            by = "DPA_PROVIN") %>% 
  mutate(casos = ifelse(is.na(casos), 0, casos)) |> 
  ggplot() + 
  geom_sf(aes(fill = casos)) +
  scale_fill_distiller(trans = "reverse")

## Número de casos por año y mes

img02j <- data.frame(anios = c(sort(rep(c("2019", "2020", "2021", "2022", "2023", "2024"), 12))),
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
  scale_fill_distiller(trans = "reverse") +
  #scale_x_continuous(breaks = c(0:11)) +
  labs(x = "Mes",
       y = "Año") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))


## Número de casos por año y mes Sierra

img03j <- data.frame(anios = c(sort(rep(c("2019", "2020", "2021", "2022", "2023", "2024"), 12))),
                     meses = c(1:12)) |> 
  mutate(meses = str_pad(meses, 2, "left", "0")) |> 
  left_join(judicatura %>% 
              filter(rnatura == "Sierra") |> 
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
  scale_fill_distiller(trans = "reverse") +
  #scale_x_continuous(breaks = c(0:11)) +
  labs(x = "Mes",
       y = "Año") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))

## Número de casos por año y mes Costa

img04j <- data.frame(anios = c(sort(rep(c("2019", "2020", "2021", "2022", "2023", "2024"), 12))),
                     meses = c(1:12)) |> 
  mutate(meses = str_pad(meses, 2, "left", "0")) |> 
  left_join(judicatura %>% 
              filter(rnatura == "Costa") |> 
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
  scale_fill_distiller(trans = "reverse") +
  #scale_x_continuous(breaks = c(0:11)) +
  labs(x = "Mes",
       y = "Año") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))

## Número de casos por año y mes Costa

img05j <- data.frame(anios = c(sort(rep(c("2019", "2020", "2021", "2022", "2023", "2024"), 12))),
                     meses = c(1:12)) |> 
  mutate(meses = str_pad(meses, 2, "left", "0")) |> 
  left_join(judicatura %>% 
              filter(rnatura == "Amazonía") |> 
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
  scale_fill_distiller(trans = "reverse") +
  #scale_x_continuous(breaks = c(0:11)) +
  labs(x = "Mes",
       y = "Año") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))

## Número de casos por año y mes resuelta

img06j <- data.frame(anios = c(sort(rep(c("2019", "2020", "2021", "2022", "2023", "2024"), 12))),
                     meses = c(1:12)) |> 
  mutate(meses = str_pad(meses, 2, "left", "0")) |> 
  left_join(judicatura %>% 
              filter(causas_resueltas == 1) |> 
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
  scale_fill_distiller(trans = "reverse") +
  #scale_x_continuous(breaks = c(0:11)) +
  labs(x = "Mes",
       y = "Año") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))

## Número de casos por año y mes ejecutoria

img07j <- data.frame(anios = c(sort(rep(c("2019", "2020", "2021", "2022", "2023", "2024"), 12))),
                     meses = c(1:12)) |> 
  mutate(meses = str_pad(meses, 2, "left", "0")) |> 
  left_join(judicatura %>% 
              filter(causas_ejecutoria == 1) |> 
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
  scale_fill_distiller(trans = "reverse") +
  #scale_x_continuous(breaks = c(0:11)) +
  labs(x = "Mes",
       y = "Año") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))

export(list("tabla01f" = tabla01f, "tabla02f" = tabla02f, 
            "tabla01j" = tabla01j, "tabla02j" = tabla02j), 
       "55_Entregable tabulados/intermedios/02_judicatura_fiscalia_tablas.xlsx")

#####
# Guardado mapas
a = 150*1.10
h = 50*1.20

ggsave(plot = img01j,
       file ="img01j.png",
       device = "png",
       path = paste0("55_Entregable tabulados/intermedios/02_judicatur_fiscalia_imagenes"),
       scale = 1, width = a, height = h, units = "mm",
       dpi = 300,
       limitsize = F)

ggsave(plot = img01f,
       file ="img01f.png",
       device = "png",
       path = paste0("55_Entregable tabulados/intermedios/02_judicatur_fiscalia_imagenes"),
       scale = 1, width = a, height = h, units = "mm",
       dpi = 300,
       limitsize = F)


#####
# Guardado raster judicatura

a = 150*1.10
h = 60*1.20

ggsave(plot = img02j,
       file ="img02j.png",
       device = "png",
       path = paste0("55_Entregable tabulados/intermedios/02_judicatur_fiscalia_imagenes"),
       scale = 1, width = a, height = h, units = "mm",
       dpi = 300,
       limitsize = F)

ggsave(plot = img03j,
       file ="img03j.png",
       device = "png",
       path = paste0("55_Entregable tabulados/intermedios/02_judicatur_fiscalia_imagenes"),
       scale = 1, width = a, height = h, units = "mm",
       dpi = 300,
       limitsize = F)

ggsave(plot = img04j,
       file ="img04j.png",
       device = "png",
       path = paste0("55_Entregable tabulados/intermedios/02_judicatur_fiscalia_imagenes"),
       scale = 1, width = a, height = h, units = "mm",
       dpi = 300,
       limitsize = F)

ggsave(plot = img05j,
       file ="img05j.png",
       device = "png",
       path = paste0("55_Entregable tabulados/intermedios/02_judicatur_fiscalia_imagenes"),
       scale = 1, width = a, height = h, units = "mm",
       dpi = 300,
       limitsize = F)

ggsave(plot = img06j,
       file ="img06j.png",
       device = "png",
       path = paste0("55_Entregable tabulados/intermedios/02_judicatur_fiscalia_imagenes"),
       scale = 1, width = a, height = h, units = "mm",
       dpi = 300,
       limitsize = F)

ggsave(plot = img07j,
       file ="img07j.png",
       device = "png",
       path = paste0("55_Entregable tabulados/intermedios/02_judicatur_fiscalia_imagenes"),
       scale = 1, width = a, height = h, units = "mm",
       dpi = 300,
       limitsize = F)



####
# Guardado raster fiscalia

a = 150*1.10
h = 60*1.20

ggsave(plot = img02f,
       file ="img02f.png",
       device = "png",
       path = paste0("55_Entregable tabulados/intermedios/02_judicatur_fiscalia_imagenes"),
       scale = 1, width = a, height = h, units = "mm",
       dpi = 300,
       limitsize = F)

ggsave(plot = img03f,
       file ="img03f.png",
       device = "png",
       path = paste0("55_Entregable tabulados/intermedios/02_judicatur_fiscalia_imagenes"),
       scale = 1, width = a, height = h, units = "mm",
       dpi = 300,
       limitsize = F)

ggsave(plot = img04f,
       file ="img04f.png",
       device = "png",
       path = paste0("55_Entregable tabulados/intermedios/02_judicatur_fiscalia_imagenes"),
       scale = 1, width = a, height = h, units = "mm",
       dpi = 300,
       limitsize = F)

ggsave(plot = img05f,
       file ="img05f.png",
       device = "png",
       path = paste0("55_Entregable tabulados/intermedios/02_judicatur_fiscalia_imagenes"),
       scale = 1, width = a, height = h, units = "mm",
       dpi = 300,
       limitsize = F)


#####
# Catálogo
# img01f: mapa calor número de casos en fiscalía por provincia
# img02f: ráster número de casos en fiscalía por año y mes
# img03f: ráster número de casos en fiscalía por año y mes Sierra
# img04f: ráster número de casos en fiscalía por año y mes Costa
# img05f: ráster número de casos en fiscalía por año y mes Amazonía
# img01j: mapa calor número de casos en judicatura por provincia
# img02j: ráster número de casos en judicatura por año y mes
# img03j: ráster número de casos en judicatura por año y mes Sierra
# img04j: ráster número de casos en judicatura por año y mes Costa
# img05j: ráster número de casos en judicatura por año y mes Amazonía
# tab01f: Número de casos por año fiscalía
# tab02f: Número de casos por provincia y año fiscalía
# tab01j: Número de casos por año judicatura
# tab02j: Número de casos por provincia y año judicatura