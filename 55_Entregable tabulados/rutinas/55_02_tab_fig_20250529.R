rm(list = ls())

library(tidyverse)
library(rio)
library(sf)
library(janitor)
library(forcats)
library(tidytext)

provincias <- read_sf("55_Entregable tabulados/insumos/provincias.gpkg")

upma_flora <- import("55_Entregable tabulados/intermedios/04_upma_FORMATO DGO UPMA  FLORA 2022, 2023 Y 2024.xlsx")
# la base de datos contempla dos columnas extra debido a un caso con un nombre y un número de cédula


aux_tipo_upma_flora <- upma_flora %>%  group_by(`TIPO I`) %>% 
  summarise(CANTIDAD = sum(CANTIDAD))

# export(aux_tipo_upma_flora, "55_Entregable tabulados/intermedios/04_aux_tipo_upma_flora.xlsx")

auxiliar_tipo <- import("55_Entregable tabulados/intermedios/04_aux_tipo_upma_flora.xlsx")

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

# La base no tiene un código por lo que se supone cada registro un evento distinto

# Número de decomisos de madera sin contar Galápagos
tab_01_con_flo_eve <- upma_flora_01 %>% 
  filter(substr(subcircuito, 1, 2) != "20") %>% 
  group_by(anio = substr(fecha_rescate, 1, 4)) %>% 
  summarise(evento = n()) %>% 
  adorn_totals()

fig_01_con_flo_eve <- tab_01_con_flo_eve |> 
  filter(anio != "Total") |> 
  mutate(anio = as.numeric(anio)) |> 
  ggplot(aes(x = anio, y = evento)) +
  geom_line(linewidth = 1) + 
  geom_point(size = 2)

  
# Número de decomisos de madera por provincia 
tab_02_con_flo_eve <- upma_flora_01 %>% 
  mutate(cod_pro = substr(subcircuito, 1, 2),
         rnatura = case_when(cod_pro %in% c("01", "02", "03", "04", "05", 
                                            "06", "10", "11", "17", "18") ~ "Sierra",
                             cod_pro %in% c("07", "08", "09", "12", "13", 
                                            "20", "23", "24") ~ "Costa",
                             cod_pro %in% c("14", "15", "16", "19", "21", 
                                            "22") ~ "Amazonía",
                             T ~ "Niidea")) |> 
  group_by(anio = substr(fecha_rescate, 1, 4), rnatura, cod_pro) %>% 
  summarise(evento = n()) %>% 
  filter(cod_pro != "20") %>% 
  pivot_wider(names_from = anio, values_from = evento) %>% 
  adorn_totals() %>% 
  adorn_totals("col") |> 
  arrange(rnatura, desc(Total))

fig_02_con_flo_eve <- tab_02_con_flo_eve |> 
  mutate(prov_ordenado = reorder_within(cod_pro, Total, rnatura)) |> 
  filter(rnatura != "Total") %>%
  ggplot() + 
  geom_col(aes(prov_ordenado, Total),
           linewidth = 1.5,
           position = position_dodge()) +
  coord_flip() +
  facet_wrap(~ rnatura, scales = "free_y") + 
  theme_light() +
  scale_x_reordered(labels = function(x) gsub("__.+$", "", x))

fig_02_con_flo_eve

# Número de decomisos de madera en Galápagos
tab_01_gal_flo_eve <- upma_flora_01 %>% 
  filter(substr(subcircuito, 1, 2) == "20") %>% 
  group_by(anio = substr(fecha_rescate, 1, 4)) %>% 
  summarise(evento = n()) %>% 
  adorn_totals()

fig_01_gal_flo_eve <- tab_01_gal_flo_eve |> 
  filter(anio != "Total") |> 
  mutate(anio = as.numeric(anio)) |> 
  ggplot(aes(x = anio, y = evento)) +
  geom_line(linewidth = 1) + 
  geom_point(size = 2)

fig_01_gal_flo_eve
# Consideramos "equivalente" madera y carne de monte
# Cantidad de decomisos de madera por año
tab_09_con_flo_can <- upma_flora_01 %>% 
  filter(substr(subcircuito, 1, 2) != "20") %>% 
  group_by(anio = substr(fecha_rescate, 1, 4)) %>% 
  summarise(cantidad = sum(cantidad)) %>% 
  adorn_totals()

fig_09_con_flo_can <- tab_09_con_flo_can |> 
  filter(anio != "Total") |> 
  mutate(anio = as.numeric(anio)) |> 
  ggplot(aes(x = anio, y = cantidad)) +
  geom_line(linewidth = 1) + 
  geom_point(size = 2)

fig_09_con_flo_can

# Cantidad de decomisos de madera por año y sub categoría
tab_10_con_flo_can <- upma_flora_01 %>% 
  filter(substr(subcircuito, 1, 2) != "20") %>% 
  group_by(anio = substr(fecha_rescate, 1, 4),sub_categoria) %>% 
  summarise(cantidad = sum(cantidad)) 

tab_10_con_flo_can %>% 
  pivot_wider(names_from = sub_categoria, values_from = cantidad) %>% 
  adorn_totals() %>% 
  adorn_totals("col")

fig_10_con_flo_can <- tab_10_con_flo_can |> 
  ggplot() +
  geom_col(aes(anio, cantidad),
           linewidth = 1.5,
           position = position_dodge()) +
  coord_flip() +
  facet_wrap(~ sub_categoria, scales = "free_y") + 
  theme_light() +
  scale_x_reordered(labels = function(x) gsub("__.+$", "", x))

fig_10_con_flo_can
# Cantidad de decomisos de madera por año y provincia
tab_11_con_flo_can <- upma_flora_01 %>% 
  filter(substr(subcircuito, 1, 2) != "20") %>% 
  group_by(anio = substr(fecha_rescate, 1, 4),cod_pro = substr(subcircuito, 1, 2)) %>% 
  summarise(cantidad = sum(cantidad))

tab_11_con_flo_can %>% 
  pivot_wider(names_from = anio, values_from = cantidad) %>% 
  adorn_totals() %>% 
  adorn_totals("col")

fig_11_con_flo_can <- provincias |> 
  filter(DPA_PROVIN != "20") |> 
  left_join(tab_11_con_flo_can |> 
              group_by(DPA_PROVIN = cod_pro) |> 
              summarise(cantidad = sum(cantidad)),
            by = "DPA_PROVIN") |> 
  ggplot() + 
  geom_sf(aes(fill = cantidad)) +
  scale_fill_distiller(trans = "reverse")
  
fig_11_con_flo_can

# asumiendo que la cantidad hace referencia unicamente a tipo_1
tab_12_con_flo_can <- upma_flora_01 %>% 
  filter(substr(subcircuito, 1, 2) != "20") %>% 
  group_by(n_tipo_1) %>% 
  summarise(cantidad = sum(cantidad)) %>% 
  arrange(desc(cantidad))

### Galapagos
# Cantidad de decomisos de madera por año
tab_09_gal_flo_can <- upma_flora_01 %>% 
  filter(substr(subcircuito, 1, 2) == "20") %>% 
  group_by(anio = substr(fecha_rescate, 1, 4)) %>% 
  summarise(cantidad = sum(cantidad)) %>% 
  adorn_totals()

fig_09_gal_flo_can <- tab_09_gal_flo_can |> 
  filter(anio != "Total") |> 
  mutate(anio = as.numeric(anio)) |> 
  ggplot(aes(x = anio, y = cantidad)) +
  geom_line(linewidth = 1) + 
  geom_point(size = 2)

fig_09_gal_flo_can

# Cantidad de decomisos de madera por año y sub categoría
tab_10_gal_flo_can <- upma_flora_01 %>% 
  filter(substr(subcircuito, 1, 2) == "20") %>% 
  group_by(anio = substr(fecha_rescate, 1, 4),sub_categoria) %>% 
  summarise(cantidad = sum(cantidad)) 

tab_10_gal_flo_can %>% 
  pivot_wider(names_from = sub_categoria, values_from = cantidad) %>% 
  adorn_totals() %>% 
  adorn_totals("col")

fig_10_gal_flo_can <- tab_10_gal_flo_can |> 
  ggplot() +
  geom_col(aes(anio, cantidad),
           linewidth = 1.5,
           position = position_dodge()) +
  coord_flip() +
  facet_wrap(~ sub_categoria, scales = "free_y") + 
  theme_light() +
  scale_x_reordered(labels = function(x) gsub("__.+$", "", x))

fig_10_gal_flo_can

# asumiendo que la cantidad hace referencia unicamente a tipo_1
tab_12_gal_flo_can <- upma_flora_01 %>% 
  filter(substr(subcircuito, 1, 2) == "20") %>% 
  group_by(n_tipo_1) %>% 
  summarise(cantidad = sum(cantidad)) %>% 
  arrange(desc(cantidad))

# carne de monte

upma_carne <- readRDS("55_Entregable tabulados/intermedios/04_upma_fauna_carne_de_monte.rds") %>% 
  filter(tipo_delito == "CARNE DE FAUNA SILVESTRE RETENIDA") %>% 
  mutate(cantidad = as.numeric(cantidad),
    anio = substr(fecha_operativo, 1, 4),
    cod_pro = substr(cod_subcircuito, 1, 2)) 

# kilogramos de carne de monte retenidad
tab_09_con_fau_can <- upma_carne %>% 
  filter(cod_pro != "20") %>% 
  group_by(anio) %>% 
  summarise(cantidad = sum(cantidad)) 

tab_09_con_fau_can %>% 
  adorn_totals()

fig_09_con_fau_can <- tab_09_con_fau_can |> 
  mutate(anio = as.numeric(anio)) |> 
  ggplot(aes(x = anio, y = cantidad)) +
  geom_line(linewidth = 1) + 
  geom_point(size = 2)

# kilogramos decomisados por taxon no se recomienda usar esta tabla pues no está adecuadamente
# categorizada
tab_10_con_fau_can <- upma_carne %>% 
  filter(cod_pro != "20") %>% 
  group_by(familia_especimen) %>% 
  summarise(cantidad = sum(cantidad))

# kilogramos decomisados por provincia
tab_11_con_fau_can <- upma_carne %>% 
  filter(cod_pro != "20") %>% 
  group_by(anio, cod_pro) %>% 
  summarise(cantidad = sum(cantidad)) 

tab_11_con_fau_can %>% 
  pivot_wider(names_from = anio, values_from = cantidad) %>% 
  adorn_totals() %>% 
  adorn_totals("col")


fig_11_con_fau_can <- provincias |> 
  filter(DPA_PROVIN != "20") |> 
  left_join(tab_11_con_fau_can |> 
              group_by(DPA_PROVIN = cod_pro) |> 
              summarise(cantidad = sum(cantidad)),
            by = "DPA_PROVIN") |> 
  ggplot() + 
  geom_sf(aes(fill = cantidad)) +
  scale_fill_distiller(trans = "reverse")

fig_11_con_fau_can


# especie más decomisada de carne de monte
tab_12_con_fau_can <- upma_carne %>% 
  filter(cod_pro != "20") %>% 
  group_by(tipo_especimen_1) %>% 
  summarise(cantidad = sum(cantidad)) %>% 
  arrange(desc(cantidad))

#### no se hace carne de monte para galápagos
# porque no hay






# elementos constitutivos

upma_elementos <- readRDS("55_Entregable tabulados/intermedios/04_upma_fauna_carne_de_monte.rds") %>% 
  filter(tipo_delito %in% c("ELEMENTOS CONSTITUTIVOS RETENIDOS",
    "ELEMENTOS CONSTITUTIVOS TRASLOCADOS")) %>% 
  mutate(cantidad = as.numeric(cantidad),
    anio = substr(fecha_operativo, 1, 4),
    cod_pro = substr(cod_subcircuito, 1, 2)) 

tab_13_con_fau_eve <- upma_elementos %>% 
  filter(cod_pro != "20") %>% 
  group_by(anio) %>% 
  summarise(eventos = n()) %>% 
  adorn_totals()

tab_14_con_fau_eve <- upma_elementos %>% 
  filter(cod_pro != "20") %>% 
  group_by(anio, cod_pro) %>% 
  summarise(eventos = n()) %>%
  pivot_wider(names_from = anio, values_from = eventos) %>% 
  adorn_totals() %>% 
  adorn_totals("col")

tab_13_gal_fau_eve <- upma_elementos %>% 
  filter(cod_pro == "20") %>% 
  group_by(anio) %>% 
  summarise(eventos = n()) %>% 
  adorn_totals()

tab_14_gal_fau_eve <- upma_elementos %>% 
  filter(cod_pro == "20") %>% 
  group_by(anio, cod_pro) %>% 
  summarise(eventos = n()) %>%
  pivot_wider(names_from = anio, values_from = eventos) %>% 
  adorn_totals() %>% 
  adorn_totals("col")

# No se realiza la 15 pues no está categorizado

#UPMA general

upma <- readRDS("55_Entregable tabulados/intermedios/04_upma_fauna_carne_de_monte.rds") %>% 
  mutate(cantidad = as.numeric(cantidad),
    anio = substr(fecha_operativo, 1, 4),
    cod_pro = substr(cod_subcircuito, 1, 2)) 

tab_01_con_fau_eve <- upma %>% 
  filter(cod_pro != "20") %>% 
  group_by(anio) %>% 
  summarise(evento = n_distinct(numero_parte)) %>% 
  adorn_totals()

tab_02_con_fau_eve <- upma %>% 
  filter(cod_pro != "20") %>% 
  group_by(anio, cod_pro) %>% 
  summarise(evento = n_distinct(numero_parte)) %>%
  pivot_wider(names_from = anio, values_from = evento) %>% 
  adorn_totals() %>% 
  adorn_totals("col")

tab_01_gal_fau_eve <- upma %>% 
  filter(cod_pro == "20") %>% 
  group_by(anio) %>% 
  summarise(evento = n_distinct(numero_parte)) %>% 
  adorn_totals()

tab_02_gal_fau_eve <- upma %>% 
  filter(cod_pro == "20") %>% 
  group_by(anio, cod_pro) %>% 
  summarise(evento = n_distinct(numero_parte)) %>%
  pivot_wider(names_from = anio, values_from = evento) %>% 
  adorn_totals() %>% 
  adorn_totals("col")

upma_rescate <- import("55_Entregable tabulados/intermedios/04_upma_RESCATE DE FAUNA SILVESTRE 2019 AL 2024.xlsx") %>% 
  mutate(anio = substr(`FECHA DE OPERATIVO`, 1, 4),
    subcategoria = case_when(`SUB CATEGORIA` %in% c("MAMÍFERO", "MAMIFRO") ~ "MAMIFERO",
    `SUB CATEGORIA` %in% c("PELICANO") ~ "AVE",
    `SUB CATEGORIA` %in% c("MAMIFERO", "AVE", "REPTIL") ~ `SUB CATEGORIA`,
    T ~ "OTROS"),
    cod_pro = substr(`CODIGO SUBCIRCUITO`, 1, 2))

tab_07_con_fau_eve <- upma_rescate %>% 
  filter(cod_pro != "20") %>% 
  group_by(subcategoria, anio) %>% 
  summarise(evento = n_distinct(NUMERO_PARTE)) %>% 
  pivot_wider(names_from = anio, values_from = evento) %>% 
  adorn_totals() %>% 
  adorn_totals("col")

tab_07_con_fau_can <- upma_rescate %>% 
  filter(cod_pro != "20") %>% 
  group_by(subcategoria, anio) %>% 
  summarise(cantidad = sum(CANTIDAD)) %>% 
  pivot_wider(names_from = anio, values_from = cantidad) %>% 
  adorn_totals() %>% 
  adorn_totals("col")

tab_07_gal_fau_eve <- upma_rescate %>% 
  filter(cod_pro == "20") %>% 
  group_by(subcategoria, anio) %>% 
  summarise(evento = n_distinct(NUMERO_PARTE)) %>% 
  pivot_wider(names_from = anio, values_from = evento) %>% 
  adorn_totals() %>% 
  adorn_totals("col")

tab_07_gal_fau_can <- upma_rescate %>% 
  filter(cod_pro == "20") %>% 
  group_by(subcategoria, anio) %>% 
  summarise(cantidad = sum(CANTIDAD)) %>% 
  pivot_wider(names_from = anio, values_from = cantidad) %>% 
  adorn_totals() %>% 
  adorn_totals("col")


cites <- readRDS("55_Entregable tabulados/intermedios/03_cites.rds")

tab_16_eve <- cites %>% 
  group_by(anio, tipo) %>% 
  summarise(n = n_distinct(cod_permiso)) %>% 
  pivot_wider(names_from = anio, values_from = n) %>% 
  adorn_totals() %>% 
  adorn_totals("col")

tab_16_can <- cites %>% 
  mutate(cantidad = as.numeric(cantidad)) %>% 
  group_by(anio, tipo) %>% 
  summarise(cantidad = sum(cantidad, na.rm = T)) %>% 
  pivot_wider(names_from = anio, values_from = cantidad) %>% 
  adorn_totals() %>% 
  adorn_totals("col")



#########################
# FIGURAS



