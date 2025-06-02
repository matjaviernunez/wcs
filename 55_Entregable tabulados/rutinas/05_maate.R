#
rm(list = ls())
#
library(rio)
library(sf)
library(lubridate)
library(forcats)
library(tidytext)
library(tidyverse)
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

#### T01.- Número de decomisos (eventos) MAATE RETENCIONES VS UPMA por año ####

t01_a <- r1 %>% 
  filter(reino == "Animal") %>% 
  group_by(reino, anio_retencion) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  pivot_wider(names_from = anio_retencion, values_from = n) %>% 
  replace(is.na(.), 0)

g01_a <- r1 %>% 
  filter(reino == "Animal") %>% 
  group_by(division, reino, anio_retencion) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  filter(anio_retencion != "no_declarado") %>% 
  ggplot() + 
  geom_line(aes(anio_retencion, n, color = reino, group = reino),
            linewidth = 1.5) +
  theme_light()

g01_a

t01_p <- r1 %>% 
  filter(reino == "Plantae") %>% 
  group_by(division, reino, anio_retencion) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  pivot_wider(names_from = anio_retencion, values_from = n) %>% 
  replace(is.na(.), 0)

g01_p <- r1 %>% 
  filter(reino == "Plantae") %>% 
  group_by(division, reino, anio_retencion) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  filter(anio_retencion != "no_declarado") %>% 
  ggplot() + 
  geom_line(aes(anio_retencion, n, color = reino, group = reino),
            linewidth = 1.5) +
  theme_light()

g01_p

#### T02.- Provincias total por año MAATE RETENCIONES VS UPMA ####

t02_a <- r1 %>% 
  filter(reino == "Animal") %>% 
  mutate(rnatura = case_when(provincia %in% c("01", "02", "03", "04", "05", 
                                               "06", "10", "11", "17", "18") ~ "Sierra",
                             provincia %in% c("07", "08", "09", "12", "13", 
                                               "20", "23", "24") ~ "Costa",
                             provincia %in% c("14", "15", "16", "19", "21", 
                                               "22") ~ "Amazonía",
                             T ~ "Niidea")) %>% 
  group_by(rnatura, provincia, anio_retencion) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  pivot_wider(names_from = anio_retencion, values_from = n) %>% 
  replace(is.na(.), 0)

g02_a <- r1 %>% 
  filter(reino == "Animal") %>% 
  mutate(rnatura = case_when(provincia %in% c("01", "02", "03", "04", "05", 
                                              "06", "10", "11", "17", "18") ~ "Sierra",
                             provincia %in% c("07", "08", "09", "12", "13", 
                                              "20", "23", "24") ~ "Costa",
                             provincia %in% c("14", "15", "16", "19", "21", 
                                              "22") ~ "Amazonía",
                             T ~ "Niidea")) %>% 
  group_by(rnatura, provincia, anio_retencion) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(prov_ordenado = reorder_within(provincia, n, rnatura)) %>% 
  filter(anio_retencion != "no_declarado") %>%
  ggplot() + 
  geom_col(aes(prov_ordenado, n, fill = anio_retencion),
           linewidth = 1.5,
           position = position_dodge()) +
  coord_flip() +
  facet_wrap(~ rnatura, scales = "free_y") + 
  theme_light() +
  scale_x_reordered(labels = function(x) gsub("__.+$", "", x))

g02_a

t02_p <- r1 %>% 
  filter(reino == "Plantae") %>% 
  mutate(rnatura = case_when(provincia %in% c("01", "02", "03", "04", "05", 
                                              "06", "10", "11", "17", "18") ~ "Sierra",
                             provincia %in% c("07", "08", "09", "12", "13", 
                                              "20", "23", "24") ~ "Costa",
                             provincia %in% c("14", "15", "16", "19", "21", 
                                              "22") ~ "Amazonía",
                             T ~ "Niidea")) %>% 
  group_by(provincia, anio_retencion) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  pivot_wider(names_from = anio_retencion, values_from = n) %>% 
  replace(is.na(.), 0)

g02_p <- r1 %>% 
  filter(reino == "Plantae") %>% 
  mutate(rnatura = case_when(provincia %in% c("01", "02", "03", "04", "05", 
                                              "06", "10", "11", "17", "18") ~ "Sierra",
                             provincia %in% c("07", "08", "09", "12", "13", 
                                              "20", "23", "24") ~ "Costa",
                             provincia %in% c("14", "15", "16", "19", "21", 
                                              "22") ~ "Amazonía",
                             T ~ "Niidea")) %>% 
  group_by(rnatura, provincia, anio_retencion) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(prov_ordenado = reorder_within(provincia, n, rnatura)) %>% 
  filter(anio_retencion != "no_declarado") %>%
  ggplot() + 
  geom_col(aes(prov_ordenado, n, fill = anio_retencion),
           linewidth = 1.5,
           position = position_dodge()) +
  coord_flip() +
  facet_wrap(~ rnatura, scales = "free_y") + 
  theme_light() +
  scale_x_reordered(labels = function(x) gsub("__.+$", "", x))

g02_p

#### T04.- Mapa nacional con decomisos ¿gradiente de calor? ####

t04_a <- r1 %>% 
  filter(reino == "Animal") %>% 
  group_by(provincia) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  arrange(desc(n))

g04_a <- provincias %>%
  left_join(t04_a, by = "provincia") %>% 
  ggplot()+
  geom_sf(aes(fill = n)) +
  scale_fill_distiller(trans = "reverse")

g04_a

t04_p <- r1 %>% 
  filter(reino == "Plantae") %>% 
  group_by(provincia) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  arrange(desc(n))

g04_p <- provincias %>% 
  left_join(t04_p, by = "provincia") %>% 
  ggplot()+
  geom_sf(aes(fill = n)) +
  scale_fill_distiller(trans = "reverse")

g04_p

#### T07.- Número de especímenes vivos decomisados por grupo taxonómico misma tendencia? ####

t07 <- r1 %>% 
  filter(elemento_retenido == "Especimen Vivo") %>% 
  mutate(nro_total = case_when(is.na(nro_total) & !is.na(cantidad) ~ cantidad,
                              T ~ 1)) %>% 
  group_by(clase) %>% 
  summarise(n = sum(nro_total)) %>% 
  ungroup() %>% 
  arrange(desc(n))

g07 <- t07 %>% 
  mutate(clase = factor(clase,
                            .$clase[order(.$n, decreasing = T)],
                            .$clase[order(.$n, decreasing = T)])) %>% 
  ggplot() + 
  geom_col(aes(clase, n, fill = clase),
           linewidth = 1.5,
           position = position_dodge()) +
  theme_light()

g07  

#### T08.- sp mayormente decomisadas para aves, reptiles, mamíferos…. ####  

t08 <- r1 %>% 
  filter(elemento_retenido == "Especimen Vivo") %>% 
  mutate(nro_total = case_when(is.na(nro_total) & !is.na(cantidad) ~ cantidad,
                               is.na(nro_total) ~ 1,
                               T ~ nro_total),
         nombre_cientifico = ifelse(is.na(nombre_cientifico), "no_declarado", nombre_cientifico)) %>% 
  group_by(clase, nombre_cientifico) %>% 
  summarise(n = sum(nro_total)) %>% 
  ungroup() %>% 
  arrange(clase, desc(n)) %>% 
  group_by(clase) %>% 
  mutate(nr = row_number()) %>% 
  filter(nr <= 5) %>% 
  select(-nr)

g08 <- t08 %>% 
  mutate(nombre_cientifico = factor(nombre_cientifico,
                        .$nombre_cientifico[order(.$n, decreasing = F)],
                        .$nombre_cientifico[order(.$n, decreasing = F)])) %>% 
  ggplot() + 
  geom_col(aes(nombre_cientifico, n, fill = clase),
           linewidth = 1.5,
           position = position_dodge()) +
  facet_wrap(~ clase, scales = "free_y") +
  theme_light() +
  coord_flip() +  
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 7))

g08

#### T13.- Cantidad de elementos constitutivos por tipo (huevos, huesos, etc ####  

t13 <- r1 %>% 
  filter(elemento_retenido == "Elementos constitutivos") %>% 
  mutate(cantidad = case_when(elemento_constitutivo == "4 especímenes muertos" ~ 4,
                              is.na(cantidad) & !is.na(nro_indeterminados) ~ nro_indeterminados,
                              is.na(cantidad) ~ 1,
                              T ~ cantidad),
         elemento_constitutivo = ifelse(is.na(elemento_constitutivo), "No declarado", elemento_constitutivo),
         elemento_constitutivo= ifelse(elemento_constitutivo == "Especimen Disecado", "Especimen disecado", elemento_constitutivo),
         elemento_constitutivo= ifelse(elemento_constitutivo == "piel", "Piel", elemento_constitutivo)) %>% 
  group_by(elemento_constitutivo) %>% 
  summarise(n = sum(cantidad)) %>% 
  ungroup() %>% 
  arrange(desc(n))

g13 <- t13 %>% 
  mutate(elemento_constitutivo = factor(elemento_constitutivo,
                                    .$elemento_constitutivo[order(.$n, decreasing = F)],
                                    .$elemento_constitutivo[order(.$n, decreasing = F)])) %>% 
  ggplot() + 
  geom_col(aes(elemento_constitutivo, n, fill = elemento_constitutivo),
           linewidth = 1.5,
           position = position_dodge()) +
  coord_flip() +
  theme_light() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 7),
        legend.position = "none")

g13

#### T14.- Cantidad de elementos constitutivos por tipo (huevos, huesos, etc) por provincia apilado ####

t14 <- r1 %>% 
  filter(elemento_retenido == "Elementos constitutivos") %>% 
  mutate(cantidad = case_when(elemento_constitutivo == "4 especímenes muertos" ~ 4,
                              is.na(cantidad) & !is.na(nro_indeterminados) ~ nro_indeterminados,
                              is.na(cantidad) ~ 1,
                              T ~ cantidad),
         elemento_constitutivo = ifelse(is.na(elemento_constitutivo), "No declarado", elemento_constitutivo),
         elemento_constitutivo= ifelse(elemento_constitutivo == "Especimen Disecado", "Especimen disecado", elemento_constitutivo),
         elemento_constitutivo= ifelse(elemento_constitutivo == "piel", "Piel", elemento_constitutivo)) %>% 
  group_by(provincia, elemento_constitutivo) %>% 
  summarise(n = sum(cantidad)) %>% 
  ungroup() %>% 
  arrange(provincia, desc(n))

g14 <- t14 %>% 
  mutate(elemento_constitutivo = factor(elemento_constitutivo,
                                        t13$elemento_constitutivo[order(t13$n, decreasing = F)],
                                        t13$elemento_constitutivo[order(t13$n, decreasing = F)])) %>% 
  ggplot() + 
  geom_col(aes(elemento_constitutivo, n, fill = provincia, group = provincia),
           linewidth = 1.5) +
  coord_flip() +
  theme_light() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 7))

g14

#### T15.- Cantidad de elementos constitutivos por tipo (huevos, huesos, etc) por taxones ####

t15 <- r1 %>% 
  filter(elemento_retenido == "Elementos constitutivos") %>% 
  mutate(cantidad = case_when(elemento_constitutivo == "4 especímenes muertos" ~ 4,
                              is.na(cantidad) & !is.na(nro_indeterminados) ~ nro_indeterminados,
                              is.na(cantidad) ~ 1,
                              T ~ cantidad),
         elemento_constitutivo = ifelse(is.na(elemento_constitutivo), "No declarado", elemento_constitutivo),
         elemento_constitutivo= ifelse(elemento_constitutivo == "Especimen Disecado", "Especimen disecado", elemento_constitutivo),
         elemento_constitutivo= ifelse(elemento_constitutivo == "piel", "Piel", elemento_constitutivo)) %>% 
  group_by(clase, elemento_constitutivo) %>% 
  summarise(n = sum(cantidad)) %>% 
  ungroup() %>% 
  arrange(clase, desc(n))

g15 <- t15 %>%
  mutate(ele_cons_ordenado = reorder_within(elemento_constitutivo, n, clase)) %>% 
  ggplot() + 
  geom_col(aes(ele_cons_ordenado, n, fill = clase),
           linewidth = 1.5,
           position = position_dodge()) +
  facet_wrap(~ clase, scales = "free_y") +
  coord_flip() +
  theme_light() +
  scale_x_reordered(labels = function(x) gsub("__.+$", "", x))

g15

#### T23.- causal coip por taxon ####

t23_a <- r1 %>%
  filter(reino == "Animal") %>% 
  group_by(clase, causal_coid_coda) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  pivot_wider(names_from = causal_coid_coda, values_from = n) %>% 
  replace(is.na(.), 0) %>% 
  cbind(total = rowSums(.[,3:9]))

g23_a <- r1 %>%
  filter(reino == "Animal") %>% 
  group_by(clase, causal_coid_coda) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(clase = factor(clase,
                        t23_a$clase[order(t23_a$total, decreasing = F)],
                        t23_a$clase[order(t23_a$total, decreasing = F)])) %>% 
  ggplot() + 
  geom_col(aes(clase, n, fill = causal_coid_coda, group = causal_coid_coda),
           linewidth = 1.5) +
  coord_flip() +
  theme_light()

g23_a

t23_p <- r1 %>%
  filter(reino == "Plantae") %>% 
  group_by(clase, causal_coid_coda) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  pivot_wider(names_from = causal_coid_coda, values_from = n) %>% 
  replace(is.na(.), 0) %>% 
  cbind(total = rowSums(.[,3:7]))

g23_p <- r1 %>%
  filter(reino == "Plantae") %>% 
  group_by(clase, causal_coid_coda) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(clase = factor(clase,
                        t23_p$clase[order(t23_p$total, decreasing = F)],
                        t23_p$clase[order(t23_p$total, decreasing = F)])) %>% 
  ggplot() + 
  geom_col(aes(clase, n, fill = causal_coid_coda, group = causal_coid_coda),
           linewidth = 1.5) +
  coord_flip() +
  theme_light()

g23_p

#### T24.- estado fisico por taxon ####

t24_a <- r1 %>% 
  filter(reino == "Animal") %>% 
  group_by(clase, estado_fisico) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  pivot_wider(names_from = estado_fisico, values_from = n) %>% 
  replace(is.na(.), 0) %>% 
  cbind(total = rowSums(.[,3:5]))

g24_a <- r1 %>%
  filter(reino == "Animal") %>% 
  group_by(clase, estado_fisico) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(clase = factor(clase,
                        t24_a$clase[order(t24_a$total, decreasing = F)],
                        t24_a$clase[order(t24_a$total, decreasing = F)])) %>% 
  ggplot() + 
  geom_col(aes(clase, n, fill = estado_fisico, group = estado_fisico),
           linewidth = 1.5) +
  coord_flip() +
  theme_light()

g24_a

t24_p <- r1 %>% 
  filter(reino == "Plantae") %>% 
  group_by(clase, estado_fisico) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  pivot_wider(names_from = estado_fisico, values_from = n) %>% 
  replace(is.na(.), 0) %>% 
  cbind(total = rowSums(.[,3:5]))

g24_p <- r1 %>%
  filter(reino == "Plantae") %>% 
  group_by(clase, estado_fisico) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(clase = factor(clase,
                        t24_p$clase[order(t24_p$total, decreasing = F)],
                        t24_p$clase[order(t24_p$total, decreasing = F)])) %>% 
  ggplot() + 
  geom_col(aes(clase, n, fill = estado_fisico, group = estado_fisico),
           linewidth = 1.5) +
  coord_flip() +
  theme_light()

g24_p

#### T25.- destino final por taxon ####

t25_a <- r1 %>%
  filter(reino == "Animal") %>% 
  group_by(clase, destino_final) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  pivot_wider(names_from = destino_final, values_from = n) %>% 
  replace(is.na(.), 0)%>% 
  cbind(total = rowSums(.[,3:5]))
  
g25_a <- r1 %>%
  filter(reino == "Animal") %>% 
  group_by(clase, destino_final) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(clase = factor(clase,
                        t25_a$clase[order(t25_a$total, decreasing = F)],
                        t25_a$clase[order(t25_a$total, decreasing = F)])) %>% 
  ggplot() + 
  geom_col(aes(clase, n, fill = destino_final, group = destino_final),
           linewidth = 1.5) +
  coord_flip() +
  theme_light()

g25_a

t25_p <- r1 %>%
  filter(reino == "Plantae") %>% 
  group_by(clase, destino_final) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  pivot_wider(names_from = destino_final, values_from = n) %>% 
  replace(is.na(.), 0) %>% 
  cbind(total = rowSums(.[,3:4]))

g25_p <- r1 %>%
  filter(reino == "Plantae") %>% 
  group_by(clase, destino_final) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(clase = factor(clase,
                        t25_p$clase[order(t25_p$total, decreasing = F)],
                        t25_p$clase[order(t25_p$total, decreasing = F)])) %>% 
  ggplot() + 
  geom_col(aes(clase, n, fill = destino_final, group = destino_final),
           linewidth = 1.5) +
  coord_flip() +
  theme_light()

g25_p
