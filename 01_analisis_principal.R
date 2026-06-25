# Patrones espaciales de accidentes de tráfico en España (2023)
# Script 01: Análisis principal
# Autor: Lisa Estela Barber Chávez
# Asignatura: Datos Espaciales y Espaciotemporales (DEE)

library(tidyverse)
library(sf)
library(mapSpain)
library(readxl)
library(spdep)
library(spatstat)
library(RColorBrewer)


# 1. CARGA DE DATOS

# Microdatos DGT 2023 (descargar desde: https://www.dgt.es)
acc <- read_xlsx("TABLA_ACCIDENTES_23.xlsx", sheet = "ACCIDENTES_23")

# Variable mortal / no mortal
acc <- acc %>%
  mutate(es_mortal = if_else(TOTAL_MU30DF > 0, "Mortal", "No mortal"))

# 2. AGREGACIÓN POR PROVINCIA

por_prov <- acc %>%
  group_by(COD_PROVINCIA) %>%
  summarise(
    total_acc    = n(),
    acc_mortales = sum(es_mortal == "Mortal"),
    fallecidos   = sum(TOTAL_MU30DF, na.rm = TRUE),
    tasa_mort    = acc_mortales / total_acc * 100
  ) %>%
  filter(!is.na(COD_PROVINCIA))

# Geometrías provinciales
spain <- esp_get_prov() %>%
  mutate(COD_PROVINCIA = as.numeric(cpro),
         superficie_km2 = as.numeric(st_area(geometry)) / 1e6)

# Densidad de población (INE 2021, descargar desde: https://www.ine.es/jaxiT3/Tabla.htm?t=2852)
pob_raw <- read_xlsx("2852.xlsx", skip = 7)
densidad <- pob_raw %>%
  rename(provincia = 1, poblacion_2021 = 2) %>%
  select(provincia, poblacion_2021) %>%
  filter(!is.na(provincia), provincia != "Total", provincia != " ") %>%
  mutate(
    COD_PROVINCIA  = as.numeric(str_extract(provincia, "^\\d+")),
    poblacion_2021 = as.numeric(poblacion_2021)
  ) %>%
  filter(!is.na(COD_PROVINCIA))

# Unión de capas
mapa <- spain %>%
  left_join(por_prov, by = "COD_PROVINCIA") %>%
  left_join(densidad %>% select(COD_PROVINCIA, poblacion_2021),
            by = "COD_PROVINCIA") %>%
  mutate(densidad_pob = poblacion_2021 / superficie_km2)


# 3. MAPAS COROPLÉTICOS

# Tasa de mortalidad por provincia
ggplot(mapa) +
  geom_sf(aes(fill = tasa_mort), color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "C", direction = -1,
                       name = "Tasa mortalidad (%)") +
  theme_minimal()


# Fallecidos absolutos por provincia
ggplot(mapa) +
  geom_sf(aes(fill = fallecidos), color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "A", direction = -1, name = "Fallecidos") +
  theme_minimal()

centroides_mapa <- muni_sf %>%
  st_centroid()

# Mapa de puntos proporcionales
ggplot() +
  geom_sf(data = spain, fill = "grey95", color = "white", size = 0.2) +
  geom_sf(data = centroides_mapa, 
          aes(size = n_mortales), 
          color = "red", alpha = 0.5) +
  scale_size_continuous(
    name = "Accidentes\nmortales",
    range = c(0.5, 8),
    breaks = c(10, 20, 30, 40)
  ) +
  theme_minimal()

# 4. FACTORES TERRITORIALES

# Correlación densidad de población - tasa de mortalidad
datos_cor <- mapa %>%
  st_drop_geometry() %>%
  select(tasa_mort, densidad_pob) %>%
  filter(!is.na(tasa_mort), !is.na(densidad_pob))

cor(datos_cor$tasa_mort, datos_cor$densidad_pob)

ggplot(datos_cor, aes(x = densidad_pob, y = tasa_mort)) +
  geom_point(color = "steelblue", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  scale_x_log10() +
  labs(x = "Densidad (hab/km², escala log)", y = "Tasa mortalidad (%)") +
  theme_minimal()


# Tasa de mortalidad por tipo de vía
tipo_via_labels <- c(
  "1" = "Autopista peaje", "2" = "Autopista libre",
  "3" = "Autovía",         "4" = "Vía p/ automóviles",
  "5" = "Conv. doble calzada", "6" = "Conv. calzada única",
  "7" = "Vía de servicio", "8" = "Ramal enlace",
  "9" = "Calle",           "10" = "Camino vecinal",
  "11" = "Recinto",        "12" = "Vía ciclista",
  "13" = "Senda ciclable", "14" = "Otro"
)

acc %>%
  mutate(TIPO_VIA = as.character(TIPO_VIA)) %>%
  group_by(TIPO_VIA) %>%
  summarise(
    accidentes = n(),
    fallecidos = sum(TOTAL_MU30DF, na.rm = TRUE),
    tasa       = fallecidos / accidentes * 100
  ) %>%
  filter(!is.na(TIPO_VIA)) %>%
  mutate(etiqueta = recode(TIPO_VIA, !!!tipo_via_labels)) %>%
  arrange(desc(tasa)) %>%
  ggplot(aes(x = reorder(etiqueta, tasa), y = tasa, fill = tasa)) +
  geom_col() +
  scale_fill_viridis_c(option = "C", direction = -1) +
  coord_flip() +
  theme_minimal()

# Urbano vs interurbano
acc %>%
  mutate(zona_label = if_else(ZONA_AGRUPADA == 1, "Interurbana", "Urbana")) %>%
  group_by(zona_label) %>%
  summarise(tasa = sum(TOTAL_MU30DF, na.rm = TRUE) / n() * 100) %>%
  ggplot(aes(x = zona_label, y = tasa, fill = zona_label)) +
  geom_col(width = 0.5) +
  scale_fill_manual(values = c("Interurbana" = "#d73027", "Urbana" = "#4575b4")) +
  theme_minimal() +
  theme(legend.position = "none")

# 5. AUTOCORRELACIÓN ESPACIAL

# Índice de Moran global
mapa_moran <- mapa %>% filter(!is.na(tasa_mort))
vecinos <- poly2nb(mapa_moran, queen = TRUE)
pesos   <- nb2listw(vecinos, style = "W", zero.policy = TRUE)

moran.test(mapa_moran$tasa_mort, pesos, zero.policy = TRUE)

moran.plot(mapa_moran$tasa_mort, pesos, zero.policy = TRUE,
           xlab = "Tasa de mortalidad (estandarizada)",
           ylab = "Retardo espacial (lag)")

# Análisis LISA
local_m <- spdep::localmoran(mapa_moran$tasa_mort, pesos, zero.policy = TRUE)
z_tasa  <- scale(mapa_moran$tasa_mort)[, 1]
lag_tasa <- spdep::lag.listw(pesos, mapa_moran$tasa_mort, zero.policy = TRUE)
z_lag   <- scale(lag_tasa)[, 1]
p_val   <- local_m[, 5]

mapa_moran$lisa_cluster <- "No significativo"
mapa_moran$lisa_cluster[z_tasa > 0 & z_lag > 0 & p_val < 0.05] <- "High-High"
mapa_moran$lisa_cluster[z_tasa < 0 & z_lag < 0 & p_val < 0.05] <- "Low-Low"
mapa_moran$lisa_cluster[z_tasa > 0 & z_lag < 0 & p_val < 0.05] <- "High-Low"
mapa_moran$lisa_cluster[z_tasa < 0 & z_lag > 0 & p_val < 0.05] <- "Low-High"

colores_lisa <- c(
  "High-High"        = "#d73027",
  "Low-Low"          = "#4575b4",
  "High-Low"         = "#f46d43",
  "Low-High"         = "#74add1",
  "No significativo" = "#f0f0f0"
)

ggplot(mapa_moran) +
  geom_sf(aes(fill = lisa_cluster), color = "white", size = 0.2) +
  scale_fill_manual(values = colores_lisa, name = "Cluster LISA") +
  theme_minimal()

# 6. ANÁLISIS DE PATRONES DE PUNTOS
municipios <- esp_get_munic() %>%
  mutate(COD_MUNICIPIO = as.character(LAU_CODE))

mortales_muni <- acc %>%
  filter(TOTAL_MU30DF > 0, !is.na(COD_MUNICIPIO), COD_MUNICIPIO != "00000") %>%
  group_by(COD_MUNICIPIO) %>%
  summarise(n_mortales = n())

muni_sf <- municipios %>%
  left_join(mortales_muni, by = "COD_MUNICIPIO") %>%
  filter(!is.na(n_mortales))

centroides_t <- st_transform(st_centroid(muni_sf), 25830)
coords_t     <- st_coordinates(centroides_t)
spain_t      <- st_transform(st_union(spain), 25830)
window       <- as.owin(c(st_bbox(spain_t)[1], st_bbox(spain_t)[3],
                          st_bbox(spain_t)[2], st_bbox(spain_t)[4]))

ppp_mortales <- ppp(x = coords_t[, 1], y = coords_t[, 2],
                    window = window, marks = muni_sf$n_mortales)

# Test chi-cuadrado CSR
q_test <- quadrat.test(unmark(ppp_mortales), nx = 5, ny = 5)
print(q_test)

# Entornos mortales
env_mort <- envelope(unmark(ppp_mortales), fun = Gest,
                     nsim = 99, nrank = 2, verbose = FALSE)

# No mortales
no_mortales_muni <- acc %>%
  filter(TOTAL_MU30DF == 0, !is.na(COD_MUNICIPIO), COD_MUNICIPIO != "00000") %>%
  group_by(COD_MUNICIPIO) %>%
  summarise(n_no_mortales = n())

muni_sf_no_mort <- municipios %>%
  left_join(no_mortales_muni, by = "COD_MUNICIPIO") %>%
  filter(!is.na(n_no_mortales))

centroides_no_mort <- st_transform(st_centroid(muni_sf_no_mort), 25830)
coords_no_mort <- st_coordinates(centroides_no_mort)

ppp_no_mortales <- ppp(x = coords_no_mort[, 1], y = coords_no_mort[, 2],
                       window = window)

env_no_mort <- envelope(ppp_no_mortales, fun = Gest,
                        nsim = 99, nrank = 2, verbose = FALSE)

# Exportar los dos juntos
png("figura4_entornos_confianza.png", width = 3000, height = 1500, res = 300)
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
plot(env_mort, main = "Mortales")
plot(env_no_mort, main = "No mortales")
par(mfrow = c(1, 1))
dev.off()

# 7. ANÁLISIS TEMPORAL

acc %>%
  mutate(mes_label = factor(MES, levels = 1:12,
    labels = c("Ene","Feb","Mar","Abr","May","Jun",
               "Jul","Ago","Sep","Oct","Nov","Dic"))) %>%
  group_by(mes_label) %>%
  summarise(fallecidos = sum(TOTAL_MU30DF, na.rm = TRUE)) %>%
  ggplot(aes(x = mes_label, y = fallecidos, group = 1)) +
  geom_line(color = "red", size = 1.2) +
  geom_point(color = "red", size = 3) +
  labs(x = NULL, y = "Fallecidos") +
  theme_minimal()

# 8. CASO DE USO: COMUNITAT VALENCIANA

acc %>%
  filter(COD_PROVINCIA %in% c(3, 12, 46)) %>%
  mutate(es_mortal = if_else(TOTAL_MU30DF > 0, "Mortal", "No mortal")) %>%
  group_by(COD_PROVINCIA) %>%
  summarise(tasa_mort = sum(es_mortal == "Mortal") / n() * 100) %>%
  left_join(spain %>% filter(COD_PROVINCIA %in% c(3, 12, 46)),
            by = "COD_PROVINCIA") %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = tasa_mort), color = "white") +
  scale_fill_viridis_c(option = "C", direction = -1, name = "Tasa (%)") +
  theme_minimal()

# 1. Top 5 provincias por tasa
por_prov %>% arrange(desc(tasa_mort)) %>% head(5)

# 2. Por tipo de vía
acc %>%
  group_by(TIPO_VIA) %>%
  summarise(
    accidentes = n(),
    fallecidos = sum(TOTAL_MU30DF, na.rm = TRUE),
    tasa = fallecidos / accidentes * 100
  ) %>%
  arrange(desc(tasa))

# 3. Urbana vs interurbana
acc %>%
  mutate(zona_label = if_else(ZONA_AGRUPADA == 1, "Interurbana", "Urbana")) %>%
  group_by(zona_label) %>%
  summarise(
    accidentes = n(),
    fallecidos = sum(TOTAL_MU30DF, na.rm = TRUE),
    tasa = fallecidos / accidentes * 100
  )

# 4. Mensual
acc %>%
  group_by(MES) %>%
  summarise(fallecidos = sum(TOTAL_MU30DF, na.rm = TRUE))

# 5. Comunitat Valenciana
acc %>%
  filter(COD_PROVINCIA %in% c(3, 12, 46)) %>%
  group_by(COD_PROVINCIA) %>%
  summarise(
    accidentes = n(),
    fallecidos = sum(TOTAL_MU30DF, na.rm = TRUE),
    tasa = sum(TOTAL_MU30DF > 0) / n() * 100
  )

# Desglose fallecidos por tipo de usuario
acc %>%
  summarise(
    peatones    = sum(TOT_PEAT_MU30DF, na.rm = TRUE),
    ciclistas   = sum(TOT_BICI_MU30DF, na.rm = TRUE),
    ciclomotor  = sum(TOT_CICLO_MU30DF, na.rm = TRUE),
    motoristas  = sum(TOT_MOTO_MU30DF, na.rm = TRUE),
    turismos    = sum(TOT_TUR_MU30DF, na.rm = TRUE),
    furgonetas  = sum(TOT_FURG_MU30DF, na.rm = TRUE),
    camiones    = sum(TOT_CAM_MAS3500_MU30DF, na.rm = TRUE)
  )
