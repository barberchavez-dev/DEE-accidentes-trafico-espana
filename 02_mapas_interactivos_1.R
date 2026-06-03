# Patrones espaciales de accidentes de tráfico en España (2023)
# Script 02: Mapas interactivos (leaflet)
# Autor: Lisa Estela Barber Chávez
# Asignatura: Datos Espaciales y Espaciotemporales (DEE)
# Nota: ejecutar después de 01_analisis_principal.R

library(leaflet)
library(htmlwidgets)

# ETIQUETAS AUXILIARES

tipo_via_labels <- c(
  "1" = "Autopista peaje", "2" = "Autopista libre",
  "3" = "Autovía",         "4" = "Vía p/ automóviles",
  "5" = "Conv. doble calzada", "6" = "Conv. calzada única",
  "7" = "Vía de servicio", "8" = "Ramal enlace",
  "9" = "Calle",           "10" = "Camino vecinal",
  "11" = "Recinto",        "14" = "Otro"
)

condicion_meteo_labels <- c(
  "1" = "Buen tiempo", "2" = "Lluvia",  "3" = "Granizo",
  "4" = "Nieve",       "5" = "Niebla",  "6" = "Viento fuerte",
  "7" = "Otro"
)

tipo_accidente_labels <- c(
  "1"  = "Colisión frontal",         "2"  = "Colisión frontolateral",
  "3"  = "Colisión lateral",         "4"  = "Colisión por alcance",
  "5"  = "Colisión múltiple",        "6"  = "Choque con obstáculo en calzada",
  "7"  = "Choque con obstáculo en margen", "8"  = "Vuelco",
  "9"  = "Caída de ocupante",        "10" = "Atropello peatón",
  "11" = "Accidente animal",         "12" = "Otro"
)

# PREPARACIÓN DE DATOS MUNICIPALES

mortales_detalle <- acc %>%
  filter(TOTAL_MU30DF > 0, !is.na(COD_MUNICIPIO), COD_MUNICIPIO != "00000") %>%
  mutate(
    TIPO_VIA        = as.character(TIPO_VIA),
    CONDICION_METEO = as.character(CONDICION_METEO),
    TIPO_ACCIDENTE  = as.character(TIPO_ACCIDENTE)
  ) %>%
  group_by(COD_MUNICIPIO) %>%
  summarise(
    n_mortales          = n(),
    n_fallecidos        = sum(TOTAL_MU30DF, na.rm = TRUE),
    via_frecuente       = names(sort(table(TIPO_VIA), decreasing = TRUE))[1],
    meteo_frecuente     = names(sort(table(CONDICION_METEO), decreasing = TRUE))[1],
    accidente_frecuente = names(sort(table(TIPO_ACCIDENTE), decreasing = TRUE))[1]
  ) %>%
  mutate(
    via_label       = recode(via_frecuente, !!!tipo_via_labels, .default = "Desconocido"),
    meteo_label     = recode(meteo_frecuente, !!!condicion_meteo_labels, .default = "Desconocido"),
    accidente_label = recode(accidente_frecuente, !!!tipo_accidente_labels, .default = "Desconocido")
  )

muni_sf2 <- municipios %>%
  left_join(mortales_detalle, by = "COD_MUNICIPIO") %>%
  filter(!is.na(n_mortales))

centroides2 <- muni_sf2 %>%
  st_centroid() %>%
  st_transform(crs = 4326)

coords2          <- st_coordinates(centroides2)
centroides2$lng  <- coords2[, 1]
centroides2$lat  <- coords2[, 2]

# Capa provincial en WGS84
mapa_leaf <- st_transform(mapa_moran, crs = 4326)

# 1. MAPA MULTICAPA: TASA MORTALIDAD + MUNICIPIOS

pal_prov <- colorNumeric(
  palette  = c("#fdcc8a", "#fc8d59", "#d7301f", "#7f0000"),
  domain   = mapa_leaf$tasa_mort,
  na.color = "#d9d9d9"
)

pal_munic2 <- colorNumeric(
  palette  = c("#c6dbef", "#6baed6", "#2171b5", "#08306b"),
  domain   = centroides2$n_mortales,
  na.color = "#d9d9d9"
)

popup_prov2 <- paste0(
  "<b>", mapa_leaf$ine.prov.name, "</b><br>",
  "Tasa mortalidad: <b>", round(mapa_leaf$tasa_mort, 2), "%</b><br>",
  "Accidentes mortales: <b>", mapa_leaf$acc_mortales, "</b><br>",
  "Total fallecidos: <b>", mapa_leaf$fallecidos, "</b>"
)

popup_munic2 <- paste0(
  "<b>", centroides2$name, "</b><br>",
  "<i>", centroides2$prov.shortname.es, "</i><br><br>",
  "Accidentes mortales: <b>", centroides2$n_mortales, "</b><br>",
  "Total fallecidos: <b>", centroides2$n_fallecidos, "</b><br>",
  "Tipo de vía más frecuente: <b>", centroides2$via_label, "</b><br>",
  "Condición meteorológica: <b>", centroides2$meteo_label, "</b><br>",
  "Tipo de accidente: <b>", centroides2$accidente_label, "</b>"
)

mapa_multicapa <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    data         = mapa_leaf,
    fillColor    = ~pal_prov(tasa_mort),
    fillOpacity  = 0.7,
    color        = "white", weight = 1,
    popup        = popup_prov2,
    label        = paste0(mapa_leaf$ine.prov.name, ": ", round(mapa_leaf$tasa_mort, 2), "%"),
    labelOptions = labelOptions(style = list("font-weight" = "bold", "font-size" = "13px")),
    highlight    = highlightOptions(weight = 2, color = "#333", fillOpacity = 0.9, bringToFront = TRUE),
    group        = "Provincias"
  ) %>%
  addCircleMarkers(
    data        = centroides2, lng = ~lng, lat = ~lat,
    radius      = ~sqrt(n_mortales) * 2,
    fillColor   = ~pal_munic2(n_mortales), fillOpacity = 0.8,
    color       = "white", weight = 0.5,
    popup       = popup_munic2,
    group       = "Municipios"
  ) %>%
  addLayersControl(
    overlayGroups = c("Provincias", "Municipios"),
    options       = layersControlOptions(collapsed = FALSE)
  ) %>%
  addLegend(
    position  = "bottomright", pal = pal_prov, values = mapa_leaf$tasa_mort,
    title     = "Tasa mortalidad (%)", labFormat = labelFormat(suffix = "%"), opacity = 0.9
  ) %>%
  setView(lng = -3.7, lat = 40.4, zoom = 6)

saveWidget(mapa_multicapa, "mapa_multicapa_interactivo.html", selfcontained = TRUE)

# 2. MAPA: FALLECIDOS ABSOLUTOS POR PROVINCIA

pal_fallecidos <- colorNumeric(palette = "YlOrRd", domain = mapa_leaf$fallecidos, na.color = "#d9d9d9")

popup_fallecidos <- paste0(
  "<b>", mapa_leaf$ine.prov.name, "</b><br>",
  "Total fallecidos: <b>", mapa_leaf$fallecidos, "</b><br>",
  "Accidentes mortales: <b>", mapa_leaf$acc_mortales, "</b><br>",
  "Total accidentes: <b>", mapa_leaf$total_acc, "</b><br>",
  "Tasa mortalidad: <b>", round(mapa_leaf$tasa_mort, 2), "%</b>"
)

mapa_fallecidos <- leaflet(mapa_leaf) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    fillColor = ~pal_fallecidos(fallecidos), fillOpacity = 0.8,
    color = "white", weight = 1, popup = popup_fallecidos,
    highlight = highlightOptions(weight = 2, color = "#333", fillOpacity = 0.95, bringToFront = TRUE)
  ) %>%
  addLegend(position = "bottomright", pal = pal_fallecidos, values = ~fallecidos,
            title = "Fallecidos", opacity = 0.9) %>%
  setView(lng = -3.7, lat = 40.4, zoom = 5)

saveWidget(mapa_fallecidos, "mapa_fallecidos_interactivo.html", selfcontained = TRUE)

# 3. MAPA: ANÁLISIS LISA

colores_lisa <- c(
  "High-High"        = "#d73027", "Low-Low"  = "#4575b4",
  "High-Low"         = "#f46d43", "Low-High" = "#74add1",
  "No significativo" = "#d9d9d9"
)

pal_lisa <- colorFactor(palette = unname(colores_lisa), levels = names(colores_lisa))

popup_lisa <- paste0(
  "<b>", mapa_leaf$ine.prov.name, "</b><br>",
  "Cluster LISA: <b>", mapa_leaf$lisa_cluster, "</b><br>",
  "Tasa mortalidad: <b>", round(mapa_leaf$tasa_mort, 2), "%</b><br>",
  "Accidentes mortales: <b>", mapa_leaf$acc_mortales, "</b><br>",
  "Total fallecidos: <b>", mapa_leaf$fallecidos, "</b>"
)

mapa_interactivo <- leaflet(mapa_leaf) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    fillColor = ~pal_lisa(lisa_cluster), fillOpacity = 0.75,
    color = "white", weight = 1, popup = popup_lisa,
    highlight = highlightOptions(weight = 2, color = "#333", fillOpacity = 0.95, bringToFront = TRUE)
  ) %>%
  addLegend(position = "bottomright", pal = pal_lisa, values = ~lisa_cluster,
            title = "Cluster LISA", opacity = 0.9) %>%
  setView(lng = -3.7, lat = 40.4, zoom = 5)

saveWidget(mapa_interactivo, "mapa_LISA_interactivo.html", selfcontained = TRUE)

# 4. MAPA: COMUNITAT VALENCIANA

mapa_cv <- mapa_leaf %>%
  filter(COD_PROVINCIA %in% c(3, 12, 46)) %>%
  mutate(tasa_mort = acc_mortales / total_acc * 100)

mortales_cv <- acc %>%
  filter(TOTAL_MU30DF > 0, !is.na(COD_MUNICIPIO),
         COD_MUNICIPIO != "00000", COD_PROVINCIA %in% c(3, 12, 46)) %>%
  mutate(across(c(TIPO_VIA, CONDICION_METEO, TIPO_ACCIDENTE), as.character)) %>%
  group_by(COD_MUNICIPIO) %>%
  summarise(
    n_mortales          = n(),
    n_fallecidos        = sum(TOTAL_MU30DF, na.rm = TRUE),
    via_frecuente       = names(sort(table(TIPO_VIA), decreasing = TRUE))[1],
    meteo_frecuente     = names(sort(table(CONDICION_METEO), decreasing = TRUE))[1],
    accidente_frecuente = names(sort(table(TIPO_ACCIDENTE), decreasing = TRUE))[1]
  ) %>%
  mutate(
    via_label       = recode(via_frecuente, !!!tipo_via_labels, .default = "Desconocido"),
    meteo_label     = recode(meteo_frecuente, !!!condicion_meteo_labels, .default = "Desconocido"),
    accidente_label = recode(accidente_frecuente, !!!tipo_accidente_labels, .default = "Desconocido")
  )

muni_cv <- municipios %>%
  left_join(mortales_cv, by = "COD_MUNICIPIO") %>%
  filter(!is.na(n_mortales))

centroides_cv <- muni_cv %>% st_centroid() %>% st_transform(crs = 4326)
coords_cv         <- st_coordinates(centroides_cv)
centroides_cv$lng <- coords_cv[, 1]
centroides_cv$lat <- coords_cv[, 2]

pal_cv_prov  <- colorNumeric(palette = c("#fdcc8a", "#fc8d59", "#d7301f"),
                              domain = mapa_cv$tasa_mort, na.color = "#d9d9d9")
pal_cv_munic <- colorNumeric(palette = c("#c6dbef", "#6baed6", "#2171b5", "#08306b"),
                              domain = centroides_cv$n_mortales, na.color = "#d9d9d9")

popup_cv_munic <- paste0(
  "<b>", centroides_cv$name, "</b><br>",
  "<i>", centroides_cv$prov.shortname.es, "</i><br><br>",
  "Accidentes mortales: <b>", centroides_cv$n_mortales, "</b><br>",
  "Total fallecidos: <b>", centroides_cv$n_fallecidos, "</b><br>",
  "Tipo de vía más frecuente: <b>", centroides_cv$via_label, "</b><br>",
  "Condición meteorológica: <b>", centroides_cv$meteo_label, "</b><br>",
  "Tipo de accidente: <b>", centroides_cv$accidente_label, "</b>"
)

mapa_cv_interactivo <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    data = mapa_cv, fillColor = ~pal_cv_prov(tasa_mort), fillOpacity = 0.7,
    color = "white", weight = 1,
    popup  = paste0("<b>", mapa_cv$ine.prov.name, "</b><br>Tasa: <b>",
                    round(mapa_cv$tasa_mort, 2), "%</b>"),
    label  = paste0(mapa_cv$ine.prov.name, ": ", round(mapa_cv$tasa_mort, 2), "%"),
    highlight = highlightOptions(weight = 2, color = "#333", fillOpacity = 0.9, bringToFront = TRUE),
    group = "Provincias"
  ) %>%
  addCircleMarkers(
    data = centroides_cv, lng = ~lng, lat = ~lat,
    radius = ~sqrt(n_mortales) * 3,
    fillColor = ~pal_cv_munic(n_mortales), fillOpacity = 0.8,
    color = "white", weight = 0.5, popup = popup_cv_munic,
    group = "Municipios"
  ) %>%
  addLayersControl(overlayGroups = c("Provincias", "Municipios"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend(position = "bottomright", pal = pal_cv_prov, values = mapa_cv$tasa_mort,
            title = "Tasa mortalidad (%)", labFormat = labelFormat(suffix = "%"), opacity = 0.9) %>%
  setView(lng = -0.5, lat = 39.5, zoom = 8)

saveWidget(mapa_cv_interactivo, "mapa_cv_interactivo.html", selfcontained = TRUE)
