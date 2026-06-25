# Patrones espaciales de accidentes de tráfico en España (2023)
Código R desarrollado como material suplementario del trabajo final de la asignatura Datos Espaciales y Espacio-temporales (DEE).

El repositorio contiene dos scripts. El primero, 01_analisis_principal.R, incluye todo el análisis del trabajo: carga y procesamiento de los microdatos de la DGT, mapas coropléticos por provincia, análisis de patrones de puntos, autocorrelación espacial (índice de Moran global y análisis LISA), factores territoriales y análisis longitudinal. El segundo, 02_mapas_interactivos.R, genera los mapas interactivos con leaflet y depende de los objetos creados en el primero, por lo que debe ejecutarse después.

Los datos no se incluyen en el repositorio y deben descargarse directamente de las fuentes oficiales: los microdatos de accidentes de la DGT (https://www.dgt.es) y los datos de población provincial del INE (https://www.ine.es/jaxiT3/Tabla.htm?t=2852). Una vez descargados, deben colocarse en el mismo directorio que los scripts con los nombres TABLA_ACCIDENTES_23.xlsx y 2852.xlsx.

Los paquetes necesarios pueden instalarse con:
install.packages(c("tidyverse", "sf", "mapSpain", "readxl", "spdep", "spatstat", "RColorBrewer", "leaflet", "htmlwidgets"))

