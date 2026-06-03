# Patrones espaciales de accidentes de tráfico en España (2023)

Código R desarrollado como material suplementario del trabajo final de la asignatura **Datos Espaciales y Espaciotemporales (DEE)**, Grado en Inteligencia y Analítica de Negocios (BIA).

## Contenido

| Script | Descripción |
|--------|-------------|
| `01_analisis_principal.R` | Análisis descriptivo, mapas coropléticos, patrones de puntos (CSR, función G), autocorrelación espacial (Moran global, LISA), factores territoriales y análisis temporal |
| `02_mapas_interactivos.R` | Mapas interactivos con leaflet: multicapa provincial/municipal, fallecidos absolutos, clusters LISA y zoom Comunitat Valenciana |

## Datos

Los datos no se incluyen en este repositorio. Deben descargarse directamente de las fuentes oficiales:

- **DGT — Microdatos de accidentes con víctimas 2023:**  
  https://www.dgt.es/menusecundario/dgt-en-cifras/dgt-en-cifras-resultados/dgt-en-cifras-detalle/Ficheros-microdatos-de-accidentes-con-victimas-2023/

- **INE — Densidad de población por provincias (2021):**  
  https://www.ine.es/jaxiT3/Tabla.htm?t=2852

Una vez descargados, colocar los archivos en el mismo directorio que los scripts con los nombres:
- `TABLA_ACCIDENTES_23.xlsx`
- `2852.xlsx`

## Paquetes necesarios

```r
install.packages(c("tidyverse", "sf", "mapSpain", "readxl",
                   "spdep", "spatstat", "RColorBrewer",
                   "leaflet", "htmlwidgets"))
```

## Orden de ejecución

Ejecutar primero `01_analisis_principal.R` y después `02_mapas_interactivos_1.R`, ya que el segundo depende de los objetos generados en el primero.
