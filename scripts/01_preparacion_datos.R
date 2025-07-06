#en este script voy a unir los datos de delitos en la Ciudad de Buenos Aires
#desde 2019 a 2023 para despues realizar el analisis exploratorio de los mismos
#como es medio pesado y tarda un poco en correr, elegi hacerlo en un script separado
#al resto, para que no se trabe todo.
#Los resultados se guardan en la carpeta "input" ya que es el input
#para el script siguiente.
# =======================
#   LIBRERÍAS Y RUTAS
# =======================
library(dplyr)
library(lubridate)
library(readr)
library(purrr)
library(openxlsx)
library(stringr)
library(sf)

#seteo de rutas
base_dir <- getwd()         
project_dir <- dirname(base_dir) 

instub <- file.path(project_dir, "raw")
outstub <- file.path(project_dir, "input")


# =======================
#   UNION DE DATA SETS
# =======================

archivos <- list.files(path = instub, pattern = "*.csv", full.names = TRUE)

#en alguno de los datasets una de las columnas "franja" estaba como
#caracter en vez de numero, lo que generaba un error al unir los datasets.
#por eso inclui el mutate.
delitos <- archivos %>%
  map_dfr(~ read_csv(.x) %>%
            mutate(
              franja = as.numeric(franja),
              latitud = if_else(
                str_count(latitud, "\\.") > 1,
                as.numeric(gsub("\\.", "", latitud)) / 1e6,
                as.numeric(latitud)
              ),
              longitud = if_else(
                str_count(longitud, "\\.") > 1,
                as.numeric(gsub("\\.", "", longitud)) / 1e6,
                as.numeric(longitud)
              )
            ))

write_csv(delitos, file.path(outstub, "delitos_unidos_2019_2023.csv"))

# ==============================
# ARCHIVO GEOJSON PARA EL MAPA
# ==============================

# Leer geometría de barrios CABA
#En barrio converti "BOCA" a "LA BOCA" porque quedaba raro.
#Lo hago en este script para no complejizar el siguiente.

barrios_caba <- st_read(file.path(instub, "barrios.geojson")) %>%
  mutate(
    nombre = str_to_upper(str_trim(nombre)),
    nombre = ifelse(str_detect(nombre, "BOCA"), "LA BOCA", nombre)
  )

st_write(barrios_caba, file.path(outstub, "barrios_caba.geojson"), append = FALSE)
#esta parte tarda un poco. 
