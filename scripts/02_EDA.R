#en este script voy a sacar distintas estadisticas descriptivas
#y agrupar datos de una manera que sea util para el analisis posterior

# =======================
#   LIBRERÍAS Y RUTAS
# =======================
library(plotly)
library(dplyr)
library(lubridate)
library(readr)
library(purrr)
library(openxlsx)
library(stringr)
library(ggthemes)
library(scales)

#seteo de rutas
base_dir <- getwd()         
project_dir <- dirname(base_dir) 

instub <- file.path(project_dir, "input")
outstub <- file.path(project_dir, "output")

delitos<- read.csv(file.path(instub, "delitos_unidos_2019_2023.csv"))

options(scipen=999)

#vistazo general
glimpse(delitos)
summary(delitos)
colSums(is.na(delitos))

#Si bien me dice que no hay valores NA en barrio, veo que hay valores NULL
sum(delitos$barrio == "NULL", na.rm = TRUE)
#Decidi eliminar estas filas, ya que la muestra continua siendo lo suficientemente
#grande 

delitos <- delitos %>%
  filter(!is.na(barrio), barrio != "NULL")

#Ahora, para aquellas filas con NA en longitud/latitud, decidi no eliminarlas,
#ya que igualmente contaban con información del Barrio que es lo relevante para este 
#análisis.

#En barrio converti "BOCA" a "LA BOCA" porque quedaba raro.
delitos <- delitos %>%
  mutate(barrio = ifelse(str_detect(barrio, regex("BOCA", ignore_case = TRUE)), 
                         "LA BOCA", 
                         barrio))
# =========================
#   ANÁLISIS UNIVARIANTE
# =========================

# Tablas de frecuencia
table(delitos$tipo)
table(delitos$subtipo)
table(delitos$barrio)
table(delitos$uso_arma)
table(delitos$franja)

#Tablas de proporcion
round(prop.table(table(delitos$tipo)) * 100, 1)
round(prop.table(table(delitos$subtipo)) * 100, 1)
round(prop.table(table(delitos$uso_arma)) * 100, 1)
round(prop.table(table(delitos$uso_moto)) * 100, 1)
round(prop.table(table(delitos$barrio)) * 100, 1)
round(prop.table(table(delitos$franja)) * 100, 1)
round(prop.table(table(delitos$mes)) * 100, 1)

# Resumen numérico

summary(delitos$franja)
summary(delitos$anio)

# =========================
#    ANÁLISIS BIVARIADO
# =========================

#Relacion tipo de délito - uso de arma
table(delitos$tipo, delitos$uso_arma)
round(prop.table(table(delitos$tipo, delitos$uso_arma), margin = 1) * 100, 1)

#Relacion tipo de délito - uso de moto
table(delitos$tipo, delitos$uso_moto)
round(prop.table(table(delitos$tipo, delitos$uso_moto), margin = 1) * 100, 1)

#Relacion tipo de délito - bario
tabla <- table(delitos$barrio, delitos$tipo)
round(prop.table(table(delitos$barrio, delitos$tipo), margin = 1) * 100, 1)


glimpse(delitos)

#Este es el archivo con el que se genera el tablero y los gráficos después.
write_csv(delitos, file.path(instub, "delitos.csv"))

