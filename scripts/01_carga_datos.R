# 01 - CARGA DE DATOS CRUDOS

if (isTRUE(getOption("visor.datos_cargados"))) {

  message("[01_carga_datos] datos ya cargados en la sesión; se omite la recarga.")

} else {

# Librerías generales

library(tidyverse)
library(sf)
library(readxl)
library(survey)
library(psych)
library(factoextra)
library(echarts4r)

# Lectura tolerante de los capítulos de la Encuesta Multipropósito

emb_faltantes <- character(0)

leer_emb <- function(ruta, ...) {
  if (!file.exists(ruta)) {
    emb_faltantes <<- c(emb_faltantes, ruta)
    return(NULL)
  }
  read.csv2(ruta, fileEncoding = "latin1", ...)
}

#Salud y Nutrición

osb_malnutricion5agnos <- read.csv("data/raw/osb_malnutricion5agnos.csv", sep=";")

osb_malnutricion5_17anos <- read.csv("data/raw/osb_malnutricion5_17anos.xlsx.csv", sep=";")

osb_tm_infantil <- read.csv("data/raw/osb_tm_infantil.csv", sep=";")

salud <- leer_emb("data/raw/Encuesta Multiproposito Bogota/Capitulo F/Salud (Capítulo F).csv")

identificacion <- leer_emb("data/raw/Encuesta Multiproposito Bogota/Capitulo A/Identificación (Capítulo A).csv", comment.char="#")

demografia <- leer_emb("data/raw/Encuesta Multiproposito Bogota/Capitulo E/Composición del hogar y demografía (Capítulo E).csv")

#Integracion


centro_proteger <- st_read("data/shapefiles/centros-proteger.shp/CENTROS PROTEGER.shp", quiet = TRUE)

espacios_rurales <- st_read("data/shapefiles/espacios-rurales.shp/ESPACIOS RURALES.shp", quiet = TRUE)

jardin_infantil_nocturno <- st_read("data/shapefiles/jardin-infantil-nocturno.shp/JARDIN INFANTIL NOCTURNO.shp", quiet = TRUE)

centro_renacer <- st_read("data/shapefiles/centro-renacer.shp/CENTRO RENACER.shp", quiet = TRUE)

centro_abrazar <- st_read("data/shapefiles/centro-abrazar.shp/CENTRO ABRAZAR.shp", quiet = TRUE)

jardin_infantil <- st_read("data/shapefiles/jardines-infantiles.shp/JARDINES INFANTILES.shp", quiet = TRUE)

centro_amar <- st_read("data/shapefiles/centro-amar.shp/CENTRO AMAR.shp", quiet = TRUE)

centro_crecer <- st_read("data/shapefiles/centros-crecer.shp/CENTROS CRECER.shp", quiet = TRUE)

casa_pensamiento_intercultural <- st_read("data/shapefiles/casas-de-pensamiento-intercultural.shp/CASAS DE PENSAMIENTO INTERCULTURAL.shp", quiet = TRUE)

comisarias_familia <- st_read("data/shapefiles/acceso-a-la-justicia-comisarias-de-familia.shp/ACCESO A LA JUSTICIA - COMISARIAS DE FAMILIA.shp", quiet = TRUE)

violencia <- read.csv("data/raw/osb_saludmental-vintrafamiliar.csv", sep=";")

poblacion <- read_excel("data/raw/anexo-proyecciones-poblacion-bogota-desagreacion-loc-2018-2035-UPZ-2018-2024.xlsx", 
                        range = "A12:KX1092")

#Economico

cap_A <- leer_emb("data/raw/Encuesta Multiproposito Bogota/Capitulo A/Identificación (Capítulo A).csv")
cap_B <- leer_emb("data/raw/Encuesta Multiproposito Bogota/Capitulo B/Datos de la vivenda y su entorno (Capítulo B).csv")
cap_C <- leer_emb("data/raw/Encuesta Multiproposito Bogota/Capitulo C/Condiciones habitacionales del hogar (Capítulo C).csv")
cap_D <- leer_emb("data/raw/Encuesta Multiproposito Bogota/Capitulo D/Servicios públicos domiciliarios y de TIC (Capítulo D).csv")
cap_E <- leer_emb("data/raw/Encuesta Multiproposito Bogota/Capitulo E/Composición del hogar y demografía (Capítulo E).csv")
cap_F <- leer_emb("data/raw/Encuesta Multiproposito Bogota/Capitulo F/Salud (Capítulo F).csv")
cap_G <- leer_emb("data/raw/Encuesta Multiproposito Bogota/Capitulo G/Atención integral de los niños y niñas menores de 5 anos (Capítulo G).csv")
cap_H <- leer_emb("data/raw/Encuesta Multiproposito Bogota/Capitulo H/Educación (Capitulo H).csv")
cap_I <- leer_emb("data/raw/Encuesta Multiproposito Bogota/Capitulo I/Uso de tecnologías de la información, TIC (Capítulo I).csv")
cap_J <- leer_emb("data/raw/Encuesta Multiproposito Bogota/Capitulo J/Participación en organizaciones y redes sociales (Capítulo J).csv")
cap_K <- leer_emb("data/raw/Encuesta Multiproposito Bogota/Capitulo K/Fuerza de trabajo (Capítulo K).csv")
cap_L <- leer_emb("data/raw/Encuesta Multiproposito Bogota/Capitulo L/Percepción sobre las condiciones de vida y el desempeño institucional (Capítulo L).csv")
cap_M1 <- leer_emb("data/raw/Encuesta Multiproposito Bogota/Capitulo M1/Gastos en alimentos y bebidas no alcohólicas de los hogares (Capítulo M1).csv")
cap_M2 <- leer_emb("data/raw/Encuesta Multiproposito Bogota/Capitulo M2/Gastos trimestrales y anuales del hogar (Capitulo M2)/Gastos trimestrales y anuales del hogar (Capitulo M2).csv")

#Educacion

colegios <- st_read("data/shapefiles/colegios12.24/Colegios12_2024.shp", quiet = TRUE)

beneficiarios <- st_read("data/shapefiles/beneficiarios03_2025/Beneficiarios_032025.shp", quiet = TRUE)

tasaaceptacion <- st_read("data/shapefiles/tasasaprobacion2024/TasasAprobacion2024.shp", quiet = TRUE)

tasadesercion <- st_read("data/shapefiles/tdesercionof/TDesercionOf.shp", quiet = TRUE)

tasareprobacion <- st_read("data/shapefiles/treprobacionof/TReprobacionOf.shp", quiet = TRUE)

#Shapefile Localidades

loca <- st_read("data/shapefiles/locashp/Loca.shp",quiet = TRUE)

if (length(emb_faltantes) > 0) {
  warning(
    "[01_carga_datos] Capítulos de la Encuesta Multipropósito ausentes en ",
    "data/raw/ (", length(emb_faltantes), "):\n  ",
    paste(emb_faltantes, collapse = "\n  "),
    "\nLos scripts que dependan de ellos reutilizarán la capa de insumos ",
    "en data/processed/ en lugar de recalcularlos desde microdatos.",
    call. = FALSE
  )
}

options(visor.datos_cargados = TRUE)
message("[01_carga_datos] carga de datos crudos completada.")

}