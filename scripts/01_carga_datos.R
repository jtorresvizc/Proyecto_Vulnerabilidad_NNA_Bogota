# Librerías generales

setwd("C:/Users/manue/OneDrive/Escritorio/a borrar/DataJam")

library(tidyverse)
library(sf)
library(readxl)
library(survey)
library(psych)
library(factoextra)
library(echarts4r)

#Salud y Nutrición

osb_malnutricion5agnos <- read.csv("data/raw/osb_malnutricion5agnos.csv", sep=";")

osb_malnutricion5_17anos <- read.csv("data/raw/osb_malnutricion5_17anos.xlsx.csv", sep=";")

osb_tm_infantil <- read.csv("data/raw/osb_tm_infantil.csv", sep=";")

salud <- read.csv2("data/raw/Encuesta Multiproposito Bogota/Capitulo F/Salud (Capítulo F).csv")

identificacion <- read.csv2("data/raw/Encuesta Multiproposito Bogota/Capitulo A/Identificación (Capítulo A).csv", comment.char="#")

demografia <- read.csv2("data/raw/Encuesta Multiproposito Bogota/Capitulo E/Composición del hogar y demografía (Capítulo E).csv")

#Integracion


centro_proteger <- st_read("data/shapefiles/centros-proteger.shp/CENTROS PROTEGER.shp", quiet = TRUE)

espacios_rurales <- st_read("data/shapefiles/espacios-rurales.shp/ESPACIOS RURALES.shp", quiet = TRUE)

jardin_infantil_nocturno <- st_read("data/shapefiles/jardin-infantil-nocturno.shp/JARDIN INFANTIL NOCTURNO.shp", quiet = TRUE)

centro_renacer <- st_read("data/shapefiles/centro-renacer.shp/CENTRO RENACER.shp", quiet = TRUE)

centro_abrazar <- st_read("data/shapefiles/centro-abrazar.shp/CENTRO ABRAZAR.shp", quiet = TRUE)

jardin_infantil <- st_read("data/shapefiles/jardines-infantiles.shp/JARDINES INFANTILES.shp", quiet = TRUE)

centro_amar <- st_read("data/shapefiles/centro-amar.shp/CENTRO AMAR.shp", quiet = TRUE)

centro_crecer <- st_read("data/shapefiles/centros-crecer.shp/CENTROs CRECER.shp", quiet = TRUE)

casa_pensamiento_intercultural <- st_read("data/shapefiles/casas-de-pensamiento-intercultural.shp/CASAS DE PENSAMIENTO INTERCULTURAL.shp", quiet = TRUE)

comisarias_familia <- st_read("data/shapefiles/acceso-a-la-justicia-comisarias-de-familia.shp/ACCESO A LA JUSTICIA - COMISARIAS DE FAMILIA.shp", quiet = TRUE)

violencia <- read.csv("data/raw/osb_saludmental-vintrafamiliar.csv", sep=";")

poblacion <- read_excel("data/raw/anexo-proyecciones-poblacion-bogota-desagreacion-loc-2018-2035-UPZ-2018-2024.xlsx", 
                        range = "A12:KX1092")

#Economico

cap_A <- read.csv2("data/raw/Encuesta Multiproposito Bogota/Capitulo A/Identificación (Capítulo A).csv", fileEncoding = "latin1")
cap_B <- read.csv2("data/raw/Encuesta Multiproposito Bogota/Capitulo B/Datos de la vivenda y su entorno (Capítulo B).csv", fileEncoding = "latin1")
cap_C <- read.csv2("data/raw/Encuesta Multiproposito Bogota/Capitulo C/Condiciones habitacionales del hogar (Capítulo C).csv", fileEncoding = "latin1")
cap_D <- read.csv2("data/raw/Encuesta Multiproposito Bogota/Capitulo D/Servicios públicos domiciliarios y de TIC (Capítulo D).csv", fileEncoding = "latin1")
cap_E <- read.csv2("data/raw/Encuesta Multiproposito Bogota/Capitulo E/Composición del hogar y demografía (Capítulo E).csv", fileEncoding = "latin1")
cap_F <- read.csv2("data/raw/Encuesta Multiproposito Bogota/Capitulo F/Salud (Capítulo F).csv", fileEncoding = "latin1")
cap_G <- read.csv2("data/raw/Encuesta Multiproposito Bogota/Capitulo G/Atención integral de los niños y niñas menores de 5 anos (Capítulo G).csv", fileEncoding = "latin1")
cap_H <- read.csv2("data/raw/Encuesta Multiproposito Bogota/Capitulo H/Educación (Capitulo H).csv", fileEncoding = "latin1")
cap_I <- read.csv2("data/raw/Encuesta Multiproposito Bogota/Capitulo I/Uso de tecnologías de la información, TIC (Capítulo I).csv", fileEncoding = "latin1")
cap_J <- read.csv2("data/raw/Encuesta Multiproposito Bogota/Capitulo J/Participación en organizaciones y redes sociales (Capítulo J).csv", fileEncoding = "latin1")
cap_K <- read.csv2("data/raw/Encuesta Multiproposito Bogota/Capitulo K/Fuerza de trabajo (Capítulo K).csv",  fileEncoding = "latin1")
cap_L <- read.csv2("data/raw/Encuesta Multiproposito Bogota/Capitulo L/Percepción sobre las condiciones de vida y el desempeño institucional (Capítulo L).csv", fileEncoding = "latin1")
cap_M1 <- read.csv2("data/raw/Encuesta Multiproposito Bogota/Capitulo M1/Gastos en alimentos y bebidas no alcohólicas de los hogares (Capítulo M1).csv", fileEncoding = "latin1")
cap_M2 <- read.csv2("data/raw/Encuesta Multiproposito Bogota/Capitulo M2/Gastos trimestrales y anuales del hogar (Capitulo M2)/Gastos trimestrales y anuales del hogar (Capitulo M2).csv", fileEncoding = "latin1")

#Educacion

colegios <- st_read("data/shapefiles/colegios12.24/Colegios12_2024.shp", quiet = TRUE)

beneficiarios <- st_read("data/shapefiles/beneficiarios03_2025/Beneficiarios_032025.shp", quiet = TRUE)

tasaaceptacion <- st_read("data/shapefiles/tasasaprobacion2024/TasasAprobacion2024.shp", quiet = TRUE)

tasadesercion <- st_read("data/shapefiles/tdesercionof/TDesercionOf.shp", quiet = TRUE)

tasareprobacion <- st_read("data/shapefiles/treprobacionof/TReprobacionOf.shp", quiet = TRUE)

#Shapefile Localidades

loca <- st_read("data/shapefiles/locashp/Loca.shp",quiet = TRUE)