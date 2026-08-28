# 04 - DIMENSIÓN ECONOMÍA

archivos_emb_economia <- c(
  "data/raw/Encuesta Multiproposito Bogota/Capitulo E/Composición del hogar y demografía (Capítulo E).csv",
  "data/raw/Encuesta Multiproposito Bogota/Capitulo K/Fuerza de trabajo (Capítulo K).csv",
  "data/raw/Encuesta Multiproposito Bogota/Capitulo M1/Gastos en alimentos y bebidas no alcohólicas de los hogares (Capítulo M1).csv"
)

emb_economia_disponible <- all(file.exists(archivos_emb_economia))

if (emb_economia_disponible) {

#Datos

source("scripts/01_carga_datos.R")

#Tratamiento

ubicacion <- cap_A %>%
  transmute(
    DIRECTORIO = as.character(DIRECTORIO),
    COD_LOCALIDAD = as.character(COD_LOCALIDAD),
    NOMBRE_LOCALIDAD
  ) %>%
  distinct()

# Hogares con NNA y tamaño del hogar
hogares_NNA <- cap_E %>%
  mutate(
    DIRECTORIO = as.character(DIRECTORIO),
    DIRECTORIO_HOG = as.character(DIRECTORIO_HOG),
    edad = as.numeric(NPCEP4),
    es_NNA = edad >= 0 & edad <= 17
  ) %>%
  group_by(DIRECTORIO, DIRECTORIO_HOG) %>%
  summarise(
    n_personas_hogar = n(),
    n_NNA = sum(es_NNA, na.rm = TRUE),
    tiene_NNA = any(es_NNA, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(tiene_NNA) %>%
  left_join(ubicacion, by = "DIRECTORIO") %>%
  filter(!is.na(COD_LOCALIDAD))


## Indicadores laborales

# Razón ponderada
razon_ponderada <- function(numerador, denominador, peso) {
  num <- sum(peso[numerador == 1], na.rm = TRUE)
  den <- sum(peso[denominador == 1], na.rm = TRUE)
  if (den == 0) return(NA_real_)
  100 * num / den
}

labor_localidad <- cap_K %>%
  mutate(DIRECTORIO_HOG = as.character(DIRECTORIO_HOG)) %>%
  inner_join(
    hogares_NNA %>%
      select(DIRECTORIO_HOG, COD_LOCALIDAD, NOMBRE_LOCALIDAD),
    by = "DIRECTORIO_HOG"
  ) %>%
  group_by(COD_LOCALIDAD, NOMBRE_LOCALIDAD) %>%
  summarise(
    tasa_ocupacion = razon_ponderada(
      numerador = OCU,
      denominador = PET,
      peso = FEX_C
    ),
    
    tasa_desempleo = razon_ponderada(
      numerador = DES,
      denominador = FL,
      peso = FEX_C
    ),
    
    tasa_informalidad = razon_ponderada(
      numerador = OINFORMAL,
      denominador = OCU,
      peso = FEX_C
    ),
    
    .groups = "drop"
  )

## Gasto - consumo en alimento

cero_si_na <- function(x) {
  replace_na(as.numeric(x), 0)
}

mediana_ponderada <- function(x, w) {
  valido <- !is.na(x) & !is.na(w) & w > 0
  
  if (!any(valido)) return(NA_real_)
  
  x <- x[valido]
  w <- w[valido]
  
  orden <- order(x)
  x <- x[orden]
  w <- w[orden]
  
  x[which(cumsum(w) >= sum(w) / 2)[1]]
}

alimentacion_hogar <- cap_M1 %>%
  mutate(DIRECTORIO_HOG = as.character(DIRECTORIO_HOG)) %>%
  inner_join(
    hogares_NNA %>%
      select(
        DIRECTORIO_HOG,
        COD_LOCALIDAD,
        NOMBRE_LOCALIDAD,
        n_personas_hogar
      ),
    by = "DIRECTORIO_HOG"
  ) %>%
  mutate(
    gasto_alimentos_mes =
      (30.44 / 7)  * cero_si_na(NHCMP2A) +
      (30.44 / 8)  * cero_si_na(NHCMP2B) +
      (30.44 / 15) * cero_si_na(NHCMP2C) +
      (30.44 / 20) * cero_si_na(NHCMP2D) +
      cero_si_na(NHCMP2E) +
      if_else(
        !is.na(NHCMP1FB) & NHCMP1FB > 0,
        (30.44 / NHCMP1FB) * cero_si_na(NHCMP2F),
        0
      ),
    
    consumo_alimentos_mes =
      gasto_alimentos_mes + cero_si_na(NHCMP3A),
    
    consumo_alimentos_pc =
      consumo_alimentos_mes / n_personas_hogar
  )
head(alimentacion_hogar)

alimentacion_localidad <- alimentacion_hogar %>%
  group_by(COD_LOCALIDAD, NOMBRE_LOCALIDAD) %>%
  summarise(
    consumo_alimentos_pc_mediano =
      mediana_ponderada(consumo_alimentos_pc, FEX_C),
    .groups = "drop"
  )
head(alimentacion_localidad)

## Ingreso mensual per capita

ingreso_persona <- cap_K %>%
  mutate(
    DIRECTORIO_HOG = as.character(DIRECTORIO_HOG),
    ingreso_laboral_mes =
      cero_si_na(NPCKP23) +   # Salario del empleo principal
      cero_si_na(NPCKP36) +   # Ganancia u honorarios de independientes
      cero_si_na(NPCKP47A) +  # Otros trabajos o negocios
      cero_si_na(NPCKP48A)    # Ingreso laboral de personas no ocupadas actualmente
  )

ingreso_persona %>%
  summarise(
    personas_con_salario_y_ganancia =
      sum(
        cero_si_na(NPCKP23) > 0 &
          cero_si_na(NPCKP36) > 0
      )
  )

ingreso_hogar <- ingreso_persona %>%
  inner_join(
    hogares_NNA %>%
      select(
        DIRECTORIO_HOG,
        COD_LOCALIDAD,
        NOMBRE_LOCALIDAD,
        n_personas_hogar
      ),
    by = "DIRECTORIO_HOG"
  ) %>%
  group_by(
    DIRECTORIO_HOG,
    COD_LOCALIDAD,
    NOMBRE_LOCALIDAD,
    n_personas_hogar
  ) %>%
  summarise(
    ingreso_laboral_hogar = sum(ingreso_laboral_mes, na.rm = TRUE),
    FEX_C = first(FEX_C),
    .groups = "drop"
  ) %>%
  mutate(
    ingreso_laboral_pc =
      ingreso_laboral_hogar / n_personas_hogar
  )

ingreso_localidad <- ingreso_hogar %>%
  group_by(COD_LOCALIDAD, NOMBRE_LOCALIDAD) %>%
  summarise(
    ingreso_pc_mediano =
      mediana_ponderada(ingreso_laboral_pc, FEX_C),
    .groups = "drop"
  )

## 

economica_localidad <- labor_localidad %>%
  left_join(
    alimentacion_localidad,
    by = c("COD_LOCALIDAD", "NOMBRE_LOCALIDAD")
  ) %>%
  left_join(
    ingreso_localidad,
    by = c("COD_LOCALIDAD", "NOMBRE_LOCALIDAD")
  ) %>%
  mutate(
    ingreso_bajo = -log1p(ingreso_pc_mediano),
    consumo_alimentario_bajo =
      -log1p(consumo_alimentos_pc_mediano),
    baja_ocupacion = 100 - tasa_ocupacion
  )

economica_localidad <- economica_localidad %>%
  arrange(as.numeric(as.character(COD_LOCALIDAD)))

  # Capa de insumos persistida para reejecutar el ACP sin releer microdatos
  saveRDS(
    economica_localidad %>%
      select(
        COD_LOCALIDAD, NOMBRE_LOCALIDAD,
        tasa_ocupacion, tasa_desempleo, tasa_informalidad,
        consumo_alimentos_pc_mediano, ingreso_pc_mediano,
        ingreso_bajo, consumo_alimentario_bajo, baja_ocupacion
      ),
    "data/processed/insumos_economia.rds"
  )

} else {

  if (!file.exists("data/processed/insumos_economia.rds")) {
    stop(
      "[04_economia] No están los capítulos E/K/M1 de la EMB ni la capa de ",
      "insumos data/processed/insumos_economia.rds. Faltan:\n  ",
      paste(archivos_emb_economia[!file.exists(archivos_emb_economia)], collapse = "\n  ")
    )
  }

  message(
    "[04_economia] Capítulos E/K/M1 de la EMB no disponibles. Se reutiliza ",
    "data/processed/insumos_economia.rds (indicadores por localidad ya calculados)."
  )

  suppressPackageStartupMessages(library(dplyr))
  economica_localidad <- readRDS("data/processed/insumos_economia.rds")

}

# ÍNDICE DE LA DIMENSIÓN ECONOMÍA

source("scripts/00_funciones.R")

variables_economia <- c(
  "ingreso_bajo",
  "consumo_alimentario_bajo",
  "baja_ocupacion",
  "tasa_desempleo",
  "tasa_informalidad"
)

datos_acp <- economica_localidad %>%
  arrange(as.numeric(as.character(COD_LOCALIDAD)))

dimension_economia <- construir_dimension(
  datos       = datos_acp,
  variables   = variables_economia,
  protectoras = character(0),
  etiqueta    = "04_economia"
)

resultado_economico <- datos_acp %>%
  mutate(
    puntaje_economico = dimension_economia$indice,
    indice_economico  = dimension_economia$indice_100,
    ranking_vulnerabilidad =
      rank(
        -indice_economico,
        ties.method = "min"
      )
  ) %>%
  arrange(desc(indice_economico))

stopifnot(cor(resultado_economico$indice_economico, resultado_economico$ingreso_pc_mediano) < 0)
stopifnot(cor(resultado_economico$indice_economico, resultado_economico$tasa_desempleo) > 0)

saveRDS(dimension_economia, "data/processed/dimension_economia.rds")

#Guardar

saveRDS(
  resultado_economico,
  "data/processed/economia.rds"
)
