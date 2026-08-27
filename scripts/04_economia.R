#Datos

setwd("C:/Users/manue/OneDrive/Escritorio/a borrar/DataJam")

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


## ACP

names(economica_localidad)
variables_acp <- c(
  "ingreso_bajo",
  "consumo_alimentario_bajo",
  "baja_ocupacion",
  "tasa_desempleo",
  "tasa_informalidad"
)

datos_acp <- economica_localidad %>%
  arrange(as.numeric(as.character(COD_LOCALIDAD)))

localidades_acp <- datos_acp %>%
  select( COD_LOCALIDAD,NOMBRE_LOCALIDAD)

matriz_acp <- datos_acp %>%
  select(all_of(variables_acp)) %>%
  as.data.frame()

rownames(matriz_acp) <- localidades_acp$NOMBRE_LOCALIDAD

#ACP
acp_economico <- prcomp(
  matriz_acp,
  center = TRUE,
  scale. = TRUE
)

# Referencia general de vulnerabilidad
referencia_vulnerabilidad <- rowMeans(
  scale(matriz_acp)
)

cor_cp1_referencia <- cor(
  acp_economico$x[, 1],
  referencia_vulnerabilidad
)

cor_cp1_referencia

sentido_cp1 <- ifelse(
  cor_cp1_referencia < 0,
  -1,
  1
)

puntaje_economico <- (
  acp_economico$x[, 1] * sentido_cp1
)

# Indice
minimo_puntaje <- min(puntaje_economico)
maximo_puntaje <- max(puntaje_economico)

indice_economico <- 100 * (
  (puntaje_economico - minimo_puntaje) /
    (maximo_puntaje - minimo_puntaje)
)

resultado_economico <- datos_acp %>%
  mutate(
    puntaje_acp_economico = puntaje_economico,
    indice_economico = indice_economico,
    ranking_vulnerabilidad =
      rank(
        -indice_economico,
        ties.method = "min"
      )
  ) %>%
  arrange(desc(indice_economico))

#Guardar

saveRDS(
  resultado_economico,
  "data/processed/economia.rds"
)
