
#Datos

setwd("C:/Users/manue/OneDrive/Escritorio/a borrar/DataJam")

source("scripts/02_salud.R")
source("scripts/03_proteccion.R")
source("scripts/04_economia.R")
source("scripts/05_educacion.R")

ind_salud <- readRDS(
  "data/processed/salud.rds"
)

ind_proteccion <- readRDS(
  "data/processed/proteccion.rds"
)

ind_economia <- readRDS(
  "data/processed/economia.rds"
)

ind_educacion <- readRDS(
  "data/processed/educacion.rds"
)


#Tratamiento

ind_economia <- ind_economia %>%
  mutate(
    LocCodigo = sprintf("%02d", as.integer(COD_LOCALIDAD))
  )

ind_educacion <- ind_educacion %>%
  mutate(
    LocCodigo = sprintf("%02d", as.integer(COD_LOCA))
  )



base_final <- ind_salud %>%
  select(
    LocCodigo,
    indice_salud_100
  ) %>%
  left_join(
    ind_proteccion %>%
      select(
        LocCodigo,
        indice_proteccion_100
      ),
    by = "LocCodigo"
  ) %>%
  left_join(
    ind_economia %>%
      select(
        LocCodigo,
        indice_economico
      ),
    by = "LocCodigo"
  ) %>%
  left_join(
    ind_educacion %>%
      select(
        LocCodigo,
        indice_educacion_100
      ),
    by = "LocCodigo"
  )


datos_vulnerabilidad <- base_final %>%
  select(
    indice_salud_100,
    indice_proteccion_100,
    indice_economico,
    indice_educacion_100
  )


acp_vulnerabilidad <- prcomp(
  datos_vulnerabilidad,
  center = TRUE,
  scale. = TRUE
)


pesos <- acp_vulnerabilidad$rotation[,1]

pesos_normalizados <- pesos / sum(pesos)

round(pesos_normalizados,3)

base_final <- base_final %>%
  mutate(
    indice_vulnerabilidad =
      indice_salud_100 * pesos_normalizados["indice_salud_100"] +
      indice_proteccion_100 * pesos_normalizados["indice_proteccion_100"] +
      indice_economico * pesos_normalizados["indice_economico"] +
      indice_educacion_100 * pesos_normalizados["indice_educacion_100"]
  )



saveRDS(
  base_final,
  "data/processed/vulnerabilidad.rds"
)
