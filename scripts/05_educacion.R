#Datos

source("scripts/01_carga_datos.R")

#Tratamiento

localidades <- data.frame(
  LocCodigo = 1:20
)

# POBLACION NNA 2026

poblacion2026 <- poblacion %>% 
  filter(
    AREA == "Total",
    AÑO == 2026
  ) %>%
  mutate(
    NNA = rowSums(
      select(
        .,
        Hombres_0:Hombres_17,
        Mujeres_0:Mujeres_17
      ),
      na.rm = TRUE
    )
  ) %>%
  select(
    COD_LOC,
    NNA
  )


# COLEGIOS

colegios_localidad <- colegios %>%
  st_drop_geometry() %>%
  group_by(COD_LOCA) %>%
  summarise(
    colegios = n(),
    .groups="drop"
  )


tasa_colegios <- colegios_localidad %>%
  mutate(
    COD_LOCA = as.numeric(COD_LOCA)
  ) %>%
  left_join(
    poblacion2026 %>%
      mutate(
        COD_LOC = as.numeric(COD_LOC)
      ),
    by=c("COD_LOCA"="COD_LOC")
  ) %>%
  mutate(
    colegios_10k = colegios/NNA*10000
  )


# BENEFICIARIOS

beneficiarios_localidad <- beneficiarios %>%
  st_drop_geometry() %>%
  mutate(
    COD_LOCA = as.numeric(COD_LOCA)
  ) %>%
  group_by(COD_LOCA) %>%
  summarise(
    beneficiarios =
      sum(B_Alimenta,na.rm=TRUE)+
      sum(B_Movilida,na.rm=TRUE),
    .groups="drop"
  )


tasa_beneficiarios <- beneficiarios_localidad %>%
  left_join(
    poblacion2026 %>%
      mutate(
        COD_LOC=as.numeric(COD_LOC)
      ),
    by=c("COD_LOCA"="COD_LOC")
  ) %>%
  mutate(
    beneficiarios_10k =
      beneficiarios/NNA*10000
  )


# TASAS OFICIALES DE APROBACIÓN, DESERCIÓN Y REPROBACIÓN

tasa_total_oficial <- function(capa, nombre_indicador) {
  capa %>%
    st_drop_geometry() %>%
    mutate(
      !!nombre_indicador := rowMeans(cbind(Thombre, Tmujer), na.rm = TRUE),
      COD_LOCA = as.numeric(COD_LOCA)
    ) %>%
    select(COD_LOCA, all_of(nombre_indicador))
}

aprobacion_localidad  <- tasa_total_oficial(tasaaceptacion, "aprobacion")
desercion_localidad   <- tasa_total_oficial(tasadesercion,  "desercion")
reprobacion_localidad <- tasa_total_oficial(tasareprobacion, "reprobacion")

# Las tres tasas son exhaustivas y deben sumar 100 en cada localidad
verificacion_suma <- aprobacion_localidad %>%
  left_join(desercion_localidad,   by = "COD_LOCA") %>%
  left_join(reprobacion_localidad, by = "COD_LOCA") %>%
  mutate(suma = aprobacion + desercion + reprobacion)

stopifnot(all(abs(verificacion_suma$suma - 100) < 0.2))

message(
  "[05_educacion] tasas oficiales verificadas: aprobación + deserción + ",
  "reprobación = 100 (desviación máxima ",
  round(max(abs(verificacion_suma$suma - 100)), 3), ")."
)


# BASE FINAL EDUCACION

educacion <- localidades %>%
  st_drop_geometry() %>%
  select(
    LocCodigo
  ) %>%
  mutate(
    COD_LOCA = as.numeric(LocCodigo)
  ) %>%
  select(-LocCodigo) %>%
  
  left_join(
    tasa_colegios,
    by="COD_LOCA"
  ) %>%
  
  left_join(
    tasa_beneficiarios,
    by="COD_LOCA"
  ) %>%
  
  left_join(
    aprobacion_localidad,
    by="COD_LOCA"
  ) %>%
  
  left_join(
    desercion_localidad,
    by="COD_LOCA"
  ) %>%
  
  left_join(
    reprobacion_localidad,
    by="COD_LOCA"
  )


# ÍNDICE DE LA DIMENSIÓN EDUCACIÓN

source("scripts/00_funciones.R")

variables_educacion <- c("desercion", "reprobacion")

educacion <- educacion %>%
  filter(complete.cases(aprobacion, desercion, reprobacion))

if (nrow(educacion) < 20) {
  warning(
    "[05_educacion] solo ", nrow(educacion),
    " de 20 localidades tienen resultados escolares completos.",
    call. = FALSE
  )
}

dimension_educacion <- construir_dimension(
  datos       = educacion,
  variables   = variables_educacion,
  protectoras = character(0),
  etiqueta    = "05_educacion"
)

educacion <- educacion %>%
  mutate(
    indice_educacion     = dimension_educacion$indice,
    indice_educacion_100 = dimension_educacion$indice_100
  )

stopifnot(cor(educacion$indice_educacion, educacion$desercion) > 0)
stopifnot(cor(educacion$indice_educacion, educacion$reprobacion) > 0)
stopifnot(cor(educacion$indice_educacion, educacion$aprobacion) < 0)

# CAPA DE OFERTA INSTITUCIONAL (fuera del índice)
oferta_educacion <- educacion %>%
  transmute(
    LocCodigo = sprintf("%02d", as.integer(COD_LOCA)),
    NNA = NNA.x,
    colegios,
    colegios_10k,
    beneficiarios,
    beneficiarios_10k
  )

saveRDS(dimension_educacion, "data/processed/dimension_educacion.rds")
saveRDS(oferta_educacion, "data/processed/oferta_educacion.rds")

#Guardar

saveRDS(
  educacion,
  "data/processed/educacion.rds"
)
