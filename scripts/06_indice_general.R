# 06 - ÍNDICE GENERAL DE VULNERABILIDAD INFANTIL

source("scripts/00_funciones.R")

source("scripts/02_salud.R")
source("scripts/03_proteccion.R")
source("scripts/04_economia.R")
source("scripts/05_educacion.R")

ind_salud      <- readRDS("data/processed/salud.rds")
ind_proteccion <- readRDS("data/processed/proteccion.rds")
ind_economia   <- readRDS("data/processed/economia.rds")
ind_educacion  <- readRDS("data/processed/educacion.rds")

#Tratamiento

ind_economia <- ind_economia %>%
  mutate(LocCodigo = sprintf("%02d", as.integer(COD_LOCALIDAD)))

ind_educacion <- ind_educacion %>%
  mutate(LocCodigo = sprintf("%02d", as.integer(COD_LOCA)))

base_final <- ind_salud %>%
  select(LocCodigo, indice_salud_100) %>%
  left_join(
    ind_proteccion %>% select(LocCodigo, indice_proteccion_100),
    by = "LocCodigo"
  ) %>%
  left_join(
    ind_economia %>% select(LocCodigo, indice_economico),
    by = "LocCodigo"
  ) %>%
  left_join(
    ind_educacion %>% select(LocCodigo, indice_educacion_100),
    by = "LocCodigo"
  ) %>%
  mutate(across(where(is.numeric), ~ as.numeric(unname(.x)))) %>%
  arrange(LocCodigo)

dimensiones_indice <- c(
  Salud      = "indice_salud_100",
  Protección = "indice_proteccion_100",
  Economía   = "indice_economico",
  Educación  = "indice_educacion_100"
)

if (any(is.na(base_final[, unname(dimensiones_indice)]))) {
  stop(
    "[06_indice_general] Hay localidades sin índice en alguna dimensión: ",
    paste(base_final$LocCodigo[!complete.cases(base_final)], collapse = ", ")
  )
}

# NOMBRES DE LOCALIDAD
nombres_localidad <- c(
  "01" = "Usaquén",        "02" = "Chapinero",     "03" = "Santa Fe",
  "04" = "San Cristóbal",  "05" = "Usme",          "06" = "Tunjuelito",
  "07" = "Bosa",           "08" = "Kennedy",       "09" = "Fontibón",
  "10" = "Engativá",       "11" = "Suba",          "12" = "Barrios Unidos",
  "13" = "Teusaquillo",    "14" = "Los Mártires",  "15" = "Antonio Nariño",
  "16" = "Puente Aranda",  "17" = "La Candelaria", "18" = "Rafael Uribe Uribe",
  "19" = "Ciudad Bolívar", "20" = "Sumapaz"
)

base_final <- base_final %>%
  mutate(LocNombre = unname(nombres_localidad[LocCodigo])) %>%
  relocate(LocNombre, .after = LocCodigo)

stopifnot(!any(is.na(base_final$LocNombre)), nrow(base_final) == 20)

# ÍNDICE GENERAL

z_dimensiones <- as.data.frame(scale(base_final[, unname(dimensiones_indice)]))
names(z_dimensiones) <- names(dimensiones_indice)

base_final <- base_final %>%
  mutate(
    puntaje_vulnerabilidad = rowMeans(z_dimensiones),
    indice_vulnerabilidad  = normalizar_100(rowMeans(z_dimensiones))
  )

correlaciones_dimension <- vapply(
  unname(dimensiones_indice),
  function(v) cor(base_final$indice_vulnerabilidad, base_final[[v]]),
  numeric(1)
)
names(correlaciones_dimension) <- names(dimensiones_indice)

if (any(correlaciones_dimension <= 0)) {
  warning(
    "[06_indice_general] Dimensiones que no correlacionan positivamente con ",
    "el índice general: ",
    paste(names(correlaciones_dimension)[correlaciones_dimension <= 0], collapse = ", "),
    call. = FALSE
  )
}

# ACP GENERAL COMO DIAGNÓSTICO
acp_vulnerabilidad <- prcomp(
  base_final[, unname(dimensiones_indice)],
  center = TRUE,
  scale. = TRUE
)

cargas_cp1 <- acp_vulnerabilidad$rotation[, 1]

# Se orienta el CP1 para que apunte en la misma dirección que el índice
sentido_cp1_general <- ifelse(
  cor(acp_vulnerabilidad$x[, 1], base_final$puntaje_vulnerabilidad) < 0, -1, 1
)
cargas_cp1 <- cargas_cp1 * sentido_cp1_general

varianza_cp1_general <- summary(acp_vulnerabilidad)$importance[2, 1]
cp1_signo_unico <- all(cargas_cp1 > 0) || all(cargas_cp1 < 0)

etiquetas_dimension <- setNames(names(dimensiones_indice), unname(dimensiones_indice))

cargas_acp_general <- data.frame(
  dimension = unname(etiquetas_dimension[names(cargas_cp1)]),
  variable  = names(cargas_cp1),
  carga_cp1 = as.numeric(cargas_cp1),
  peso_en_indice = 0.25,
  correlacion_con_indice = as.numeric(
    correlaciones_dimension[unname(etiquetas_dimension[names(cargas_cp1)])]
  ),
  row.names = NULL,
  stringsAsFactors = FALSE
) %>%
  arrange(desc(abs(carga_cp1)))

matriz_correlacion_dimensiones <- cor(base_final[, unname(dimensiones_indice)])
dimnames(matriz_correlacion_dimensiones) <- list(
  names(dimensiones_indice), names(dimensiones_indice)
)

dimension_mayor_carga <- cargas_acp_general$dimension[1]

texto_dimension_mayor_carga <- paste0(
  "El índice general pondera las cuatro dimensiones por igual (25% cada una) ",
  "sobre sus puntajes estandarizados. El ACP se conserva como diagnóstico de ",
  "la estructura de correlación: su primer componente resume ",
  round(varianza_cp1_general * 100, 1), "% de la varianza conjunta y la ",
  "dimensión que más pesa en él es ", dimension_mayor_carga,
  " (carga ", sprintf("%+.2f", cargas_acp_general$carga_cp1[1]), "). ",
  if (cp1_signo_unico) {
    paste0(
      "Las cuatro dimensiones cargan en la misma dirección, lo que indica que ",
      "las localidades más vulnerables tienden a serlo de forma simultánea en ",
      "varias dimensiones."
    )
  } else {
    paste0(
      "Las cargas tienen signos mixtos: el primer componente separa las ",
      "localidades por el TIPO de vulnerabilidad que concentran más que por su ",
      "nivel general. Por esa razón el índice no se pondera con el ACP sino con ",
      "pesos iguales, y el perfil dimensional de cada territorio se reporta por ",
      "separado."
    )
  }
)

# PERFIL DIMENSIONAL: ¿qué dimensión caracteriza a cada territorio?

base_final$dimension_dominante <-
  names(z_dimensiones)[max.col(z_dimensiones, ties.method = "first")]

base_final$z_dimension_dominante <- apply(z_dimensiones, 1, max)

perfil_dimensional_z <- cbind(
  base_final[, c("LocCodigo", "LocNombre")],
  z_dimensiones
)

message("[06_indice_general] índice general = promedio de 4 puntajes z (25% c/u)")
message("[06_indice_general] varianza CP1 (diagnóstico) = ",
        round(varianza_cp1_general * 100, 1), "% | CP1 de signo único: ", cp1_signo_unico)
print(round(correlaciones_dimension, 3))

saveRDS(base_final, "data/processed/vulnerabilidad.rds")

saveRDS(
  list(
    cargas_acp_general             = cargas_acp_general,
    varianza_cp1_general           = varianza_cp1_general,
    cp1_signo_unico                = cp1_signo_unico,
    matriz_correlacion_dimensiones = matriz_correlacion_dimensiones,
    correlaciones_dimension        = correlaciones_dimension,
    dimension_mayor_carga          = dimension_mayor_carga,
    texto_dimension_mayor_carga    = texto_dimension_mayor_carga,
    perfil_dimensional_z           = perfil_dimensional_z,
    dimensiones_indice             = dimensiones_indice,
    detalle_dimensiones            = list(
      Salud      = readRDS("data/processed/dimension_salud.rds")[c("cargas","varianza_cp1","variables","protectoras")],
      Protección = readRDS("data/processed/dimension_proteccion.rds")[c("cargas","varianza_cp1","variables","protectoras")],
      Economía   = readRDS("data/processed/dimension_economia.rds")[c("cargas","varianza_cp1","variables","protectoras")],
      Educación  = readRDS("data/processed/dimension_educacion.rds")[c("cargas","varianza_cp1","variables","protectoras")]
    )
  ),
  "data/processed/acp_general.rds"
)
