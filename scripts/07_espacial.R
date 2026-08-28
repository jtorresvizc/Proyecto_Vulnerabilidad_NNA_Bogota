# 07 - ANÁLISIS ESPACIAL: MORAN GLOBAL Y LISA

suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
  library(spdep)
})

base_final <- readRDS("data/processed/vulnerabilidad.rds")
loca <- st_read("data/shapefiles/locashp/Loca.shp", quiet = TRUE)

localidades <- loca %>%
  mutate(LocCodigo = sprintf("%02d", as.integer(LocCodigo))) %>%
  select(LocCodigo, geometry) %>%
  inner_join(base_final, by = "LocCodigo") %>%
  arrange(LocCodigo)

if (nrow(localidades) != nrow(base_final)) {
  stop(
    "[07_espacial] El shapefile y la base de índices no coinciden: ",
    nrow(localidades), " polígonos unidos frente a ", nrow(base_final),
    " localidades con índice."
  )
}

# MATRIZ DE PESOS: CONTIGÜIDAD TIPO REINA

vecinos <- poly2nb(localidades, queen = TRUE)

n_sin_vecinos <- sum(card(vecinos) == 0)
localidades_sin_vecinos <- localidades$LocNombre[card(vecinos) == 0]

pesos <- nb2listw(vecinos, style = "W", zero.policy = TRUE)

resumen_vecindad <- data.frame(
  LocCodigo = localidades$LocCodigo,
  LocNombre = localidades$LocNombre,
  n_vecinos = card(vecinos),
  stringsAsFactors = FALSE
)

message(
  "[07_espacial] contigüidad reina: ",
  round(mean(card(vecinos)), 2), " vecinas en promedio (mínimo ",
  min(card(vecinos)), ", máximo ", max(card(vecinos)), ")",
  if (n_sin_vecinos > 0)
    paste0(" | sin vecinas: ", paste(localidades_sin_vecinos, collapse = ", ")) else ""
)

# MORAN GLOBAL
x <- localidades$indice_vulnerabilidad

moran_global <- moran.test(
  x,
  listw = pesos,
  zero.policy = TRUE,
  randomisation = TRUE
)

moran_i        <- unname(moran_global$estimate["Moran I statistic"])
moran_esperado <- unname(moran_global$estimate["Expectation"])
moran_varianza <- unname(moran_global$estimate["Variance"])
moran_z        <- unname(moran_global$statistic)
moran_p        <- unname(moran_global$p.value)

# Interpretación derivada del resultado, no escrita de antemano
significativo_global <- moran_p < 0.05

interpretacion_moran_global <- if (!significativo_global) {
  paste0(
    "El índice de Moran global es I = ", sprintf("%.3f", moran_i),
    ", frente a un valor esperado de ", sprintf("%.3f", moran_esperado),
    " bajo la hipótesis de distribución espacial aleatoria ",
    "(z = ", sprintf("%.3f", moran_z), "; p = ", sprintf("%.3f", moran_p), "). ",
    "Con un umbral de 0,05 no hay evidencia suficiente para rechazar la ",
    "hipótesis nula de aleatoriedad espacial: los datos disponibles no ",
    "permiten afirmar que la vulnerabilidad infantil se agrupe ni que se ",
    "disperse sistemáticamente entre localidades vecinas. La hipótesis de ",
    "dependencia espacial no queda respaldada por este resultado."
  )
} else if (moran_i > moran_esperado) {
  paste0(
    "El índice de Moran global es I = ", sprintf("%.3f", moran_i),
    ", por encima del valor esperado bajo aleatoriedad (",
    sprintf("%.3f", moran_esperado), "), con z = ", sprintf("%.3f", moran_z),
    " y p = ", sprintf("%.4f", moran_p), ". ",
    "Existe evidencia de autocorrelación espacial positiva: las localidades ",
    "con niveles similares de vulnerabilidad infantil tienden a ser vecinas ",
    "entre sí, formando agrupaciones territoriales antes que un mosaico ",
    "aleatorio."
  )
} else {
  paste0(
    "El índice de Moran global es I = ", sprintf("%.3f", moran_i),
    ", por debajo del valor esperado bajo aleatoriedad (",
    sprintf("%.3f", moran_esperado), "), con z = ", sprintf("%.3f", moran_z),
    " y p = ", sprintf("%.4f", moran_p), ". ",
    "Existe evidencia de autocorrelación espacial negativa: las localidades ",
    "tienden a ser vecinas de otras con niveles contrastantes de ",
    "vulnerabilidad, un patrón de alternancia o dispersión espacial más que ",
    "de agrupación."
  )
}

message("[07_espacial] Moran I = ", round(moran_i, 4),
        " | E[I] = ", round(moran_esperado, 4),
        " | p = ", signif(moran_p, 4))

# LOCAL MORAN / LISA

UMBRAL_LISA <- 0.05

lisa <- localmoran(
  x,
  listw = pesos,
  zero.policy = TRUE
)

z_indice <- as.numeric(scale(x))
rezago_z <- lag.listw(pesos, z_indice, zero.policy = TRUE)

p_local <- lisa[, "Pr(z != E(Ii))"]

clasificacion_lisa <- dplyr::case_when(
  p_local >= UMBRAL_LISA          ~ "No significativo",
  z_indice > 0 & rezago_z > 0     ~ "Alto-Alto",
  z_indice < 0 & rezago_z < 0     ~ "Bajo-Bajo",
  z_indice > 0 & rezago_z < 0     ~ "Alto-Bajo",
  z_indice < 0 & rezago_z > 0     ~ "Bajo-Alto",
  TRUE                            ~ "No significativo"
)

niveles_cluster <- c("Alto-Alto", "Bajo-Bajo", "Alto-Bajo", "Bajo-Alto", "No significativo")

tabla_lisa <- data.frame(
  LocCodigo             = localidades$LocCodigo,
  LocNombre             = localidades$LocNombre,
  indice_vulnerabilidad = x,
  z_indice              = z_indice,
  rezago_z              = rezago_z,
  I_local               = as.numeric(lisa[, "Ii"]),
  z_local               = as.numeric(lisa[, "Z.Ii"]),
  p_local               = as.numeric(p_local),
  n_vecinos             = card(vecinos),
  cluster               = factor(clasificacion_lisa, levels = niveles_cluster),
  stringsAsFactors      = FALSE
)

conteo_clusters <- table(tabla_lisa$cluster)
n_significativas <- sum(tabla_lisa$p_local < UMBRAL_LISA)

message("[07_espacial] LISA: ", n_significativas, " de ", nrow(tabla_lisa),
        " localidades significativas a p < ", UMBRAL_LISA)
print(conteo_clusters)

# INTERPRETACIÓN DEL PATRÓN ESPACIAL
nombrar <- function(tipo) {
  nombres <- tabla_lisa$LocNombre[tabla_lisa$cluster == tipo]
  if (length(nombres) == 0) return(NULL)
  paste0(
    tipo, " (", length(nombres), "): ",
    paste(sort(nombres), collapse = ", ")
  )
}

detalle_clusters <- Filter(
  Negate(is.null),
  lapply(setdiff(niveles_cluster, "No significativo"), nombrar)
)

# Glosario: solo se explican las categorías efectivamente encontradas
glosario_cluster <- c(
  "Alto-Alto" = paste(
    "Alto-Alto: vulnerabilidad alta rodeada de vecinas también vulnerables;",
    "son el núcleo territorial de la priorización."
  ),
  "Bajo-Bajo" = paste(
    "Bajo-Bajo: vulnerabilidad baja rodeada de vecinas también poco",
    "vulnerables; zonas de sostenimiento."
  ),
  "Alto-Bajo" = paste(
    "Alto-Bajo: foco de vulnerabilidad alta dentro de un entorno menos",
    "vulnerable; requiere intervención focalizada y no de escala zonal."
  ),
  "Bajo-Alto" = paste(
    "Bajo-Alto: territorio comparativamente mejor situado dentro de un",
    "entorno vulnerable; expuesto a la presión de su entorno."
  )
)

tipos_presentes <- intersect(
  names(glosario_cluster),
  as.character(unique(tabla_lisa$cluster[tabla_lisa$cluster != "No significativo"]))
)

interpretacion_lisa <- if (n_significativas == 0) {
  paste0(
    "Ninguna de las 20 localidades presenta autocorrelación local ",
    "significativa a p < ", UMBRAL_LISA, ". El análisis LISA no identifica ",
    "conglomerados espaciales de vulnerabilidad infantil: en la escala de ",
    "localidad, cada territorio se comporta de forma independiente respecto ",
    "de sus vecinos."
  )
} else {
  paste0(
    "El análisis LISA identifica ", n_significativas,
    if (n_significativas == 1) " localidad con" else " localidades con",
    " autocorrelación local significativa a p < ", UMBRAL_LISA, ". ",
    paste(detalle_clusters, collapse = ". "), ". ",
    paste(glosario_cluster[tipos_presentes], collapse = " "),
    if (!"Alto-Alto" %in% tipos_presentes) {
      paste0(
        " No se identifica ningún conglomerado Alto-Alto: no hay evidencia de ",
        "un núcleo contiguo de localidades que concentren simultáneamente alta ",
        "vulnerabilidad y vecindad vulnerable."
      )
    } else ""
  )
}

lectura_conjunta <- if (significativo_global && n_significativas <= 1) {
  paste0(
    "Los dos niveles de análisis apuntan en direcciones distintas y conviene ",
    "leerlos juntos. A escala de ciudad hay evidencia de agrupación: las ",
    "localidades vecinas se parecen más entre sí de lo que cabría esperar por ",
    "azar. Pero esa señal global no se concreta en conglomerados locales ",
    "estadísticamente significativos. La explicación más plausible es de ",
    "potencia estadística: con 20 unidades territoriales y un promedio de ",
    sprintf("%.1f", mean(card(vecinos))), " vecinas por localidad, el ",
    "estadístico local se apoya en muy pocas observaciones y rara vez alcanza ",
    "significancia. La conclusión defendible es que la vulnerabilidad infantil ",
    "NO se distribuye de forma aleatoria en Bogotá, pero los datos disponibles ",
    "no permiten delimitar con confianza estadística los bordes de cada ",
    "conglomerado. La priorización territorial debe apoyarse principalmente en ",
    "el nivel del índice y en el perfil dimensional, usando el patrón espacial ",
    "como criterio complementario."
  )
} else if (!significativo_global && n_significativas > 0) {
  paste0(
    "A escala de ciudad no hay evidencia de autocorrelación espacial, pero el ",
    "análisis local sí identifica territorios atípicos respecto de su entorno. ",
    "Esto indica heterogeneidad local sin un patrón global de agrupación."
  )
} else ""

respuesta_pregunta_espacial <- trimws(paste(
  interpretacion_moran_global,
  interpretacion_lisa,
  lectura_conjunta
))

# PRIORIZACIÓN TERRITORIAL

terciles <- quantile(x, probs = c(1/3, 2/3), na.rm = TRUE)

prioridades_localidad_precalculadas <- tabla_lisa %>%
  left_join(
    base_final %>% select(LocCodigo, dimension_dominante, z_dimension_dominante),
    by = "LocCodigo"
  ) %>%
  mutate(
    nivel_vulnerabilidad = case_when(
      indice_vulnerabilidad >= terciles[2] ~ "Alta",
      indice_vulnerabilidad >= terciles[1] ~ "Media",
      TRUE                                 ~ "Baja"
    ),
    prioridad = case_when(
      nivel_vulnerabilidad == "Alta"                            ~ "Intensificación prioritaria",
      nivel_vulnerabilidad == "Media" & cluster == "Alto-Alto"  ~ "Intensificación prioritaria",
      nivel_vulnerabilidad == "Media"                           ~ "Prevención reforzada",
      cluster == "Bajo-Alto"                                    ~ "Prevención reforzada",
      TRUE                                                      ~ "Sostenimiento y monitoreo"
    ),
    prioridad = factor(
      prioridad,
      levels = c("Intensificación prioritaria", "Prevención reforzada",
                 "Sostenimiento y monitoreo")
    ),
    justificacion = paste0(
      "Vulnerabilidad ", tolower(nivel_vulnerabilidad),
      " (índice ", sprintf("%.1f", indice_vulnerabilidad), "). ",
      "Dimensión de mayor vulnerabilidad relativa: ", dimension_dominante, ". ",
      if_else(
        cluster == "No significativo",
        "Sin patrón espacial local significativo.",
        paste0("Patrón espacial local: ", cluster, " (p = ",
               sprintf("%.3f", p_local), ").")
      )
    )
  ) %>%
  arrange(desc(indice_vulnerabilidad))

message("[07_espacial] priorización territorial:")
print(table(prioridades_localidad_precalculadas$prioridad))

# GUARDAR
saveRDS(
  list(
    tabla_lisa                  = tabla_lisa,
    prioridades                 = prioridades_localidad_precalculadas,
    resumen_vecindad            = resumen_vecindad,
    conteo_clusters             = conteo_clusters,
    n_significativas            = n_significativas,
    umbral_lisa                 = UMBRAL_LISA,
    niveles_cluster             = niveles_cluster,
    moran = list(
      I           = moran_i,
      esperanza   = moran_esperado,
      varianza    = moran_varianza,
      z           = moran_z,
      p_valor     = moran_p,
      significativo = significativo_global,
      metodo      = moran_global$method,
      alternativa = moran_global$alternative
    ),
    interpretacion_moran_global = interpretacion_moran_global,
    interpretacion_lisa         = interpretacion_lisa,
    lectura_conjunta            = lectura_conjunta,
    respuesta_pregunta_espacial = respuesta_pregunta_espacial,
    promedio_vecinas            = mean(card(vecinos)),
    n_sin_vecinos               = n_sin_vecinos,
    localidades_sin_vecinos     = localidades_sin_vecinos
  ),
  "data/processed/analisis_espacial.rds"
)

message("[07_espacial] resultados guardados en data/processed/analisis_espacial.rds")
