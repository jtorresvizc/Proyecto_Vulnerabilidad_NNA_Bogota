# 08 - PERSPECTIVA Y ALCANCE

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readxl)
})

MIN_ANIOS       <- 4   # años distintos mínimos para hablar de tendencia
MIN_LOCALIDADES <- 18  # de 20

ANIO_INICIO_SERIE <- 2017
ANIO_FIN_SERIE    <- 2026

UMBRAL_DENOMINADOR <- 100

# Cambio entre el primer y el último año disponible
calcular_cambio <- function(serie, anios) {
  primero <- min(anios)
  ultimo  <- max(anios)

  serie %>%
    filter(anio %in% c(primero, ultimo)) %>%
    select(LocCodigo, LocNombre, anio, valor) %>%
    mutate(momento = if_else(anio == primero, "inicio", "fin")) %>%
    select(-anio) %>%
    pivot_wider(names_from = momento, values_from = valor) %>%
    mutate(
      anio_inicio     = primero,
      anio_fin        = ultimo,
      cambio_absoluto = fin - inicio,
      cambio_relativo = if_else(inicio > 0, 100 * (fin - inicio) / inicio, NA_real_),
      direccion = case_when(
        is.na(cambio_absoluto)  ~ "Sin dato",
        cambio_absoluto < -0.5  ~ "A la baja",
        cambio_absoluto >  0.5  ~ "Al alza",
        TRUE                    ~ "Estable"
      )
    )
}

base_final <- readRDS("data/processed/vulnerabilidad.rds")

nombres_localidad <- setNames(base_final$LocNombre, base_final$LocCodigo)

# 1. INVENTARIO DE SERIES TEMPORALES DISPONIBLES

inventariar <- function(ruta, col_anio, col_localidad, etiqueta) {

  if (!file.exists(ruta)) {
    return(data.frame(
      fuente = etiqueta, archivo = basename(ruta), disponible = FALSE,
      n_anios = NA_integer_, anio_min = NA_integer_, anio_max = NA_integer_,
      n_localidades = NA_integer_, continua = FALSE,
      nota = "archivo no encontrado", stringsAsFactors = FALSE
    ))
  }

  datos <- read.csv(ruta, sep = ";", nrows = 200000)

  if (!col_anio %in% names(datos) || !col_localidad %in% names(datos)) {
    return(data.frame(
      fuente = etiqueta, archivo = basename(ruta), disponible = FALSE,
      n_anios = NA_integer_, anio_min = NA_integer_, anio_max = NA_integer_,
      n_localidades = NA_integer_, continua = FALSE,
      nota = paste0("faltan columnas: ",
                    paste(setdiff(c(col_anio, col_localidad), names(datos)),
                          collapse = ", ")),
      stringsAsFactors = FALSE
    ))
  }

  anios <- sort(unique(suppressWarnings(as.integer(datos[[col_anio]]))))
  anios <- anios[!is.na(anios)]
  locs  <- unique(datos[[col_localidad]])
  locs  <- locs[!is.na(locs)]

  # ¿los años forman una secuencia sin huecos?
  continua <- length(anios) >= MIN_ANIOS &&
    identical(anios, seq(min(anios), max(anios)))

  data.frame(
    fuente = etiqueta, archivo = basename(ruta), disponible = TRUE,
    n_anios = length(anios), anio_min = min(anios), anio_max = max(anios),
    n_localidades = length(locs),
    continua = continua,
    nota = paste0("años: ", paste(anios, collapse = ", ")),
    stringsAsFactors = FALSE
  )
}

inventario_series <- bind_rows(
  inventariar("data/raw/osb_malnutricion5agnos.csv", "AÑO", "CODIGO_lOCALIDAD",
              "Malnutrición en menores de 5 años"),
  inventariar("data/raw/osb_malnutricion5_17anos.xlsx.csv", "ANIO", "CODIGO_lOCALIDAD",
              "Malnutrición de 5 a 17 años"),
  inventariar("data/raw/osb_tm_infantil.csv", "ANIO", "CÓDIGO.LOCALIDAD",
              "Mortalidad infantil")
)

print(inventario_series[, c("fuente","disponible","n_anios","anio_min",
                            "anio_max","n_localidades","continua")])

# 2. SERIES CONSTRUIDAS (solo las que pasan el filtro)
series_disponibles <- list()

# Mortalidad infantil por localidad y año
fila_mortalidad <- inventario_series[inventario_series$fuente == "Mortalidad infantil", ]

if (isTRUE(fila_mortalidad$disponible) &&
    fila_mortalidad$n_anios >= MIN_ANIOS) {

  tm <- read.csv("data/raw/osb_tm_infantil.csv", sep = ";")

  serie_mortalidad <- tm %>%
    filter(!is.na(ANIO), !is.na(`CÓDIGO.LOCALIDAD`)) %>%
    mutate(LocCodigo = sprintf("%02d", as.integer(`CÓDIGO.LOCALIDAD`))) %>%
    filter(LocCodigo %in% base_final$LocCodigo) %>%
    group_by(LocCodigo, anio = as.integer(ANIO)) %>%
    summarise(
      casos         = sum(CASOS, na.rm = TRUE),
      nacidos_vivos = sum(NACIDOS.VIVOS, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(nacidos_vivos > 0) %>%
    mutate(
      valor     = casos / nacidos_vivos * 1000,
      LocNombre = unname(nombres_localidad[LocCodigo])
    )

  cobertura <- serie_mortalidad %>%
    count(anio, name = "n_localidades")

  anios_completos <- cobertura$anio[cobertura$n_localidades >= MIN_LOCALIDADES]
  anios_completos <- anios_completos[
    anios_completos >= ANIO_INICIO_SERIE & anios_completos <= ANIO_FIN_SERIE
  ]

  if (length(anios_completos) >= MIN_ANIOS) {
    serie_mortalidad <- serie_mortalidad %>% filter(anio %in% anios_completos)

    cambio_mortalidad <- calcular_cambio(serie_mortalidad, anios_completos)

    # Estabilidad de la tasa según el tamaño del denominador
    estabilidad_mortalidad <- serie_mortalidad %>%
      group_by(LocCodigo, LocNombre) %>%
      summarise(
        nacidos_vivos_mediana = median(nacidos_vivos),
        .groups = "drop"
      ) %>%
      mutate(serie_estable = nacidos_vivos_mediana >= UMBRAL_DENOMINADOR)

    inestables_mortalidad <- estabilidad_mortalidad$LocNombre[
      !estabilidad_mortalidad$serie_estable
    ]

    serie_mortalidad <- serie_mortalidad %>%
      left_join(
        estabilidad_mortalidad %>% select(LocCodigo, serie_estable, nacidos_vivos_mediana),
        by = "LocCodigo"
      )

    cambio_mortalidad <- cambio_mortalidad %>%
      left_join(
        estabilidad_mortalidad %>% select(LocCodigo, serie_estable, nacidos_vivos_mediana),
        by = "LocCodigo"
      )

    nota_estabilidad_mortalidad <- if (length(inestables_mortalidad) == 0) NULL else paste0(
      "La tasa de ",
      paste(inestables_mortalidad, collapse = ", "),
      " se calcula sobre menos de ", UMBRAL_DENOMINADOR,
      " nacidos vivos al año (mediana de ",
      paste(round(estabilidad_mortalidad$nacidos_vivos_mediana[!estabilidad_mortalidad$serie_estable]),
            collapse = ", "),
      "). Con denominadores tan pequeños una sola defunción mueve la tasa ",
      "decenas de puntos, de modo que su serie refleja variación aleatoria y ",
      "no una tendencia. Se excluye del gráfico por defecto para que las ",
      "demás localidades sean legibles; puede incluirse con la casilla."
    )

    message("[08_perspectiva] mortalidad: series inestables por denominador pequeño: ",
            if (length(inestables_mortalidad) == 0) "ninguna"
            else paste(inestables_mortalidad, collapse = ", "))

    series_disponibles$mortalidad_infantil <- list(
      etiqueta = "Tasa de mortalidad infantil (por 1.000 nacidos vivos)",
      fuente_inventario = "Mortalidad infantil",
      fuente   = "Observatorio de Salud de Bogotá (osb_tm_infantil)",
      unidad   = "por 1.000 nacidos vivos",
      anios    = sort(anios_completos),
      serie    = serie_mortalidad,
      cambio   = cambio_mortalidad,
      estabilidad             = estabilidad_mortalidad,
      localidades_inestables  = inestables_mortalidad,
      nota_estabilidad        = nota_estabilidad_mortalidad
    )
  }
}

# Malnutrición en menores de 5 años
fila_mal5 <- inventario_series[
  inventario_series$fuente == "Malnutrición en menores de 5 años", ]

if (isTRUE(fila_mal5$disponible) && fila_mal5$n_anios >= MIN_ANIOS) {

  m5 <- read.csv("data/raw/osb_malnutricion5agnos.csv", sep = ";")

  if ("PROPORCION_RIESGO_DNT_GLOBAL" %in% names(m5)) {

    serie_dnt <- m5 %>%
      filter(!is.na(AÑO), !is.na(CODIGO_lOCALIDAD)) %>%
      mutate(LocCodigo = sprintf("%02d", as.integer(CODIGO_lOCALIDAD))) %>%
      filter(LocCodigo %in% base_final$LocCodigo) %>%
      group_by(LocCodigo, anio = as.integer(AÑO)) %>%
      summarise(
        valor = mean(as.numeric(PROPORCION_RIESGO_DNT_GLOBAL), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(!is.na(valor)) %>%
      mutate(LocNombre = unname(nombres_localidad[LocCodigo]))

    cobertura_dnt <- serie_dnt %>% count(anio, name = "n_localidades")
    anios_dnt <- cobertura_dnt$anio[cobertura_dnt$n_localidades >= MIN_LOCALIDADES]
    anios_dnt <- anios_dnt[
      anios_dnt >= ANIO_INICIO_SERIE & anios_dnt <= ANIO_FIN_SERIE
    ]

    if (length(anios_dnt) >= MIN_ANIOS) {
      serie_dnt <- serie_dnt %>% filter(anio %in% anios_dnt)

      cambio_dnt <- calcular_cambio(serie_dnt, anios_dnt)

      serie_dnt$serie_estable  <- TRUE
      cambio_dnt$serie_estable <- TRUE

      series_disponibles$riesgo_dnt <- list(
        etiqueta = "Proporción con riesgo de desnutrición global (menores de 5 años)",
        fuente_inventario = "Malnutrición en menores de 5 años",
        fuente   = "Observatorio de Salud de Bogotá (osb_malnutricion5agnos)",
        unidad   = "% de menores de 5 años valorados",
        anios    = sort(anios_dnt),
        serie    = serie_dnt,
        cambio   = cambio_dnt,
        estabilidad            = NULL,
        localidades_inestables = character(0),
        nota_estabilidad       = paste0(
          "La fuente publica este indicador ya como proporción y no entrega ",
          "el número de menores valorados, por lo que no es posible evaluar ",
          "la estabilidad del denominador como se hace en mortalidad."
        )
      )
    }
  }
}

# 3. ALCANCE POBLACIONAL (población de referencia observada)

alcance_poblacional <- NULL

archivo_proteccion <- "data/processed/oferta_proteccion.rds"
if (file.exists(archivo_proteccion)) {
  alcance_poblacional <- readRDS(archivo_proteccion) %>%
    select(LocCodigo, NNA_referencia = NNA) %>%
    left_join(
      base_final %>%
        select(LocCodigo, LocNombre, indice_vulnerabilidad, dimension_dominante),
      by = "LocCodigo"
    ) %>%
    mutate(
      participacion_ciudad = 100 * NNA_referencia / sum(NNA_referencia)
    ) %>%
    arrange(desc(indice_vulnerabilidad))
}

ANIO_POBLACION_REFERENCIA <- 2026

# 4. TEXTOS
texto_alcance <- paste0(
  "El alcance poblacional describe CUÁNTOS niños, niñas y adolescentes ",
  "podrían estar involucrados en la respuesta institucional de cada ",
  "territorio. Es una magnitud distinta del índice: una localidad puede ",
  "tener vulnerabilidad relativa alta y pocos NNA, o vulnerabilidad ",
  "moderada y una población muy grande. La cifra corresponde a la ",
  "población de referencia ", ANIO_POBLACION_REFERENCIA,
  " del anexo de población por localidad de la Secretaría Distrital de ",
  "Planeación, que es la misma base usada como denominador de todas las ",
  "tasas por 10.000 NNA del proyecto. Se reporta como población de ",
  "referencia y no como una estimación futura ni como una proyección ",
  "elaborada por el equipo."
)

texto_perspectiva <- if (length(series_disponibles) == 0) {
  paste0(
    "No se incorpora ningún componente de tendencia histórica. La revisión ",
    "programática de los datasets disponibles no encontró ninguna serie con ",
    "al menos ", MIN_ANIOS, " años y cobertura de ", MIN_LOCALIDADES,
    " de las 20 localidades. Documentar esta ausencia es preferible a forzar ",
    "una tendencia sobre una base temporal insuficiente."
  )
} else {
  paste0(
    "Este componente NO predice la vulnerabilidad futura. El índice es ",
    "transversal y no existe una serie temporal del índice compuesto, de modo ",
    "que no se ajusta ningún modelo de pronóstico. Lo que sí permiten los ",
    "datos es mostrar la evolución observada de ",
    length(series_disponibles),
    if (length(series_disponibles) == 1) " indicador" else " indicadores",
    " que sí tienen serie anual suficiente (",
    paste(vapply(series_disponibles, function(s) s$etiqueta, character(1)),
          collapse = "; "),
    "). Se lee como señal direccional descriptiva: hacia dónde se ha movido ",
    "cada territorio en ese indicador concreto, no hacia dónde irá el índice."
  )
}

# GUARDAR
# Marca en el inventario cuáles series se llevaron efectivamente al visor
fuentes_usadas <- vapply(
  series_disponibles,
  function(s) s$fuente_inventario,
  character(1)
)
inventario_series$usada_en_visor <- inventario_series$fuente %in% fuentes_usadas

saveRDS(
  list(
    inventario_series         = inventario_series,
    series                    = series_disponibles,
    alcance_poblacional       = alcance_poblacional,
    anio_poblacion_referencia = ANIO_POBLACION_REFERENCIA,
    texto_perspectiva         = texto_perspectiva,
    texto_alcance             = texto_alcance,
    min_anios                 = MIN_ANIOS,
    min_localidades           = MIN_LOCALIDADES,
    anio_inicio_serie         = ANIO_INICIO_SERIE,
    anio_fin_serie            = ANIO_FIN_SERIE
  ),
  "data/processed/perspectiva.rds"
)

message("[08_perspectiva] series con continuidad suficiente: ",
        length(series_disponibles),
        if (length(series_disponibles) > 0)
          paste0(" (", paste(names(series_disponibles), collapse = ", "), ")") else "")
