# 00 - FUNCIONES COMPARTIDAS DE CONSTRUCCIÓN DE ÍNDICES

# Matriz de puntajes z con el signo armonizado
armonizar <- function(datos, variables, protectoras = character(0)) {

  faltantes <- setdiff(c(variables, protectoras), names(datos))
  if (length(faltantes) > 0) {
    stop("armonizar(): columnas ausentes: ", paste(faltantes, collapse = ", "))
  }

  matriz <- scale(as.data.frame(datos[, variables, drop = FALSE]))

  sin_varianza <- variables[
    vapply(as.data.frame(matriz), function(x) all(is.na(x)), logical(1))
  ]
  if (length(sin_varianza) > 0) {
    stop(
      "armonizar(): variables sin varianza, no pueden estandarizarse: ",
      paste(sin_varianza, collapse = ", ")
    )
  }

  protectoras <- intersect(protectoras, variables)
  if (length(protectoras) > 0) {
    matriz[, protectoras] <- -matriz[, protectoras]
  }

  attr(matriz, "protectoras") <- protectoras
  matriz
}

# Normalización 0-100 sobre el rango observado
normalizar_100 <- function(x) {
  rango <- range(x, na.rm = TRUE)
  if (diff(rango) == 0) {
    stop("normalizar_100(): el indicador no tiene variación entre localidades.")
  }
  100 * (x - rango[1]) / diff(rango)
}

# Construcción completa de una dimensión
construir_dimension <- function(datos, variables, protectoras = character(0),
                                etiqueta = "dimensión") {

  matriz <- armonizar(datos, variables, protectoras)

  # Índice: promedio de puntajes z armonizados
  indice <- rowMeans(matriz)

  # ACP conservado como diagnóstico
  acp <- prcomp(matriz, center = TRUE, scale. = TRUE)
  varianza_cp1 <- summary(acp)$importance[2, 1]
  cor_cp1_indice <- cor(acp$x[, 1], indice)

  cargas <- data.frame(
    variable   = rownames(acp$rotation),
    protectora = rownames(acp$rotation) %in% protectoras,
    carga_cp1  = as.numeric(acp$rotation[, 1]) * ifelse(cor_cp1_indice < 0, -1, 1),
    correlacion_con_indice = vapply(
      rownames(acp$rotation),
      function(v) cor(matriz[, v], indice),
      numeric(1)
    ),
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  discordantes <- cargas$variable[cargas$correlacion_con_indice < 0]

  message(sprintf(
    "[%s] índice = media de %d z armonizados (%d protectoras invertidas) | var(CP1) = %.1f%% | r(CP1, índice) = %+.3f%s",
    etiqueta, length(variables), length(intersect(protectoras, variables)),
    varianza_cp1 * 100, cor_cp1_indice,
    if (length(discordantes) > 0)
      paste0(" | variables discordantes: ", paste(discordantes, collapse = ", ")) else ""
  ))

  list(
    indice        = indice,
    indice_100    = normalizar_100(indice),
    z             = matriz,
    acp           = acp,
    cargas        = cargas,
    varianza_cp1  = varianza_cp1,
    cor_cp1_indice = cor_cp1_indice,
    discordantes  = discordantes,
    variables     = variables,
    protectoras   = intersect(protectoras, variables),
    etiqueta      = etiqueta
  )
}
