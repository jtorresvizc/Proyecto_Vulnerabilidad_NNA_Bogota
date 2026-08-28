# EJECUTAR TODO EL PROCESAMIENTO

if (!dir.exists("data/processed")) {
  stop(
    "No se encuentra data/processed/. Ejecute este script desde la raíz del ",
    "proyecto (la carpeta que contiene data/ y scripts/). Directorio actual: ",
    getwd()
  )
}

inicio <- Sys.time()

pasos <- c(
  "scripts/06_indice_general.R",  # a su vez ejecuta 00, 01, 02, 03, 04 y 05
  "scripts/07_espacial.R",
  "scripts/08_perspectiva.R"
)

for (paso in pasos) {
  message("\n===============================================")
  message("EJECUTANDO: ", paso)
  message("===============================================")
  source(paso, echo = FALSE)
}

message("\n===============================================")
message("PIPELINE COMPLETO en ",
        round(as.numeric(difftime(Sys.time(), inicio, units = "mins")), 1),
        " minutos")
message("===============================================")

# VERIFICACIÓN FINAL
salidas_esperadas <- c(
  "data/processed/vulnerabilidad.rds",
  "data/processed/acp_general.rds",
  "data/processed/analisis_espacial.rds",
  "data/processed/perspectiva.rds",
  "data/processed/oferta_proteccion.rds",
  "data/processed/oferta_educacion.rds"
)

faltantes <- salidas_esperadas[!file.exists(salidas_esperadas)]
if (length(faltantes) > 0) {
  stop("No se generaron: ", paste(faltantes, collapse = ", "))
}

base_final <- readRDS("data/processed/vulnerabilidad.rds")
espacial   <- readRDS("data/processed/analisis_espacial.rds")

stopifnot(
  nrow(base_final) == 20,
  !any(is.na(base_final$indice_vulnerabilidad)),
  !any(is.na(base_final$LocNombre)),
  nrow(espacial$tabla_lisa) == 20
)

message("\nVerificación final correcta:")
message("  - 20 localidades con índice general completo")
message("  - Moran I = ", round(espacial$moran$I, 4),
        " (p = ", signif(espacial$moran$p_valor, 4), ")")
message("  - LISA significativas: ", espacial$n_significativas, " de 20")
message("\nPara iniciar el visor:  shiny::runApp('scripts/app.R')")
