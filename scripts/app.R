# VISOR DE VULNERABILIDAD INFANTIL (NNA) - BOGOTÁ
# DataJam Edición 3 - 2026



library(shiny)
library(bs4Dash)
library(leaflet)
library(echarts4r)
library(dplyr)
library(tidyr)
library(sf)
library(purrr)
library(DT)


ruta_proyecto <- if (dir.exists("data/processed")) "." else ".."
ruta <- function(...) file.path(ruta_proyecto, ...)

archivos_requeridos <- c(
  "data/processed/vulnerabilidad.rds",
  "data/processed/acp_general.rds",
  "data/processed/analisis_espacial.rds",
  "data/processed/perspectiva.rds",
  "data/shapefiles/locashp/Loca.shp"
)
faltantes <- archivos_requeridos[!file.exists(ruta(archivos_requeridos))]
if (length(faltantes) > 0) {
  stop(
    "Faltan insumos generados por el procesamiento:\n  ",
    paste(faltantes, collapse = "\n  "),
    "\nEjecute scripts/06_indice_general.R, 07_espacial.R y 08_perspectiva.R ",
    "antes de iniciar la aplicación."
  )
}


base_final  <- readRDS(ruta("data/processed/vulnerabilidad.rds"))
acp_general <- readRDS(ruta("data/processed/acp_general.rds"))
espacial    <- readRDS(ruta("data/processed/analisis_espacial.rds"))
perspectiva <- readRDS(ruta("data/processed/perspectiva.rds"))

oferta_proteccion <- if (file.exists(ruta("data/processed/oferta_proteccion.rds")))
  readRDS(ruta("data/processed/oferta_proteccion.rds")) else NULL
oferta_educacion <- if (file.exists(ruta("data/processed/oferta_educacion.rds")))
  readRDS(ruta("data/processed/oferta_educacion.rds")) else NULL

loca <- st_read(ruta("data/shapefiles/locashp/Loca.shp"), quiet = TRUE) %>%
  st_transform(4326)


formato_miles <- function(x) {
  formatC(round(x), format = "d", big.mark = ".", decimal.mark = ",")
}


# OBJETO ESPACIAL PRINCIPAL

datos <- loca %>%
  mutate(LocCodigo = sprintf("%02d", as.integer(LocCodigo))) %>%
  select(LocCodigo, geometry) %>%
  inner_join(base_final, by = "LocCodigo") %>%
  left_join(
    espacial$tabla_lisa %>%
      select(LocCodigo, I_local, p_local, cluster, z_indice, rezago_z, n_vecinos),
    by = "LocCodigo"
  ) %>%
  left_join(
    espacial$prioridades %>%
      select(LocCodigo, nivel_vulnerabilidad, prioridad, justificacion),
    by = "LocCodigo"
  )

stopifnot(nrow(datos) == nrow(base_final))


# ETIQUETAS

titulos_indicadores <- c(
  indice_vulnerabilidad = "Índice de vulnerabilidad",
  indice_salud_100      = "Índice de salud",
  indice_proteccion_100 = "Índice de protección",
  indice_economico      = "Índice económico",
  indice_educacion_100  = "Índice de educación"
)
variables_indicadores <- names(titulos_indicadores)

PALETA_INDICE <- c("#FFFFBF", "#FEE08B", "#FDAE61", "#F46D43", "#D73027", "#B2182B")

COLORES_CLUSTER <- c(
  "Alto-Alto"        = "#B2182B",
  "Bajo-Bajo"        = "#2166AC",
  "Alto-Bajo"        = "#EF8A62",
  "Bajo-Alto"        = "#67A9CF",
  "No significativo" = "#D9D9D9"
)

COLORES_PRIORIDAD <- c(
  "Intensificación prioritaria" = "#B2182B",
  "Prevención reforzada"        = "#F4A582",
  "Sostenimiento y monitoreo"   = "#92C5DE"
)


# PRECÁLCULOS DE VISUALIZACIÓN
sin_geo <- st_drop_geometry(datos)

perfil_bogota <- sin_geo %>%
  summarise(
    Salud          = mean(indice_salud_100, na.rm = TRUE),
    Protección     = mean(indice_proteccion_100, na.rm = TRUE),
    Economía       = mean(indice_economico, na.rm = TRUE),
    Educación      = mean(indice_educacion_100, na.rm = TRUE),
    Vulnerabilidad = mean(indice_vulnerabilidad, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "indicador", values_to = "valor")

rankings_precalculados <- setNames(
  lapply(variables_indicadores, function(variable) {
    ranking <- sin_geo %>%
      select(LocNombre, valor = all_of(variable)) %>%
      arrange(desc(valor))
    ranking$LocNombre <- factor(ranking$LocNombre, levels = rev(ranking$LocNombre))
    ranking
  }),
  variables_indicadores
)

pares_indicadores <- expand.grid(
  variable_x = variables_indicadores,
  variable_y = variables_indicadores,
  stringsAsFactors = FALSE
) %>%
  filter(variable_x != variable_y)

comparaciones_precalculadas <- setNames(
  map2(
    pares_indicadores$variable_x,
    pares_indicadores$variable_y,
    function(variable_x, variable_y) {
      comparacion <- sin_geo %>%
        transmute(
          LocNombre,
          valor_x = .data[[variable_x]],
          valor_y = .data[[variable_y]]
        ) %>%
        filter(complete.cases(.))

      modelo <- lm(valor_y ~ valor_x, data = comparacion)

      comparacion %>%
        mutate(
          ajuste = as.numeric(predict(modelo)),
          tooltip = paste0(
            LocNombre, "<br>",
            titulos_indicadores[variable_x], ": ", round(valor_x, 1), "<br>",
            titulos_indicadores[variable_y], ": ", round(valor_y, 1)
          )
        )
    }
  ),
  paste(pares_indicadores$variable_x, pares_indicadores$variable_y, sep = "__")
)

correlaciones_pares <- setNames(
  map2_dbl(
    pares_indicadores$variable_x, pares_indicadores$variable_y,
    ~ cor(sin_geo[[.x]], sin_geo[[.y]], use = "complete.obs")
  ),
  paste(pares_indicadores$variable_x, pares_indicadores$variable_y, sep = "__")
)

orden_localidades_panorama <- sin_geo %>%
  arrange(indice_vulnerabilidad) %>%
  pull(LocNombre)

matriz_perfiles_precalculada <- sin_geo %>%
  select(
    LocNombre,
    Economía       = indice_economico,
    Salud          = indice_salud_100,
    Protección     = indice_proteccion_100,
    Educación      = indice_educacion_100,
    Vulnerabilidad = indice_vulnerabilidad
  ) %>%
  pivot_longer(-LocNombre, names_to = "indicador", values_to = "valor") %>%
  mutate(
    LocNombre = factor(LocNombre, levels = orden_localidades_panorama),
    indicador = factor(indicador,
                       levels = c("Economía", "Salud", "Protección",
                                  "Educación", "Vulnerabilidad"))
  )

promedio_indice_general <- mean(datos$indice_vulnerabilidad, na.rm = TRUE)

brechas_promedio_precalculadas <- sin_geo %>%
  transmute(
    LocNombre,
    indice_vulnerabilidad,
    prioridad = as.character(prioridad),
    brecha_promedio = indice_vulnerabilidad - promedio_indice_general
  ) %>%
  arrange(brecha_promedio) %>%
  mutate(
    LocNombre = factor(LocNombre, levels = LocNombre),
    tooltip = paste0(
      LocNombre,
      "<br>Índice: ", round(indice_vulnerabilidad, 1),
      "<br>Brecha frente al promedio: ",
      ifelse(brecha_promedio >= 0, "+", ""), round(brecha_promedio, 1),
      "<br>", prioridad
    )
  )

# Perfil dimensional en puntajes z (vulnerabilidad relativa)
perfil_z_largo <- acp_general$perfil_dimensional_z %>%
  pivot_longer(
    -c(LocCodigo, LocNombre),
    names_to = "dimension", values_to = "z"
  ) %>%
  left_join(sin_geo %>% select(LocCodigo, indice_vulnerabilidad), by = "LocCodigo")

tabla_dimension_dominante <- sin_geo %>%
  transmute(
    Localidad = LocNombre,
    `Índice general` = round(indice_vulnerabilidad, 1),
    `Dimensión dominante` = dimension_dominante,
    `Distancia al promedio (z)` = round(z_dimension_dominante, 2),
    Salud = round(indice_salud_100, 1),
    Protección = round(indice_proteccion_100, 1),
    Economía = round(indice_economico, 1),
    Educación = round(indice_educacion_100, 1)
  ) %>%
  arrange(desc(`Índice general`))

conteo_dominante <- sin_geo %>%
  count(dimension_dominante, name = "n") %>%
  arrange(desc(n))

# Contraste vulnerabilidad vs. oferta institucional
tabla_oferta <- NULL
if (!is.null(oferta_proteccion) && !is.null(oferta_educacion)) {
  tabla_oferta <- sin_geo %>%
    select(LocCodigo, LocNombre, indice_vulnerabilidad, prioridad) %>%
    left_join(
      oferta_proteccion %>% select(LocCodigo, NNA, centros_proteccion_10k),
      by = "LocCodigo"
    ) %>%
    left_join(
      oferta_educacion %>% select(LocCodigo, colegios_10k, beneficiarios_10k),
      by = "LocCodigo"
    ) %>%
    mutate(
      tooltip = paste0(
        LocNombre,
        "<br>Índice de vulnerabilidad: ", round(indice_vulnerabilidad, 1),
        "<br>Centros de protección por 10.000 NNA: ", round(centros_proteccion_10k, 2),
        "<br>Colegios oficiales por 10.000 NNA: ", round(colegios_10k, 2),
        "<br>NNA de referencia: ", formato_miles(NNA)
      )
    )
}

# Series temporales
series_disponibles <- perspectiva$series
opciones_series <- if (length(series_disponibles) > 0) {
  setNames(names(series_disponibles),
           vapply(series_disponibles, function(s) s$etiqueta, character(1)))
} else NULL


# COMPONENTES DE UI REUTILIZABLES

caja_dato <- function(titulo, valor, nota = NULL) {
  div(
    class = "caja-dato",
    div(class = "caja-dato-titulo", titulo),
    div(class = "caja-dato-valor", valor),
    if (!is.null(nota)) div(class = "caja-dato-nota", nota)
  )
}


# UI

ui <- bs4DashPage(
  title = "Vulnerabilidad Infantil Bogotá",
  dark = FALSE,

  header = bs4DashNavbar(title = "Vulnerabilidad Infantil Bogotá"),

  sidebar = bs4DashSidebar(
    width = 285,
    collapsed = FALSE,
    minified = TRUE,
    expandOnHover = TRUE,
    skin = "dark",
    status = "danger",

    bs4SidebarMenu(
      id = "menu_principal",
      bs4SidebarMenuItem("Descripción del visor", tabName = "descripcion",
                         icon = icon("info-circle"), selected = TRUE),
      bs4SidebarMenuItem("Visor territorial", tabName = "visor", icon = icon("map")),
      bs4SidebarMenuItem("Panorama distrital", tabName = "panorama", icon = icon("chart-bar")),
      bs4SidebarMenuItem("Clusters espaciales", tabName = "clusters", icon = icon("layer-group")),
      bs4SidebarMenuItem("Priorización territorial", tabName = "priorizacion", icon = icon("bullseye")),
      bs4SidebarMenuItem("Perspectiva y alcance", tabName = "perspectiva", icon = icon("chart-line")),
      bs4SidebarMenuItem("Metodología", tabName = "metodologia", icon = icon("cogs")),
      bs4SidebarMenuItem("Limitaciones", tabName = "limitaciones", icon = icon("triangle-exclamation"))
    )
  ),

  body = bs4DashBody(
    tags$head(
      tags$style(HTML("
        .leaflet-container { height: 620px !important; background: #fff; }
        .card { height: 100%; }
        .introduccion-visor { color: #343a40; line-height: 1.6; }
        .titulo-pestana { margin: 4px 0 16px 0; }
        .tabla-desplazable { width: 100%; overflow-x: auto; }
        .tabla-desplazable table { min-width: 720px; white-space: nowrap; }
        .escala-indice {
          background: linear-gradient(90deg, #FFFFBF, #FDAE61, #B2182B);
          border-radius: 8px; display: flex; justify-content: space-between;
          padding: 12px; font-weight: 600;
        }
        .flujo-indice { display: flex; justify-content: center; align-items: center;
          gap: 10px; flex-wrap: wrap; }
        .dimension-indice { background: #FDEDEC; border: 1px solid #E6B0AA;
          border-radius: 8px; padding: 10px 15px; font-weight: 600; }
        .indice-general { background: #B2182B; color: white; border-radius: 8px;
          padding: 10px 15px; font-weight: 600; }
        .operador-indice { font-size: 20px; font-weight: bold; }
        .pregunta-investigacion { margin-top: 18px; padding: 14px 18px;
          background: #f8f9fa; border-left: 4px solid #B2182B; font-weight: 600;
          border-radius: 4px; }
        .hipotesis { margin-top: 14px; padding: 14px 18px; background: #f8f9fa;
          border-left: 4px solid #2166AC; border-radius: 4px; }
        .caja-dato { background:#f8f9fa; border:1px solid #e3e6ea; border-radius:8px;
          padding:14px 16px; height:100%; }
        .caja-dato-titulo { font-size:12px; text-transform:uppercase;
          letter-spacing:.04em; color:#6c757d; font-weight:700; }
        .caja-dato-valor { font-size:26px; font-weight:700; color:#212529;
          margin-top:4px; line-height:1.2; }
        .caja-dato-nota { font-size:12px; color:#6c757d; margin-top:6px; }
        .bloque-interpretacion { line-height:1.65; }
        .aviso-limitacion { background:#FFF6E5; border-left:4px solid #E8A33D;
          padding:12px 16px; border-radius:4px; margin-top:12px; line-height:1.55; }
        .leyenda-cluster { display:flex; flex-wrap:wrap; gap:14px; margin-top:10px; }
        .leyenda-cluster span { display:flex; align-items:center; gap:6px;
          font-size:13px; }
        .muestra-color { width:16px; height:16px; border-radius:3px;
          border:1px solid rgba(0,0,0,.15); display:inline-block; }
      "))
    ),

    bs4TabItems(

      # DESCRIPCIÓN 
      bs4TabItem(
        tabName = "descripcion",
        h2(class = "titulo-pestana", "Descripción del visor"),

        fluidRow(
          bs4Card(
            width = 7, title = "Problema territorial",
            div(
              class = "introduccion-visor",
              p("Las condiciones asociadas al bienestar de niños, niñas y adolescentes (NNA) presentan diferencias importantes entre las 20 localidades de Bogotá. Cada territorio puede concentrar dificultades económicas, educativas, sanitarias o relacionadas con protección, y esas dificultades no se distribuyen de la misma manera ni tienen la misma composición."),
              p("El visor integra cuatro dimensiones en una escala común para comparar territorios, identificar el perfil de cada uno y examinar si la vulnerabilidad forma patrones espaciales. El objetivo no es producir un ranking, sino orientar una respuesta distrital con intensidad diferenciada."),
              div(class = "pregunta-investigacion",
                  "Pregunta analítica: ¿Qué perfiles territoriales de vulnerabilidad infantil existen en Bogotá, cómo se distribuyen espacialmente y qué implicaciones tienen para una respuesta distrital con intensidad diferenciada según las necesidades territoriales?"),
              div(class = "hipotesis",
                  strong("Hipótesis espacial: "),
                  "la vulnerabilidad infantil no se distribuye aleatoriamente entre las localidades de Bogotá y presenta patrones de dependencia espacial identificables mediante Moran global y LISA.",
                  br(), br(),
                  strong("Resultado obtenido: "),
                  textOutput("veredicto_hipotesis", inline = TRUE))
            )
          ),
          bs4Card(
            width = 5, title = "¿Qué permite hacer?",
            tags$ul(
              tags$li("Comparar las 20 localidades en una escala común de 0 a 100."),
              tags$li("Identificar la dimensión que caracteriza a cada territorio."),
              tags$li("Examinar si existe agrupación espacial (Moran global y LISA)."),
              tags$li("Contrastar vulnerabilidad contra oferta institucional instalada."),
              tags$li("Apoyar la priorización territorial de una estrategia distrital.")
            ),
            div(class = "escala-indice",
                span("0 · Menor vulnerabilidad"),
                span("100 · Mayor vulnerabilidad")),
            div(class = "aviso-limitacion",
                strong("La escala es relativa. "),
                "0 corresponde a la localidad menos vulnerable observada y 100 a la más vulnerable observada, no a un mínimo o máximo absoluto.")
          )
        ),

        fluidRow(
          bs4Card(
            width = 12, title = "Construcción del índice de vulnerabilidad",
            div(
              class = "flujo-indice",
              span(class = "dimension-indice", "Salud"),
              span(class = "operador-indice", "+"),
              span(class = "dimension-indice", "Protección"),
              span(class = "operador-indice", "+"),
              span(class = "dimension-indice", "Economía"),
              span(class = "operador-indice", "+"),
              span(class = "dimension-indice", "Educación"),
              span(class = "operador-indice", "→"),
              span(class = "indice-general", "Índice de vulnerabilidad")
            ),
            br(),
            p("Cada dimensión se construye estandarizando sus variables, invirtiendo las protectoras para que un valor alto siempre signifique mayor vulnerabilidad, y promediando los puntajes resultantes. El índice general pondera las cuatro dimensiones por igual. El detalle y las razones de cada decisión están en la pestaña de Metodología.")
          )
        )
      ),

      # VISOR TERRITORIAL
      bs4TabItem(
        tabName = "visor",
        h2(class = "titulo-pestana", "Visor territorial por localidad"),

        fluidRow(
          bs4Card(
            width = 6, title = "Mapa de localidades",
            selectInput(
              "indicador_mapa", "Seleccionar indicador:",
              choices = setNames(variables_indicadores, unname(titulos_indicadores))
            ),
            leafletOutput("mapa_localidades", height = "620px")
          ),

          column(
            width = 6,
            bs4Card(
              width = 12, title = "Perfil de la localidad",
              h4(textOutput("nombre_localidad")),
              echarts4rOutput("perfil_localidad", height = "270px"),
              div(class = "tabla-desplazable", tableOutput("tabla_localidad")),
              uiOutput("resumen_localidad")
            ),
            bs4Card(
              width = 12, title = "Ranking de localidades",
              echarts4rOutput("ranking_localidades", height = "320px")
            )
          )
        ),

        fluidRow(
          bs4Card(
            width = 12, title = "Comparación entre indicadores",
            selectInput("indicador_comparacion", "Comparar con:", choices = NULL),
            echarts4rOutput("comparacion_indicadores", height = "400px"),
            uiOutput("nota_comparacion")
          )
        )
      ),

      # PANORAMA 
      bs4TabItem(
        tabName = "panorama",
        h2(class = "titulo-pestana", "Panorama distrital"),

        fluidRow(
          bs4Card(
            width = 12, title = "¿Dónde está la mayor vulnerabilidad?",
            uiOutput("texto_donde"),
            echarts4rOutput("brechas_promedio", height = "600px")
          )
        ),

        fluidRow(
          bs4Card(
            width = 12, title = "¿Qué dimensión caracteriza a cada territorio?",
            p("La dimensión dominante es aquella en la que la localidad se aleja más, hacia arriba, del promedio distrital. Mide vulnerabilidad ", strong("relativa"), " dentro de cada dimensión, no el valor absoluto del índice dimensional."),
            fluidRow(
              column(width = 5, echarts4rOutput("conteo_dominante", height = "320px")),
              column(width = 7, echarts4rOutput("perfil_z_heatmap", height = "560px"))
            ),
            div(class = "tabla-desplazable", DTOutput("tabla_dominante"))
          )
        ),

        fluidRow(
          bs4Card(
            width = 12, title = "Perfiles de vulnerabilidad por localidad (escala 0-100)",
            echarts4rOutput("mapa_calor_perfiles", height = "620px")
          )
        ),

        fluidRow(
          bs4Card(
            width = 12, title = "¿Qué dimensiones explican el índice general?",
            uiOutput("texto_cargas"),
            fluidRow(
              column(width = 6, echarts4rOutput("grafico_cargas", height = "320px")),
              column(width = 6, div(class = "tabla-desplazable", tableOutput("tabla_cargas")))
            ),
            h5("Correlación entre dimensiones"),
            div(class = "tabla-desplazable", tableOutput("tabla_correlaciones"))
          )
        ),

        if (!is.null(tabla_oferta)) fluidRow(
          bs4Card(
            width = 12, title = "Vulnerabilidad frente a oferta institucional instalada",
            p("Las tasas de oferta pública por 10.000 NNA (colegios oficiales, centros de protección, beneficios escolares) ", strong("no forman parte del índice"), ", porque son oferta focalizada: el Distrito las ubica donde hay más necesidad, de modo que son más altas justamente en las localidades más vulnerables. Se muestran aquí como capa de respuesta institucional. El cuadrante de interés para la política es el de alta vulnerabilidad con baja oferta."),
            selectInput(
              "variable_oferta", "Indicador de oferta:",
              choices = c(
                "Centros de protección por 10.000 NNA" = "centros_proteccion_10k",
                "Colegios oficiales por 10.000 NNA"    = "colegios_10k",
                "Beneficios escolares por 10.000 NNA"  = "beneficiarios_10k"
              )
            ),
            echarts4rOutput("grafico_oferta", height = "440px")
          )
        )
      ),

      # CLUSTERS ESPACIALES
      bs4TabItem(
        tabName = "clusters",
        h2(class = "titulo-pestana", "Clusters espaciales"),

        fluidRow(
          column(width = 3, uiOutput("caja_moran_i")),
          column(width = 3, uiOutput("caja_moran_e")),
          column(width = 3, uiOutput("caja_moran_p")),
          column(width = 3, uiOutput("caja_lisa_n"))
        ),

        br(),

        fluidRow(
          bs4Card(
            width = 7, title = "Mapa de conglomerados LISA",
            leafletOutput("mapa_clusters", height = "620px"),
            uiOutput("leyenda_clusters")
          ),
          bs4Card(
            width = 5, title = "Diagrama de dispersión de Moran",
            echarts4rOutput("scatter_moran", height = "400px"),
            p(class = "caja-dato-nota",
              "Cada punto es una localidad. El eje horizontal es el índice estandarizado y el vertical el promedio de sus vecinas. La pendiente de la recta es el I de Moran.")
          )
        ),

        fluidRow(
          bs4Card(
            width = 12, title = "¿La vulnerabilidad infantil presenta un patrón espacial en Bogotá?",
            div(class = "bloque-interpretacion", uiOutput("texto_espacial"))
          )
        ),

        fluidRow(
          bs4Card(
            width = 12, title = "Resultados LISA por localidad",
            div(class = "tabla-desplazable", DTOutput("tabla_lisa")),
            div(class = "aviso-limitacion",
                strong("LISA exploratorio. "),
                "Se usa un umbral de p < 0,05 sin corrección por comparaciones múltiples. Con 20 pruebas simultáneas la probabilidad de al menos un falso positivo es sustancialmente mayor al 5%. La clasificación se usa como herramienta exploratoria de priorización y no como prueba confirmatoria.")
          )
        )
      ),

      # PRIORIZACION
      bs4TabItem(
        tabName = "priorizacion",
        h2(class = "titulo-pestana", "Priorización territorial"),

        fluidRow(
          bs4Card(
            width = 12, title = "Enfoque de ciudad",
            div(
              class = "introduccion-visor",
              p("El producto está formulado como ", strong("una estrategia distrital con intensidad territorial diferenciada"), ", no como veinte programas independientes. Las localidades se usan como unidades territoriales para detectar concentración, perfiles, desigualdades y patrones espaciales, y para graduar la intensidad de una misma arquitectura de intervención."),
              div(class = "aviso-limitacion",
                  strong("Las categorías no son una clasificación causal. "),
                  "Son una herramienta de priorización construida a partir del nivel del índice, el perfil dimensional, el patrón espacial cuando es significativo y la población de referencia disponible. No estiman el efecto de ninguna intervención.")
            )
          )
        ),

        fluidRow(
          bs4Card(
            width = 6, title = "Mapa de priorización",
            leafletOutput("mapa_prioridad", height = "560px")
          ),
          bs4Card(
            width = 6, title = "Reglas de clasificación",
            tags$ol(
              tags$li(strong("Intensificación prioritaria: "),
                      "vulnerabilidad en el tercil superior del índice, o vulnerabilidad media que además pertenece a un conglomerado Alto-Alto."),
              tags$li(strong("Prevención reforzada: "),
                      "vulnerabilidad en el tercil intermedio, o vulnerabilidad baja rodeada de entornos vulnerables (patrón Bajo-Alto)."),
              tags$li(strong("Sostenimiento y monitoreo: "),
                      "el resto de localidades.")
            ),
            echarts4rOutput("conteo_prioridad", height = "260px")
          )
        ),

        fluidRow(
          bs4Card(
            width = 12, title = "Priorización por localidad",
            div(class = "tabla-desplazable", DTOutput("tabla_prioridad"))
          )
        ),

        fluidRow(
          bs4Card(
            width = 12, title = "Componentes de la arquitectura distrital",
            uiOutput("recomendaciones")
          )
        )
      ),

      # PERSPECTIVA Y ALCANCE 
      bs4TabItem(
        tabName = "perspectiva",
        h2(class = "titulo-pestana", "Perspectiva y alcance"),

        fluidRow(
          bs4Card(
            width = 12, title = "Qué es y qué no es este componente",
            div(class = "bloque-interpretacion", uiOutput("texto_perspectiva")),
            div(class = "aviso-limitacion",
                strong("No hay modelo predictivo del índice. "),
                "Con 20 unidades territoriales, diseño transversal y sin serie temporal del índice compuesto, no es estadísticamente defendible construir un pronóstico. Lo que se muestra son tendencias históricas observadas de indicadores concretos que sí tienen serie anual suficiente.")
          )
        ),

        if (length(series_disponibles) > 0) fluidRow(
          bs4Card(
            width = 12, title = "Tendencia histórica observada",
            selectInput("serie_seleccionada", "Indicador con serie anual:",
                        choices = opciones_series),
            checkboxInput(
              "incluir_inestables",
              "Incluir localidades con denominador pequeño (serie inestable)",
              value = FALSE
            ),
            uiOutput("encabezado_serie"),
            echarts4rOutput("grafico_serie", height = "460px"),
            h5("Cambio entre el primer y el último año disponible"),
            echarts4rOutput("grafico_cambio", height = "460px"),
            div(class = "tabla-desplazable", DTOutput("tabla_cambio"))
          )
        ),

        fluidRow(
          bs4Card(
            width = 12, title = "Inventario de series temporales revisadas",
            p("Se comprobó programáticamente qué series existen, cuántos años cubren y cuántas localidades tienen información. Solo se construyen las que superan el umbral mínimo."),
            div(class = "tabla-desplazable", tableOutput("tabla_inventario"))
          )
        ),

        fluidRow(
          bs4Card(
            width = 12, title = "Alcance poblacional (población de referencia)",
            div(class = "bloque-interpretacion", uiOutput("texto_alcance")),
            echarts4rOutput("grafico_alcance", height = "480px"),
            div(class = "tabla-desplazable", DTOutput("tabla_alcance"))
          )
        )
      ),

      # METODOLOGÍA
      bs4TabItem(
        tabName = "metodologia",
        h2(class = "titulo-pestana", "Metodología"),

        fluidRow(
          bs4Card(
            width = 12, title = "Unidad de análisis y diseño",
            div(class = "bloque-interpretacion",
              p(strong("Unidad de análisis: "), "las 20 localidades de Bogotá."),
              p(strong("Diseño: "), "transversal y territorial. El índice describe la situación relativa de cada localidad en el periodo de referencia. No es una predicción ni una trayectoria."),
              p(strong("Pregunta analítica: "), "¿Qué perfiles territoriales de vulnerabilidad infantil existen en Bogotá, cómo se distribuyen espacialmente y qué implicaciones tienen para una respuesta distrital con intensidad diferenciada según las necesidades territoriales?")
            )
          )
        ),

        fluidRow(
          bs4Card(
            width = 12, title = "Fuentes de datos",
            div(class = "tabla-desplazable", tableOutput("tabla_fuentes"))
          )
        ),

        fluidRow(
          bs4Card(
            width = 12, title = "Construcción de las dimensiones",
            div(class = "bloque-interpretacion",
              p("Las cuatro dimensiones se construyen con el mismo procedimiento:"),
              tags$ol(
                tags$li(strong("Estandarización. "), "Cada variable se convierte a puntaje z entre localidades."),
                tags$li(strong("Armonización de signo. "), "Las variables protectoras (alto = mejor situación) se invierten, de modo que todas queden con el sentido alto = peor situación."),
                tags$li(strong("Índice dimensional. "), "Promedio de los puntajes z armonizados."),
                tags$li(strong("Normalización. "), "Reescalado 0-100 sobre el rango observado entre localidades.")
              ),
              p(strong("Verificación de orientación. "), "El pipeline comprueba automáticamente, mediante asserts que detienen la ejecución si fallan, que cada índice dimensional correlaciona en el sentido sustantivo esperado: salud sube con mortalidad infantil y baja con afiliación; protección sube con violencia y baja con disponibilidad de comisarías; educación sube con deserción y reprobación; economía baja con ingreso y sube con desempleo.")
            ),
            h5("Variables de cada dimensión"),
            div(class = "tabla-desplazable", tableOutput("tabla_variables_dimension"))
          )
        ),

        fluidRow(
          bs4Card(
            width = 12, title = "Papel del ACP y construcción del índice general",
            div(class = "bloque-interpretacion",
              p("El proyecto se planteó originalmente construir cada índice con el primer componente principal (CP1) de un ACP. Al corregir la orientación de las variables ese enfoque dejó de ser defendible y se documenta aquí el porqué:"),
              tags$ul(
                tags$li(strong("En salud, "), "el CP1 resultaba ser un contraste entre el bloque nutricional de 0 a 5 años y el de 5 a 17, con una correlación de apenas 0,07 frente al promedio estandarizado de vulnerabilidad. Con una correlación tan cercana a cero, el criterio de reorientación por signo queda indeterminado."),
                tags$li(strong("En el índice general, "), "el CP1 sobre las cuatro dimensiones corregidas tiene cargas de signo mixto. Ponderar con él restaría unas dimensiones a otras y el resultado no sería interpretable como vulnerabilidad."),
                tags$li(strong("Con N = 20 "), "y pocas variables por dimensión, el promedio de puntajes z es más estable que un componente principal y garantiza la orientación por construcción.")
              ),
              p("Por eso el índice general ", strong("pondera las cuatro dimensiones por igual (25% cada una)"), " sobre sus puntajes estandarizados, y el ACP se conserva y se reporta como diagnóstico de la estructura de correlación (cargas y varianza explicada, visibles en Panorama distrital). El pipeline emite una advertencia explícita si alguna dimensión no correlaciona positivamente con el índice general."),
              p(strong("Variables excluidas por redundancia composicional. "),
                "Se retiraron variables que son el complemento aritmético de otras del mismo bloque, porque duplican la misma información con signo opuesto y generan correlaciones artificiales: en salud, la proporción con peso adecuado para la edad y la proporción con IMC adecuado; en educación, la tasa de aprobación, que equivale a 100 menos deserción menos reprobación.")
            )
          )
        ),

        fluidRow(
          bs4Card(
            width = 12, title = "Tratamiento de la oferta pública focalizada",
            div(class = "bloque-interpretacion",
              p("Las tasas de oferta pública por 10.000 NNA (colegios oficiales, beneficios de alimentación y movilidad escolar, jardines infantiles, centros AMAR, CRECER, PROTEGER, ABRAZAR, RENACER, casas de pensamiento intercultural y espacios rurales) ", strong("no entran al índice"), "."),
              p("La razón es empírica: son oferta focalizada. El Distrito las ubica donde hay mayor necesidad, de modo que su tasa por 10.000 NNA es más alta en las localidades más pobres. Tratarlas como factor protector invertía por completo la dimensión de educación —Teusaquillo y Chapinero aparecían como las más vulnerables y Sumapaz como la menos— y dejaba educación correlacionando -0,74 con economía, lo que impedía construir un índice general coherente."),
              p("Estas tasas se conservan como ", strong("capa de oferta institucional"), " y el visor las contrasta contra el índice en la pestaña de Panorama distrital. Ese contraste, y no su inclusión en el índice, es lo que aporta valor prescriptivo."),
              p(strong("Excepción: "), "la tasa de comisarías de familia sí entra en la dimensión de protección, porque su distribución responde al diseño administrativo de acceso a la justicia familiar y no a focalización socioeconómica.")
            )
          )
        ),

        fluidRow(
          bs4Card(
            width = 12, title = "Análisis espacial",
            div(class = "bloque-interpretacion",
              tags$ul(
                tags$li(strong("Matriz de vecindad: "), "contigüidad tipo reina construida con ", code("poly2nb(queen = TRUE)"), " sobre la cartografía oficial de localidades. Dos localidades son vecinas si comparten al menos un punto de frontera."),
                tags$li(strong("Matriz de pesos: "), code("nb2listw(style = \"W\", zero.policy = TRUE)"), ". Estandarización por filas: el rezago espacial de cada localidad es el promedio de sus vecinas."),
                tags$li(strong("Moran global: "), code("moran.test()"), " bajo aleatorización, con hipótesis alternativa de autocorrelación positiva."),
                tags$li(strong("Moran local (LISA): "), code("localmoran()"), ". La clasificación en Alto-Alto, Bajo-Bajo, Alto-Bajo y Bajo-Alto se deriva del signo del índice estandarizado y del signo de su rezago espacial. Ningún cluster se asigna manualmente."),
                tags$li(strong("Umbral de significancia local: "), "p < 0,05 ", strong("sin"), " corrección por comparaciones múltiples, decisión documentada como limitación exploratoria.")
              ),
              uiOutput("resumen_vecindad_texto")
            )
          )
        ),

        fluidRow(
          bs4Card(
            width = 12, title = "Priorización territorial",
            div(class = "bloque-interpretacion",
              p("La priorización combina cuatro criterios en este orden:"),
              tags$ol(
                tags$li(strong("Nivel de vulnerabilidad: "), "terciles del índice general."),
                tags$li(strong("Patrón espacial LISA: "), "se aplica únicamente cuando es estadísticamente significativo. Una localidad de vulnerabilidad media dentro de un conglomerado Alto-Alto asciende a intensificación prioritaria; una de vulnerabilidad baja con patrón Bajo-Alto asciende a prevención reforzada."),
                tags$li(strong("Perfil dimensional: "), "la dimensión de mayor vulnerabilidad relativa define el contenido de la intervención, no su intensidad."),
                tags$li(strong("Alcance poblacional: "), "los NNA de referencia dimensionan la escala operativa de la respuesta, sin alterar la categoría de prioridad.")
              )
            )
          )
        )
      ),

      # LIMITACIONES
      bs4TabItem(
        tabName = "limitaciones",
        h2(class = "titulo-pestana", "Limitaciones"),
        fluidRow(
          bs4Card(
            width = 12, title = "Limitaciones del análisis",
            div(class = "bloque-interpretacion", uiOutput("lista_limitaciones"))
          )
        )
      )
    )
  )
)

 
# SERVER

server <- function(input, output, session) {

  localidad_seleccionada <- reactiveVal(NULL)

  # Opciones de comparación
  observeEvent(input$indicador_mapa, {
    req(input$indicador_mapa)
    disponibles <- setdiff(variables_indicadores, input$indicador_mapa)
    seleccion_actual <- isolate(input$indicador_comparacion)
    seleccion <- if (!is.null(seleccion_actual) && seleccion_actual %in% disponibles)
      seleccion_actual else disponibles[1]

    updateSelectInput(
      session, "indicador_comparacion",
      choices = setNames(disponibles, unname(titulos_indicadores[disponibles])),
      selected = seleccion
    )
  }, ignoreInit = FALSE)

  #  Veredicto de la hipótesis 
  output$veredicto_hipotesis <- renderText({
    m <- espacial$moran
    if (m$significativo) {
      paste0(
        "la hipótesis queda respaldada a escala global (I de Moran = ",
        sprintf("%.3f", m$I), "; p = ", sprintf("%.3f", m$p_valor),
        "), aunque el análisis local identifica ", espacial$n_significativas,
        if (espacial$n_significativas == 1) " localidad significativa"
        else " localidades significativas", ". Ver Clusters espaciales."
      )
    } else {
      paste0(
        "la hipótesis NO queda respaldada (I de Moran = ",
        sprintf("%.3f", m$I), "; p = ", sprintf("%.3f", m$p_valor),
        "). Ver Clusters espaciales."
      )
    }
  })

  # MAPA PRINCIPAL
  output$mapa_localidades <- renderLeaflet({
    req(input$indicador_mapa)
    datos_mapa <- datos %>% mutate(valor = .data[[input$indicador_mapa]])

    pal <- colorNumeric(palette = PALETA_INDICE, domain = c(0, 100), na.color = "#808080")

    leaflet(datos_mapa) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        layerId = ~LocCodigo,
        fillColor = ~pal(valor), fillOpacity = 0.85,
        color = "white", weight = 1.5,
        highlightOptions = highlightOptions(weight = 3, color = "#666", bringToFront = TRUE),
        label = ~paste0(LocNombre, ": ", round(valor, 1)),
        popup = ~paste0(
          "<b>", LocNombre, "</b><br>",
          titulos_indicadores[input$indicador_mapa], ": ", round(valor, 2),
          "<br>Dimensión dominante: ", dimension_dominante,
          "<br>Prioridad: ", prioridad
        )
      ) %>%
      addLegend(pal = pal, values = c(0, 100),
                title = titulos_indicadores[input$indicador_mapa],
                position = "bottomright") %>%
      setView(lng = -74.15, lat = 4.55, zoom = 10)
  })

  observeEvent(input$mapa_localidades_shape_click, {
    click <- input$mapa_localidades_shape_click
    if (!is.null(click$id)) localidad_seleccionada(click$id)
  })

  seleccionado <- reactive({
    req(localidad_seleccionada())
    datos %>% filter(LocCodigo == localidad_seleccionada())
  })

  output$nombre_localidad <- renderText({
    if (is.null(localidad_seleccionada())) "Haz clic en una localidad del mapa"
    else seleccionado()$LocNombre
  })

  output$perfil_localidad <- renderEcharts4r({
    if (is.null(localidad_seleccionada()) || nrow(seleccionado()) == 0) {
      df <- perfil_bogota; titulo <- "Promedio Bogotá"
    } else {
      x <- seleccionado()
      df <- data.frame(
        indicador = c("Salud", "Protección", "Economía", "Educación", "Vulnerabilidad"),
        valor = c(x$indice_salud_100, x$indice_proteccion_100,
                  x$indice_economico, x$indice_educacion_100,
                  x$indice_vulnerabilidad)
      )
      titulo <- x$LocNombre
    }
    df %>%
      e_charts(indicador) %>%
      e_bar(valor, name = titulo) %>%
      e_y_axis(max = 100) %>%
      e_tooltip(trigger = "axis") %>%
      e_color("#B2182B") %>%
      e_title(titulo)
  })

  output$tabla_localidad <- renderTable({
    if (is.null(localidad_seleccionada()) || nrow(seleccionado()) == 0) {
      perfil_bogota %>%
        pivot_wider(names_from = indicador, values_from = valor) %>%
        mutate(Localidad = "Promedio Bogotá") %>%
        select(Localidad, Economía, Salud, Protección, Educación, Vulnerabilidad)
    } else {
      seleccionado() %>%
        st_drop_geometry() %>%
        select(
          Localidad = LocNombre, Economía = indice_economico,
          Salud = indice_salud_100, Protección = indice_proteccion_100,
          Educación = indice_educacion_100, Vulnerabilidad = indice_vulnerabilidad
        )
    }
  }, digits = 1)

  output$resumen_localidad <- renderUI({
    if (is.null(localidad_seleccionada()) || nrow(seleccionado()) == 0) return(NULL)
    x <- st_drop_geometry(seleccionado())
    div(
      class = "aviso-limitacion",
      strong(x$LocNombre), ". ", x$justificacion, " ",
      "Prioridad asignada: ", strong(as.character(x$prioridad)), "."
    )
  })

  output$ranking_localidades <- renderEcharts4r({
    req(input$indicador_mapa)
    rankings_precalculados[[input$indicador_mapa]] %>%
      e_charts(LocNombre) %>%
      e_bar(valor, name = titulos_indicadores[input$indicador_mapa]) %>%
      e_flip_coords() %>%
      e_tooltip() %>%
      e_color("#B2182B")
  })

  output$comparacion_indicadores <- renderEcharts4r({
    req(input$indicador_mapa, input$indicador_comparacion)
    validate(need(input$indicador_mapa != input$indicador_comparacion,
                  "Seleccione dos indicadores diferentes"))
    clave <- paste(input$indicador_mapa, input$indicador_comparacion, sep = "__")
    comparaciones_precalculadas[[clave]] %>%
      e_charts(valor_x) %>%
      e_scatter(valor_y, bind = tooltip, symbol_size = 12) %>%
      e_line(ajuste, name = "Ajuste lineal", symbol = "none") %>%
      e_x_axis(min = 0, max = 100, name = titulos_indicadores[input$indicador_mapa]) %>%
      e_y_axis(min = 0, max = 100, name = titulos_indicadores[input$indicador_comparacion]) %>%
      e_tooltip() %>%
      e_color(c("#B2182B", "#666666"))
  })

  output$nota_comparacion <- renderUI({
    req(input$indicador_mapa, input$indicador_comparacion)
    if (input$indicador_mapa == input$indicador_comparacion) return(NULL)
    clave <- paste(input$indicador_mapa, input$indicador_comparacion, sep = "__")
    r <- correlaciones_pares[[clave]]
    div(class = "aviso-limitacion",
        "Correlación de Pearson entre ambos indicadores: ", strong(sprintf("%+.3f", r)),
        ". Con N = 20 localidades, la asociación es descriptiva y no implica causalidad.")
  })

  # PANORAMA 
  output$texto_donde <- renderUI({
    top <- sin_geo %>% arrange(desc(indice_vulnerabilidad)) %>% head(5)
    piezas <- lapply(seq_len(nrow(top)), function(i) {
      tags$li(
        strong(top$LocNombre[i]), " — índice ",
        sprintf("%.1f", top$indice_vulnerabilidad[i]),
        ". Dimensión de mayor vulnerabilidad relativa: ",
        strong(top$dimension_dominante[i]),
        " (", sprintf("%+.2f", top$z_dimension_dominante[i]),
        " desviaciones sobre el promedio distrital). Perfil: salud ",
        sprintf("%.0f", top$indice_salud_100[i]), ", protección ",
        sprintf("%.0f", top$indice_proteccion_100[i]), ", economía ",
        sprintf("%.0f", top$indice_economico[i]), ", educación ",
        sprintf("%.0f", top$indice_educacion_100[i]), "."
      )
    })
    tagList(
      p("Las cinco localidades con mayor índice general y su perfil dimensional:"),
      tags$ul(piezas),
      p(class = "caja-dato-nota",
        "El gráfico muestra la brecha de cada localidad frente al promedio distrital del índice.")
    )
  })

  output$brechas_promedio <- renderEcharts4r({
    brechas_promedio_precalculadas %>%
      e_charts(LocNombre) %>%
      e_bar(brecha_promedio, bind = tooltip, name = "Brecha frente al promedio") %>%
      e_flip_coords() %>%
      e_tooltip() %>%
      e_visual_map(
        brecha_promedio, show = FALSE,
        inRange = list(color = c("#2166AC", "#F7F7F7", "#B2182B"))
      )
  })

  output$conteo_dominante <- renderEcharts4r({
    conteo_dominante %>%
      e_charts(dimension_dominante) %>%
      e_pie(n, radius = c("40%", "70%")) %>%
      e_tooltip() %>%
      e_title("Localidades por dimensión dominante") %>%
      e_legend(bottom = 0)
  })

  output$perfil_z_heatmap <- renderEcharts4r({
    orden <- sin_geo %>% arrange(indice_vulnerabilidad) %>% pull(LocNombre)
    perfil_z_largo %>%
      mutate(LocNombre = factor(LocNombre, levels = orden),
             z = round(z, 2)) %>%
      e_charts(dimension) %>%
      e_heatmap(LocNombre, z) %>%
      e_visual_map(z, orient = "horizontal", bottom = 0,
                   inRange = list(color = c("#2166AC", "#F7F7F7", "#B2182B"))) %>%
      e_tooltip() %>%
      e_title("Vulnerabilidad relativa (puntaje z por dimensión)")
  })

  output$tabla_dominante <- renderDT({
    datatable(
      tabla_dimension_dominante,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE,
                     language = list(url = NULL, search = "Buscar:")),
      caption = "Dimensión de mayor vulnerabilidad relativa por localidad"
    )
  })

  output$mapa_calor_perfiles <- renderEcharts4r({
    matriz_perfiles_precalculada %>%
      e_charts(indicador) %>%
      e_heatmap(LocNombre, valor) %>%
      e_visual_map(valor, min = 0, max = 100, orient = "horizontal", bottom = 0,
                   inRange = list(color = PALETA_INDICE)) %>%
      e_tooltip()
  })

  output$texto_cargas <- renderUI({
    tagList(
      p(acp_general$texto_dimension_mayor_carga),
      if (!acp_general$cp1_signo_unico) div(
        class = "aviso-limitacion",
        strong("Por qué el índice no se pondera con el ACP. "),
        "Las cargas del primer componente tienen signos mixtos, de modo que ponderar con ellas restaría unas dimensiones a otras. El índice usa pesos iguales y el ACP se reporta solo como diagnóstico. El detalle está en la pestaña de Metodología."
      )
    )
  })

  output$grafico_cargas <- renderEcharts4r({
    acp_general$cargas_acp_general %>%
      mutate(carga_cp1 = round(carga_cp1, 3)) %>%
      arrange(carga_cp1) %>%
      mutate(dimension = factor(dimension, levels = dimension)) %>%
      e_charts(dimension) %>%
      e_bar(carga_cp1, name = "Carga en CP1") %>%
      e_flip_coords() %>%
      e_tooltip() %>%
      e_visual_map(carga_cp1, show = FALSE,
                   inRange = list(color = c("#2166AC", "#F7F7F7", "#B2182B"))) %>%
      e_title("Cargas del CP1 (diagnóstico)")
  })

  output$tabla_cargas <- renderTable({
    acp_general$cargas_acp_general %>%
      transmute(
        Dimensión = dimension,
        `Carga CP1` = carga_cp1,
        `Peso en el índice` = peso_en_indice,
        `Correlación con el índice` = correlacion_con_indice
      )
  }, digits = 3)

  output$tabla_correlaciones <- renderTable({
    m <- acp_general$matriz_correlacion_dimensiones
    cbind(Dimensión = rownames(m), as.data.frame(round(m, 2)))
  }, digits = 2)

  output$grafico_oferta <- renderEcharts4r({
    req(tabla_oferta, input$variable_oferta)
    etiqueta <- c(
      centros_proteccion_10k = "Centros de protección por 10.000 NNA",
      colegios_10k           = "Colegios oficiales por 10.000 NNA",
      beneficiarios_10k      = "Beneficios escolares por 10.000 NNA"
    )[[input$variable_oferta]]

    tabla_oferta %>%
      mutate(oferta = .data[[input$variable_oferta]]) %>%
      filter(!is.na(oferta)) %>%
      e_charts(indice_vulnerabilidad) %>%
      e_scatter(oferta, bind = tooltip, symbol_size = 14) %>%
      e_x_axis(name = "Índice de vulnerabilidad", min = 0, max = 100) %>%
      e_y_axis(name = etiqueta) %>%
      e_tooltip() %>%
      e_color("#B2182B") %>%
      e_legend(show = FALSE)
  })

  # CLUSTERS ESPACIALES
  output$caja_moran_i <- renderUI(
    caja_dato("I de Moran", sprintf("%.4f", espacial$moran$I),
              "Índice de vulnerabilidad, contigüidad reina")
  )
  output$caja_moran_e <- renderUI(
    caja_dato("Esperanza E[I]", sprintf("%.4f", espacial$moran$esperanza),
              "Valor esperado bajo aleatoriedad espacial")
  )
  output$caja_moran_p <- renderUI(
    caja_dato("Valor p", sprintf("%.4f", espacial$moran$p_valor),
              paste0("z = ", sprintf("%.3f", espacial$moran$z), " · ",
                     if (espacial$moran$significativo) "significativo a 0,05"
                     else "no significativo a 0,05"))
  )
  output$caja_lisa_n <- renderUI(
    caja_dato("Localidades LISA significativas",
              paste0(espacial$n_significativas, " de ", nrow(espacial$tabla_lisa)),
              paste0("Umbral p < ", espacial$umbral_lisa, ", sin corrección múltiple"))
  )

  output$mapa_clusters <- renderLeaflet({
    pal <- colorFactor(
      palette = unname(COLORES_CLUSTER[espacial$niveles_cluster]),
      levels = espacial$niveles_cluster
    )
    leaflet(datos) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(cluster), fillOpacity = 0.85,
        color = "white", weight = 1.5,
        highlightOptions = highlightOptions(weight = 3, color = "#666", bringToFront = TRUE),
        label = ~paste0(LocNombre, ": ", cluster),
        popup = ~paste0(
          "<b>", LocNombre, "</b><br>",
          "Conglomerado: ", cluster, "<br>",
          "Índice: ", round(indice_vulnerabilidad, 1), "<br>",
          "Índice estandarizado (z): ", round(z_indice, 2), "<br>",
          "Rezago espacial (z de vecinas): ", round(rezago_z, 2), "<br>",
          "I local: ", round(I_local, 3), "<br>",
          "p local: ", round(p_local, 3), "<br>",
          "Vecinas: ", n_vecinos
        )
      ) %>%
      addLegend(
        colors = unname(COLORES_CLUSTER[espacial$niveles_cluster]),
        labels = espacial$niveles_cluster,
        title = "Conglomerado LISA", position = "bottomright", opacity = 0.9
      ) %>%
      setView(lng = -74.15, lat = 4.55, zoom = 10)
  })

  output$leyenda_clusters <- renderUI({
    conteo <- espacial$conteo_clusters
    div(
      class = "leyenda-cluster",
      lapply(espacial$niveles_cluster, function(nivel) {
        span(
          span(class = "muestra-color",
               style = paste0("background:", COLORES_CLUSTER[[nivel]], ";")),
          paste0(nivel, " (", conteo[[nivel]], ")")
        )
      })
    )
  })

  output$scatter_moran <- renderEcharts4r({
    espacial$tabla_lisa %>%
      mutate(
        tooltip = paste0(
          LocNombre, "<br>z del índice: ", round(z_indice, 2),
          "<br>rezago espacial: ", round(rezago_z, 2),
          "<br>p local: ", round(p_local, 3),
          "<br>", cluster
        ),
        ajuste = as.numeric(predict(lm(rezago_z ~ z_indice)))
      ) %>%
      e_charts(z_indice) %>%
      e_scatter(rezago_z, bind = tooltip, symbol_size = 13) %>%
      e_line(ajuste, name = "Pendiente = I de Moran", symbol = "none") %>%
      e_x_axis(name = "Índice estandarizado (z)") %>%
      e_y_axis(name = "Rezago espacial") %>%
      e_tooltip() %>%
      e_color(c("#B2182B", "#666666"))
  })

  output$texto_espacial <- renderUI({
    tagList(
      p(espacial$interpretacion_moran_global),
      p(espacial$interpretacion_lisa),
      if (nzchar(espacial$lectura_conjunta)) p(strong("Lectura conjunta. "),
                                               espacial$lectura_conjunta)
    )
  })

  output$tabla_lisa <- renderDT({
    tabla <- espacial$tabla_lisa %>%
      transmute(
        Localidad = LocNombre,
        `Índice de vulnerabilidad` = round(indice_vulnerabilidad, 1),
        `z del índice` = round(z_indice, 2),
        `Rezago espacial` = round(rezago_z, 2),
        `I local` = round(I_local, 3),
        `p local` = round(p_local, 3),
        Vecinas = n_vecinos,
        Cluster = as.character(cluster)
      ) %>%
      arrange(`p local`)

    datatable(
      tabla, rownames = FALSE,
      options = list(pageLength = 20, scrollX = TRUE, dom = "tip")
    ) %>%
      formatStyle(
        "Cluster",
        backgroundColor = styleEqual(
          names(COLORES_CLUSTER), unname(COLORES_CLUSTER)
        ),
        color = styleEqual(
          names(COLORES_CLUSTER),
          c("white", "white", "black", "black", "black")
        )
      )
  })

  output$resumen_vecindad_texto <- renderUI({
    rv <- espacial$resumen_vecindad
    p(
      "En la cartografía utilizada, cada localidad tiene en promedio ",
      strong(sprintf("%.1f", espacial$promedio_vecinas)), " vecinas ",
      "(mínimo ", min(rv$n_vecinos), ", máximo ", max(rv$n_vecinos), "). ",
      if (espacial$n_sin_vecinos > 0)
        paste0("Localidades sin vecinas: ",
               paste(espacial$localidades_sin_vecinos, collapse = ", "),
               ". Se usa zero.policy = TRUE para conservarlas en el análisis.")
      else "Ninguna localidad queda aislada, de modo que zero.policy no altera el resultado.",
      " Con tan pocas vecinas por unidad, el estadístico local de Moran se apoya en muy pocas observaciones, lo que reduce su potencia."
    )
  })

  # PRIORIZACIÓN 
  output$mapa_prioridad <- renderLeaflet({
    niveles <- names(COLORES_PRIORIDAD)
    pal <- colorFactor(palette = unname(COLORES_PRIORIDAD), levels = niveles)
    leaflet(datos) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(as.character(prioridad)), fillOpacity = 0.85,
        color = "white", weight = 1.5,
        highlightOptions = highlightOptions(weight = 3, color = "#666", bringToFront = TRUE),
        label = ~paste0(LocNombre, ": ", prioridad),
        popup = ~paste0("<b>", LocNombre, "</b><br>", justificacion)
      ) %>%
      addLegend(colors = unname(COLORES_PRIORIDAD), labels = niveles,
                title = "Prioridad", position = "bottomright", opacity = 0.9) %>%
      setView(lng = -74.15, lat = 4.55, zoom = 10)
  })

  output$conteo_prioridad <- renderEcharts4r({
    espacial$prioridades %>%
      count(prioridad, name = "n") %>%
      mutate(prioridad = as.character(prioridad)) %>%
      e_charts(prioridad) %>%
      e_pie(n, radius = c("40%", "70%")) %>%
      e_tooltip() %>%
      e_legend(bottom = 0) %>%
      e_color(unname(COLORES_PRIORIDAD))
  })

  output$tabla_prioridad <- renderDT({
    tabla <- espacial$prioridades %>%
      left_join(
        if (!is.null(perspectiva$alcance_poblacional))
          perspectiva$alcance_poblacional %>% select(LocCodigo, NNA_referencia)
        else espacial$prioridades %>% transmute(LocCodigo, NNA_referencia = NA_real_),
        by = "LocCodigo"
      ) %>%
      transmute(
        Localidad = LocNombre,
        `Índice` = round(indice_vulnerabilidad, 1),
        Nivel = nivel_vulnerabilidad,
        `Dimensión dominante` = dimension_dominante,
        `Cluster LISA` = as.character(cluster),
        `NNA de referencia` = round(NNA_referencia),
        Prioridad = as.character(prioridad)
      )

    datatable(
      tabla, rownames = FALSE,
      options = list(pageLength = 20, scrollX = TRUE, dom = "tip")
    ) %>%
      formatStyle(
        "Prioridad",
        backgroundColor = styleEqual(names(COLORES_PRIORIDAD),
                                     unname(COLORES_PRIORIDAD)),
        color = styleEqual(names(COLORES_PRIORIDAD), c("white", "black", "black"))
      ) %>%
      formatCurrency("NNA de referencia", currency = "", digits = 0, mark = ".")
  })

  output$recomendaciones <- renderUI({
    pr <- espacial$prioridades
    intensificacion <- pr$LocNombre[pr$prioridad == "Intensificación prioritaria"]
    prevencion      <- pr$LocNombre[pr$prioridad == "Prevención reforzada"]

    dom <- pr %>% count(dimension_dominante, name = "n") %>% arrange(desc(n))

    tagList(
      p("Las recomendaciones se formulan como componentes de una arquitectura distrital escalable, graduada por territorio, y se limitan a lo que los resultados respaldan."),
      tags$ol(
        tags$li(
          strong("Priorización territorial de recursos. "),
          "Concentrar la intensidad de la oferta en las ",
          length(intensificacion), " localidades de intensificación prioritaria (",
          paste(sort(intensificacion), collapse = ", "),
          "), manteniendo cobertura universal en el resto de la ciudad."
        ),
        tags$li(
          strong("Fortalecimiento diferencial según perfil. "),
          "El contenido de la intervención debe seguir la dimensión dominante de cada territorio. En el conjunto de la ciudad, la dimensión dominante más frecuente es ",
          strong(dom$dimension_dominante[1]), " (", dom$n[1], " localidades), seguida de ",
          dom$dimension_dominante[2], " (", dom$n[2], "). Un mismo nivel de índice puede requerir respuestas distintas."
        ),
        tags$li(
          strong("Prevención reforzada en la periferia del riesgo. "),
          "Las ", length(prevencion), " localidades de prevención reforzada (",
          paste(sort(prevencion), collapse = ", "),
          ") requieren capacidad instalada anticipatoria antes que respuesta correctiva."
        ),
        tags$li(
          strong("Coordinación intersectorial. "),
          "El diagnóstico muestra que las dimensiones no se mueven juntas: una localidad puede estar bien situada en una dimensión y mal en otra. Eso exige coordinación entre salud, integración social, educación y acceso a la justicia sobre un mismo territorio, en lugar de intervenciones sectoriales independientes."
        ),
        tags$li(
          strong("Fortalecimiento de oferta donde hay brecha. "),
          "El contraste entre índice y oferta instalada, en Panorama distrital, identifica territorios con vulnerabilidad alta y baja oferta por cada 10.000 NNA. Ese cuadrante es el candidato natural a ampliación de capacidad."
        ),
        tags$li(
          strong("Monitoreo diferencial. "),
          "El seguimiento debe reportarse por dimensión y no solo por índice general, porque el índice agregado oculta perfiles muy distintos entre territorios con puntajes similares."
        )
      ),
      div(class = "aviso-limitacion",
          "No se proponen programas específicos que los datos no respalden. Estas son orientaciones de intensidad y contenido derivadas del diagnóstico, no evaluaciones de intervenciones.")
    )
  })

  # PERSPECTIVA Y ALCANCE
  output$texto_perspectiva <- renderUI(p(perspectiva$texto_perspectiva))
  output$texto_alcance     <- renderUI(p(perspectiva$texto_alcance))

  serie_actual <- reactive({
    req(input$serie_seleccionada)
    series_disponibles[[input$serie_seleccionada]]
  })

  serie_filtrada <- reactive({
    s <- serie_actual()
    if (isTRUE(input$incluir_inestables)) s$serie
    else s$serie %>% filter(serie_estable)
  })

  cambio_filtrado <- reactive({
    s <- serie_actual()
    if (isTRUE(input$incluir_inestables)) s$cambio
    else s$cambio %>% filter(serie_estable)
  })

  output$encabezado_serie <- renderUI({
    s <- serie_actual()
    conteo <- table(cambio_filtrado()$direccion)
    n_excluidas <- length(s$localidades_inestables)

    tagList(
      div(
        class = "aviso-limitacion",
        strong(s$etiqueta), ". Fuente: ", s$fuente, ". ",
        "Serie ", min(s$anios), "-", max(s$anios), " (", length(s$anios), " años). ",
        "Entre el primer y el último año, la dirección del cambio fue: ",
        paste(sprintf("%s en %d localidades", names(conteo), as.integer(conteo)),
              collapse = ", "), ". ",
        strong("Es una tendencia observada de este indicador, no un pronóstico del índice.")
      ),
      if (!is.null(s$nota_estabilidad)) div(
        class = "aviso-limitacion",
        strong(
          if (n_excluidas > 0 && !isTRUE(input$incluir_inestables))
            "Series excluidas del gráfico: " else "Nota sobre el denominador: "
        ),
        s$nota_estabilidad
      )
    )
  })

  output$grafico_serie <- renderEcharts4r({
    s <- serie_actual()

    serie_filtrada() %>%
      mutate(valor = round(valor, 2), anio = as.character(anio)) %>%
      arrange(anio) %>%
      group_by(LocNombre) %>%
      e_charts(anio) %>%
      e_line(valor) %>%
      e_tooltip(trigger = "axis") %>%
      e_y_axis(name = s$unidad, scale = TRUE) %>%
      e_x_axis(name = "Año", type = "category", boundaryGap = FALSE) %>%
      e_legend(type = "scroll", bottom = 0) %>%
      e_datazoom(x_index = 0)
  })

  output$grafico_cambio <- renderEcharts4r({
    s <- serie_actual()
    cambio_filtrado() %>%
      mutate(cambio_absoluto = round(cambio_absoluto, 2)) %>%
      arrange(cambio_absoluto) %>%
      mutate(LocNombre = factor(LocNombre, levels = LocNombre)) %>%
      e_charts(LocNombre) %>%
      e_bar(cambio_absoluto, name = paste0("Cambio ", min(s$anios), "-", max(s$anios))) %>%
      e_flip_coords() %>%
      e_tooltip() %>%
      e_visual_map(cambio_absoluto, show = FALSE,
                   inRange = list(color = c("#2166AC", "#F7F7F7", "#B2182B")))
  })


  output$tabla_cambio <- renderDT({
    s <- serie_actual()
    tabla <- s$cambio %>%
      transmute(
        Localidad = LocNombre,
        !!paste0("Valor ", min(s$anios)) := round(inicio, 2),
        !!paste0("Valor ", max(s$anios)) := round(fin, 2),
        `Cambio absoluto` = round(cambio_absoluto, 2),
        `Cambio relativo (%)` = round(cambio_relativo, 1),
        Dirección = direccion,
        Serie = ifelse(serie_estable, "Estable", "Inestable (denominador pequeño)")
      ) %>%
      arrange(`Cambio absoluto`)

    datatable(tabla, rownames = FALSE,
              options = list(pageLength = 20, scrollX = TRUE, dom = "tip")) %>%
      formatStyle(
        "Serie",
        target = "row",
        backgroundColor = styleEqual("Inestable (denominador pequeño)", "#FFF6E5")
      )
  })

  output$tabla_inventario <- renderTable({
    perspectiva$inventario_series %>%
      transmute(
        Fuente = fuente,
        Archivo = archivo,
        Disponible = ifelse(disponible, "Sí", "No"),
        `Años` = n_anios,
        `Desde` = anio_min,
        `Hasta` = anio_max,
        `Códigos de localidad` = n_localidades,
        `Serie continua` = ifelse(continua, "Sí", "No"),
        `Usada en el visor` = ifelse(usada_en_visor, "Sí", "No")
      )
  }, digits = 0)

  output$grafico_alcance <- renderEcharts4r({
    req(perspectiva$alcance_poblacional)
    perspectiva$alcance_poblacional %>%
      mutate(
        tooltip = paste0(
          LocNombre,
          "<br>NNA de referencia: ", formato_miles(NNA_referencia),
          "<br>Índice de vulnerabilidad: ", round(indice_vulnerabilidad, 1),
          "<br>Dimensión dominante: ", dimension_dominante
        )
      ) %>%
      e_charts(indice_vulnerabilidad) %>%
      e_scatter(NNA_referencia, bind = tooltip, symbol_size = 14) %>%
      e_x_axis(name = "Índice de vulnerabilidad", min = 0, max = 100) %>%
      e_y_axis(name = "NNA de referencia") %>%
      e_tooltip() %>%
      e_color("#B2182B") %>%
      e_legend(show = FALSE) %>%
      e_title("Vulnerabilidad relativa frente a alcance poblacional")
  })

  output$tabla_alcance <- renderDT({
    req(perspectiva$alcance_poblacional)
    tabla <- perspectiva$alcance_poblacional %>%
      transmute(
        Localidad = LocNombre,
        `NNA de referencia` = round(NNA_referencia),
        `% de los NNA de la ciudad` = round(participacion_ciudad, 1),
        `Índice de vulnerabilidad` = round(indice_vulnerabilidad, 1),
        `Dimensión dominante` = dimension_dominante
      )
    datatable(tabla, rownames = FALSE,
              options = list(pageLength = 20, scrollX = TRUE, dom = "tip")) %>%
      formatCurrency("NNA de referencia", currency = "", digits = 0, mark = ".")
  })

  # METODOLOGÍA 
  output$tabla_fuentes <- renderTable({
    data.frame(
      Fuente = c(
        "Encuesta Multipropósito de Bogotá",
        "Observatorio de Salud de Bogotá - mortalidad infantil",
        "Observatorio de Salud de Bogotá - malnutrición menores de 5 años",
        "Observatorio de Salud de Bogotá - malnutrición 5 a 17 años",
        "Observatorio de Salud de Bogotá - violencia intrafamiliar",
        "Infraestructura de protección social (10 capas)",
        "Colegios oficiales y beneficiarios escolares",
        "Tasas oficiales de aprobación, deserción y reprobación",
        "Anexo de población por localidad",
        "Cartografía de localidades"
      ),
      `Uso en el proyecto` = c(
        "Afiliación en salud de NNA; indicadores laborales, de ingreso y de consumo alimentario de hogares con NNA",
        "Tasa de mortalidad infantil por localidad y serie histórica",
        "Riesgo de desnutrición global y desnutrición aguda; serie histórica",
        "Sobrepeso, obesidad y delgadez en población escolar",
        "Casos de violencia contra NNA, base de la tasa por 10.000 NNA",
        "Conteo y tasa de centros de protección por 10.000 NNA",
        "Capa de oferta institucional, fuera del índice",
        "Dimensión de educación (deserción y reprobación)",
        "Denominador de todas las tasas por 10.000 NNA y población de referencia",
        "Geometría de las 20 localidades y matriz de contigüidad reina"
      ),
      Tipo = c("Microdatos", "Registro administrativo", "Registro administrativo",
               "Registro administrativo", "Registro administrativo",
               "Cartografía", "Cartografía", "Cartografía",
               "Estadística oficial", "Cartografía"),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  })

  output$tabla_variables_dimension <- renderTable({
    detalle <- acp_general$detalle_dimensiones
    bind_rows(lapply(names(detalle), function(d) {
      x <- detalle[[d]]
      data.frame(
        Dimensión = d,
        `Variables en el índice` = paste(x$variables, collapse = ", "),
        `Invertidas (protectoras)` = if (length(x$protectoras) > 0)
          paste(x$protectoras, collapse = ", ") else "ninguna",
        `Varianza del CP1 (diagnóstico)` = paste0(round(x$varianza_cp1 * 100, 1), "%"),
        check.names = FALSE, stringsAsFactors = FALSE
      )
    }))
  })

  # LIMITACIONES 
  output$lista_limitaciones <- renderUI({
    tags$ol(
      tags$li(strong("N = 20 localidades. "),
              "El tamaño de muestra territorial es reducido y limita la estabilidad de cualquier técnica multivariada y del análisis espacial. Con 20 unidades y un promedio de ",
              sprintf("%.1f", espacial$promedio_vecinas),
              " vecinas por localidad, el estadístico local de Moran tiene poca potencia."),
      tags$li(strong("Diseño transversal. "),
              "El índice describe la situación territorial en el periodo de referencia y no permite inferir trayectorias futuras del índice."),
      tags$li(strong("No existe modelo predictivo del índice compuesto. "),
              "No es estadísticamente defendible construirlo con 20 observaciones territoriales y sin serie temporal del índice. El componente de perspectiva muestra tendencias históricas de indicadores concretos, no un pronóstico."),
      tags$li(strong("Población de referencia, no proyección propia. "),
              "El denominador de las tasas por 10.000 NNA proviene del anexo oficial de población por localidad. Se reporta como población de referencia del año utilizado y no se presenta ninguna cifra futura ni estimación elaborada por el equipo."),
      tags$li(strong("LISA exploratorio. "),
              "Se utiliza p < 0,05 sin corrección por comparaciones múltiples. Con 20 pruebas simultáneas la tasa de falsos positivos es mayor que el nivel nominal."),
      tags$li(strong("Orientación del ACP y decisión de ponderación. "),
              "La orientación de los componentes requirió una decisión conceptual explícita. Al aplicarla, el primer componente dejó de comportarse como un factor común de vulnerabilidad, por lo que el índice se construye con pesos iguales sobre puntajes z y el ACP se reporta como diagnóstico. La decisión y su justificación están documentadas en Metodología."),
      tags$li(strong("Redundancia composicional. "),
              "Se excluyeron variables que son el complemento aritmético de otras del mismo bloque, porque generaban correlaciones artificiales que distorsionaban la estructura de los componentes."),
      tags$li(strong("Oferta pública focalizada. "),
              "Las tasas de colegios oficiales, beneficios escolares y centros de protección por 10.000 NNA no entran al índice porque son más altas en las localidades más pobres: miden respuesta institucional, no necesidad. Se reportan como capa separada. Su relación con la vulnerabilidad no implica causalidad en ninguna dirección."),
      tags$li(strong("Educación mide solo el sector oficial. "),
              "Las tasas de deserción y reprobación corresponden únicamente a la matrícula oficial. En localidades donde la mayoría de NNA asiste a colegios privados, la matrícula oficial es pequeña y no representa a la población residente. Esto explica que la dimensión de educación correlacione negativamente con las demás y debe tenerse en cuenta al leer esa dimensión."),
      tags$li(strong("Falacia ecológica. "),
              "Los resultados corresponden a localidades y no permiten inferir características de individuos u hogares concretos."),
      tags$li(strong("Asociación no implica causalidad. "),
              "El índice, las correlaciones entre dimensiones y los patrones espaciales son herramientas descriptivas y de priorización. No son estimaciones de efectos causales ni evaluaciones de intervenciones."),
      tags$li(strong("Reproducibilidad parcial de la etapa de microdatos. "),
              "Las dimensiones de salud y economía se derivan de capítulos de la Encuesta Multipropósito que deben estar presentes en data/raw/. Cuando no lo están, el pipeline reutiliza la capa de insumos ya calculada en data/processed/ y lo informa explícitamente, en lugar de imputar o simular valores.")
    )
  })
}

shinyApp(ui, server)
