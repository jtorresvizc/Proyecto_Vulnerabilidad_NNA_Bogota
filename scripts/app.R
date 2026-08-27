# ======================================================
# LIBRERÍAS
# ======================================================
library(shiny)
library(bs4Dash)
library(leaflet)
library(echarts4r)
library(dplyr)
library(tidyr)
library(sf)
library(purrr)

# ======================================================
# BASE FINAL DE INDICADORES 
# ======================================================

setwd("C:/Users/manue/OneDrive/Escritorio/a borrar/DataJam")

source("scripts/06_indice_general.R")

base_final <- readRDS(
  "data/processed/vulnerabilidad.rds"
)


base_final <- base_final %>%
  mutate(LocCodigo = sprintf("%02d", as.integer(LocCodigo)))

# ======================================================
# OBJETO ESPACIAL PRINCIPAL
# ======================================================
datos <- loca %>%
  left_join(base_final, by = "LocCodigo") %>%
  filter(!is.na(indice_vulnerabilidad))

# ======================================================
# NOMBRES DE LOS INDICADORES
# ======================================================
titulos_indicadores <- c(
  indice_vulnerabilidad = "Índice de vulnerabilidad",
  indice_economico      = "Índice económico",
  indice_salud_100      = "Índice de salud",
  indice_proteccion_100 = "Índice de protección",
  indice_educacion_100  = "Índice de educación"
)

# ======================================================
# PROMEDIO GENERAL DE BOGOTÁ
# ======================================================
perfil_bogota <- datos %>%
  st_drop_geometry() %>%
  summarise(
    Salud         = mean(indice_salud_100, na.rm = TRUE),
    Protección    = mean(indice_proteccion_100, na.rm = TRUE),
    Economía      = mean(indice_economico, na.rm = TRUE),
    Educación     = mean(indice_educacion_100, na.rm = TRUE),
    Vulnerabilidad = mean(indice_vulnerabilidad, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "indicador", values_to = "valor")

# ======================================================
# RANKINGS PRECALCULADOS
# ======================================================
variables_indicadores <- names(titulos_indicadores)

rankings_precalculados <- setNames(
  lapply(variables_indicadores, function(variable) {
    ranking <- datos %>%
      st_drop_geometry() %>%
      select(LocNombre, valor = all_of(variable)) %>%
      arrange(desc(valor))
    
    ranking$LocNombre <- factor(ranking$LocNombre, levels = rev(ranking$LocNombre))
    ranking
  }),
  variables_indicadores
)

# ======================================================
# COMPARACIONES ENTRE INDICADORES
# ======================================================
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
      comparacion <- datos %>%
        st_drop_geometry() %>%
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

# ======================================================
# MATRIZ DE PERFILES TERRITORIALES
# ======================================================
orden_localidades_panorama <- datos %>%
  st_drop_geometry() %>%
  arrange(indice_vulnerabilidad) %>%
  pull(LocNombre)

matriz_perfiles_precalculada <- datos %>%
  st_drop_geometry() %>%
  select(
    LocNombre,
    Economía      = indice_economico,
    Salud         = indice_salud_100,
    Protección    = indice_proteccion_100,
    Educación     = indice_educacion_100,
    Vulnerabilidad = indice_vulnerabilidad
  ) %>%
  pivot_longer(-LocNombre, names_to = "indicador", values_to = "valor") %>%
  mutate(
    LocNombre = factor(LocNombre, levels = orden_localidades_panorama),
    indicador = factor(indicador, levels = c("Economía", "Salud", "Protección", "Educación", "Vulnerabilidad"))
  )

# ======================================================
# BRECHAS FRENTE AL PROMEDIO
# ======================================================
promedio_indice_general <- mean(datos$indice_vulnerabilidad, na.rm = TRUE)
cuartiles_indice_general <- quantile(datos$indice_vulnerabilidad, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)

brechas_promedio_precalculadas <- datos %>%
  st_drop_geometry() %>%
  transmute(
    LocNombre,
    indice_vulnerabilidad,
    brecha_promedio = indice_vulnerabilidad - promedio_indice_general
  ) %>%
  mutate(
    prioridad = case_when(
      indice_vulnerabilidad >= cuartiles_indice_general[3] ~ "Prioridad alta",
      indice_vulnerabilidad >= cuartiles_indice_general[2] ~ "Prioridad media-alta",
      indice_vulnerabilidad >= cuartiles_indice_general[1] ~ "Prioridad media-baja",
      TRUE ~ "Prioridad baja"
    ),
    tooltip = paste0(
      LocNombre,
      "<br>Índice: ", round(indice_vulnerabilidad, 1),
      "<br>Brecha frente al promedio: ",
      ifelse(brecha_promedio >= 0, "+", ""),
      round(brecha_promedio, 1),
      "<br>", prioridad
    )
  )

prioridad_alta_precalculada <- brechas_promedio_precalculadas %>%
  filter(prioridad == "Prioridad alta") %>%
  arrange(desc(indice_vulnerabilidad)) %>%
  transmute(
    Localidad = LocNombre,
    `Índice de vulnerabilidad` = indice_vulnerabilidad,
    `Brecha frente al promedio` = brecha_promedio
  )

# ======================================================
# UI
# ======================================================
ui <- bs4DashPage(
  title = "Vulnerabilidad Infantil Bogotá",
  dark = FALSE,
  
  header = bs4DashNavbar(title = "Vulnerabilidad Infantil Bogotá"),
  
  sidebar = bs4DashSidebar(
    width = 270,
    collapsed = FALSE,
    minified = TRUE,
    expandOnHover = TRUE,
    skin = "dark",
    status = "danger",
    
    bs4SidebarMenu(
      id = "menu_principal",
      bs4SidebarMenuItem("Descripción del visor", tabName = "descripcion", icon = icon("info-circle"), selected = TRUE),
      bs4SidebarMenuItem("Visor territorial", tabName = "visor", icon = icon("map")),
      bs4SidebarMenuItem("Panorama distrital", tabName = "panorama", icon = icon("chart-bar")),
      bs4SidebarMenuItem("Metodología", tabName = "metodologia", icon = icon("cogs"))
    )
  ),
  
  body = bs4DashBody(
    tags$head(
      tags$style(HTML("
        .leaflet-container { height: 650px !important; background: #fff; }
        .card { height: 100%; }
        .introduccion-visor { color: #343a40; line-height: 1.55; }
        .titulo-pestana { margin: 4px 0 16px 0; }
        .tabla-desplazable { width: 100%; overflow-x: auto; }
        .tabla-desplazable table { min-width: 720px; white-space: nowrap; }
        .escala-indice {
          background: linear-gradient(90deg, #FFFFBF, #FDAE61, #B2182B);
          border-radius: 8px; display: flex; justify-content: space-between;
          padding: 12px; font-weight: 600;
        }
        .flujo-indice { display: flex; justify-content: center; align-items: center; gap: 10px; flex-wrap: wrap; }
        .dimension-indice {
          background: #FDEDEC; border: 1px solid #E6B0AA; border-radius: 8px;
          padding: 10px 15px; font-weight: 600;
        }
        .indice-general {
          background: #B2182B; color: white; border-radius: 8px;
          padding: 10px 15px; font-weight: 600;
        }
        .operador-indice { font-size: 20px; font-weight: bold; }
        .pregunta-investigacion {
          margin-top: 18px; padding: 14px 18px; background: #f8f9fa;
          border-left: 4px solid #B2182B; font-weight: 600; border-radius: 4px;
        }
      "))
    ),
    
    bs4TabItems(
      # ---------- DESCRIPCIÓN ----------
      bs4TabItem(
        tabName = "descripcion",
        h2(class = "titulo-pestana", "Descripción del visor"),
        
        fluidRow(
          bs4Card(
            width = 7, title = "Problema territorial",
            div(
              class = "introduccion-visor",
              p("Las condiciones asociadas al bienestar de niños, niñas y adolescentes presentan diferencias importantes entre localidades de Bogotá. Cada territorio puede concentrar dificultades económicas, educativas, sanitarias o relacionadas con protección."),
              p("El visor integra diferentes dimensiones en una escala común para facilitar la comparación territorial e identificar patrones de vulnerabilidad."),
              div(class = "pregunta-investigacion",
                  "¿Qué perfiles territoriales de vulnerabilidad infantil pueden identificarse en Bogotá y cómo se distribuyen espacialmente?")
            )
          ),
          bs4Card(
            width = 5, title = "¿Qué permite hacer?",
            tags$ul(
              tags$li("Comparar localidades en una escala común de 0 a 100."),
              tags$li("Identificar territorios con mayor vulnerabilidad relativa."),
              tags$li("Analizar dimensiones económicas, salud, protección y educación."),
              tags$li("Explorar relaciones entre indicadores."),
              tags$li("Apoyar la priorización territorial.")
            ),
            div(class = "escala-indice",
                span("0 · Menor vulnerabilidad"),
                span("100 · Mayor vulnerabilidad"))
          )
        ),
        
        fluidRow(
          bs4Card(
            width = 12, title = "Construcción del índice de vulnerabilidad",
            div(
              class = "flujo-indice",
              span(class = "dimension-indice", "Economía"),
              span(class = "operador-indice", "+"),
              span(class = "dimension-indice", "Salud"),
              span(class = "operador-indice", "+"),
              span(class = "dimension-indice", "Protección"),
              span(class = "operador-indice", "+"),
              span(class = "dimension-indice", "Educación"),
              span(class = "operador-indice", "→"),
              span(class = "indice-general", "Índice de vulnerabilidad")
            )
          )
        )
      ),
      
      # ---------- VISOR TERRITORIAL ----------
      bs4TabItem(
        tabName = "visor",
        h2(class = "titulo-pestana", "Visor territorial por localidad"),
        
        fluidRow(
          bs4Card(
            width = 6, title = "Mapa de localidades",
            selectInput(
              "indicador_mapa",
              "Seleccionar indicador:",
              choices = c(
                "Índice de Vulnerabilidad" = "indice_vulnerabilidad",
                "Economía"                 = "indice_economico",
                "Salud"                    = "indice_salud_100",
                "Protección"               = "indice_proteccion_100",
                "Educación"                = "indice_educacion_100"
              )
            ),
            leafletOutput("mapa_localidades", height = "650px")
          ),
          
          column(
            width = 6,
            bs4Card(
              width = 12, title = "Perfil de la localidad",
              h4(textOutput("nombre_localidad")),
              echarts4rOutput("perfil_localidad", height = "280px"),
              div(class = "tabla-desplazable", tableOutput("tabla_localidad"))
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
            echarts4rOutput("comparacion_indicadores", height = "420px")
          )
        )
      ),
      
      # ---------- PANORAMA ----------
      bs4TabItem(
        tabName = "panorama",
        h2(class = "titulo-pestana", "Panorama distrital"),
        
        bs4Card(
          width = 12, title = "Brecha frente al promedio de localidades",
          echarts4rOutput("brechas_promedio", height = "620px"),
          h5("Localidades de prioridad alta relativa"),
          tableOutput("tabla_prioridad_alta")
        ),
        
        bs4Card(
          width = 12, title = "Perfiles de vulnerabilidad por localidad",
          echarts4rOutput("mapa_calor_perfiles", height = "650px")
        )
      ),
      
      # ---------- METODOLOGÍA ----------
      bs4TabItem(
        tabName = "metodologia",
        h2(class = "titulo-pestana", "Metodología e interpretación"),
        
        bs4Card(
          width = 12, title = "Ruta analítica",
          tags$ol(
            tags$li("Construcción de indicadores territoriales."),
            tags$li("Normalización en escala 0-100."),
            tags$li("Integración de dimensiones mediante índices sintéticos."),
            tags$li("Comparación espacial entre localidades."),
            tags$li("Identificación de brechas territoriales.")
          )
        ),
        
        bs4Card(
          width = 12, title = "Interpretación",
          p("Los resultados representan comparaciones entre localidades y no corresponden a probabilidades individuales."),
          div(class = "escala-indice",
              span("0 · Menor"),
              span("100 · Mayor"))
        )
      )
    )
  )
)

# ======================================================
# SERVER
# ======================================================
server <- function(input, output, session) {
  
  localidad_seleccionada <- reactiveVal(NULL)
  
  # Actualizar opciones de comparación
  observeEvent(input$indicador_mapa, {
    req(input$indicador_mapa)
    
    disponibles <- setdiff(names(titulos_indicadores), input$indicador_mapa)
    seleccion_actual <- isolate(input$indicador_comparacion)
    
    seleccion <- if (!is.null(seleccion_actual) && seleccion_actual %in% disponibles) {
      seleccion_actual
    } else {
      disponibles[1]
    }
    
    updateSelectInput(
      session, "indicador_comparacion",
      choices = setNames(disponibles, unname(titulos_indicadores[disponibles])),
      selected = seleccion
    )
  }, ignoreInit = FALSE)
  
  # ---------- MAPA ----------
  output$mapa_localidades <- renderLeaflet({
    req(input$indicador_mapa)
    
    datos_mapa <- datos %>% mutate(valor = .data[[input$indicador_mapa]])
    
    pal <- colorNumeric(
      palette = c("#FFFFBF", "#FEE08B", "#FDAE61", "#F46D43", "#D73027", "#B2182B"),
      domain = c(0, 100),
      na.color = "#808080"
    )
    
    leaflet(datos_mapa) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        layerId = ~LocCodigo,
        fillColor = ~pal(valor),
        fillOpacity = 0.85,
        color = "white",
        weight = 1.5,
        highlightOptions = highlightOptions(weight = 3, color = "#666", bringToFront = TRUE),
        label = ~paste0(LocNombre, ": ", round(valor, 1)),
        popup = ~paste0(
          "<b>", LocNombre, "</b><br>",
          titulos_indicadores[input$indicador_mapa], ": ", round(valor, 2)
        )
      ) %>%
      addLegend(
        pal = pal, values = ~valor,
        title = titulos_indicadores[input$indicador_mapa],
        position = "bottomright"
      ) %>%
      setView(lng = -74.08, lat = 4.65, zoom = 11)
  })
  
  observeEvent(input$mapa_localidades_shape_click, {
    click <- input$mapa_localidades_shape_click
    if (!is.null(click$id)) localidad_seleccionada(click$id)
  })
  
  seleccionado <- reactive({
    req(localidad_seleccionada())
    datos %>% filter(LocCodigo == localidad_seleccionada())
  })
  
  # ---------- NOMBRE ----------
  output$nombre_localidad <- renderText({
    if (is.null(localidad_seleccionada())) {
      "Haz clic en una localidad del mapa"
    } else {
      seleccionado()$LocNombre
    }
  })
  
  # ---------- PERFIL ----------
  output$perfil_localidad <- renderEcharts4r({
    if (is.null(localidad_seleccionada()) || nrow(seleccionado()) == 0) {
      df <- perfil_bogota
      titulo <- "Promedio Bogotá"
    } else {
      x <- seleccionado()
      df <- data.frame(
        indicador = c("Salud", "Protección", "Economía", "Educación", "Vulnerabilidad"),
        valor = c(
          x$indice_salud_100,
          x$indice_proteccion_100,
          x$indice_economico,
          x$indice_educacion_100,
          x$indice_vulnerabilidad
        )
      )
      titulo <- x$LocNombre
    }
    
    df %>%
      e_charts(indicador) %>%
      e_bar(valor, name = titulo) %>%
      e_y_axis(max = 100) %>%
      e_tooltip(trigger = "axis") %>%
      e_title(titulo)
  })
  
  # ---------- TABLA (ahora también muestra promedio cuando no hay selección) ----------
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
          Localidad = LocNombre,
          Economía = indice_economico,
          Salud = indice_salud_100,
          Protección = indice_proteccion_100,
          Educación = indice_educacion_100,
          Vulnerabilidad = indice_vulnerabilidad
        )
    }
  }, digits = 1)
  
  # ---------- RANKING ----------
  output$ranking_localidades <- renderEcharts4r({
    req(input$indicador_mapa)
    
    rankings_precalculados[[input$indicador_mapa]] %>%
      e_charts(LocNombre) %>%
      e_bar(valor, name = titulos_indicadores[input$indicador_mapa]) %>%
      e_flip_coords() %>%
      e_tooltip() %>%
      e_color("#B2182B")
  })
  
  # ---------- COMPARACIÓN ----------
  output$comparacion_indicadores <- renderEcharts4r({
    req(input$indicador_mapa, input$indicador_comparacion)
    validate(need(input$indicador_mapa != input$indicador_comparacion, "Seleccione dos indicadores diferentes"))
    
    clave <- paste(input$indicador_mapa, input$indicador_comparacion, sep = "__")
    datos_comp <- comparaciones_precalculadas[[clave]]
    
    datos_comp %>%
      e_charts(valor_x) %>%
      e_scatter(valor_y, bind = tooltip, symbol_size = 12) %>%
      e_line(ajuste, name = "Regresión lineal", symbol = "none") %>%
      e_x_axis(min = 0, max = 100, name = titulos_indicadores[input$indicador_mapa]) %>%
      e_y_axis(min = 0, max = 100, name = titulos_indicadores[input$indicador_comparacion]) %>%
      e_tooltip()
  })
  
  # ---------- MAPA DE CALOR ----------
  output$mapa_calor_perfiles <- renderEcharts4r({
    matriz_perfiles_precalculada %>%
      e_charts(indicador) %>%
      e_heatmap(LocNombre, valor) %>%
      e_visual_map(valor, min = 0, max = 100, orient = "horizontal") %>%
      e_tooltip()
  })
  
  # ---------- BRECHAS ----------
  output$brechas_promedio <- renderEcharts4r({
    brechas_promedio_precalculadas %>%
      e_charts(LocNombre) %>%
      e_bar(brecha_promedio, bind = tooltip) %>%
      e_flip_coords() %>%
      e_tooltip()
  })
  
  # ---------- PRIORIDAD ALTA ----------
  output$tabla_prioridad_alta <- renderTable({
    prioridad_alta_precalculada
  }, digits = 1)
}

shinyApp(ui, server)