#Datos

setwd("C:/Users/manue/OneDrive/Escritorio/a borrar/DataJam")

source("scripts/01_carga_datos.R")

#Tratamiento

malnutricion5anos = osb_malnutricion5agnos %>% filter(AÑO == 2026)

malnutricion5_17anos = osb_malnutricion5_17anos %>% filter(ANIO == 2026)

mortalidadinf = osb_tm_infantil %>% filter(ANIO == 2026)

codigos_localidad <- osb_malnutricion5_17anos %>%
  select(
    CODIGO_lOCALIDAD,
    LOCALIDAD_RESIDENCIA
  ) %>%
  distinct()

base <- salud %>%
  left_join(
    demografia %>%
      select(DIRECTORIO_PER, NPCEP4),
    by = "DIRECTORIO_PER"
  )


base = base %>%
  left_join(
    identificacion %>%
      select(DIRECTORIO, COD_LOCALIDAD, NOMBRE_LOCALIDAD),
    by = "DIRECTORIO"
  )


baseNNA <- base %>%
  filter(
    !is.na(NPCEP4),
    NPCEP4 <= 17,
    !is.na(COD_LOCALIDAD)
  )

baseNNA <- baseNNA %>%
  mutate(
    NOMBRE_LOCALIDAD = str_replace_all(NOMBRE_LOCALIDAD, "\\\\xf3", "ó"),
    NOMBRE_LOCALIDAD = str_replace_all(NOMBRE_LOCALIDAD, "\\\\xed", "í"),
    NOMBRE_LOCALIDAD = str_replace_all(NOMBRE_LOCALIDAD, "\\\\xf1", "ñ"),
    NOMBRE_LOCALIDAD = str_replace_all(NOMBRE_LOCALIDAD, "\\\\xe1", "á")
  )


prop_ponderada <- function(x, peso, categoria = 1) {
  
  validos <- !is.na(x) & !is.na(peso)
  
  sum(peso[validos] * (x[validos] == categoria)) /
    sum(peso[validos]) * 100
}

prop_ponderada(
  base$NPCFP1,
  base$FEX_C,
  categoria = 1
)


acceso_salud <- baseNNA %>%
  filter(
    !is.na(NOMBRE_LOCALIDAD),
    !is.na(NPCFP1),
    !is.na(FEX_C)
  ) %>%
  group_by(NOMBRE_LOCALIDAD) %>%
  summarise(
    poblacion_estimada = sum(FEX_C),
    afiliados_estimados = sum(FEX_C * (NPCFP1 == 1)),
    no_afiliados_estimados = sum(FEX_C * (NPCFP1 == 2)),
    porcentaje_afiliado =
      afiliados_estimados / 
      (afiliados_estimados + no_afiliados_estimados) * 100,
    .groups = "drop"
  ) %>% print(n = 1000)



acceso_salud <- acceso_salud %>%
  mutate(
    NOMBRE_LOCALIDAD = case_when(
      NOMBRE_LOCALIDAD == "Antonio Nari�o" ~ "Antonio Nariño",
      NOMBRE_LOCALIDAD == "Ciudad Bol�var" ~ "Ciudad Bolívar",
      NOMBRE_LOCALIDAD == "Engativ�" ~ "Engativá",
      NOMBRE_LOCALIDAD == "Fontib�n" ~ "Fontibón",
      NOMBRE_LOCALIDAD == "Los M�rtires" ~ "Los Mártires",
      NOMBRE_LOCALIDAD == "San Crist�bal" ~ "San Cristóbal",
      NOMBRE_LOCALIDAD == "Usaqu�n" ~ "Usaquén",
      TRUE ~ NOMBRE_LOCALIDAD
    )
  )

acceso_salud <- acceso_salud %>%
  left_join(
    codigos_localidad,
    by = c("NOMBRE_LOCALIDAD" = "LOCALIDAD_RESIDENCIA")
  )


nutricion_5 <- malnutricion5anos %>%
  select(
    CODIGO_lOCALIDAD,
    PROPORCION_PESO_ADECUADO_PARA_LA_EDAD,
    PROPORCION_RIESGO_DNT_GLOBAL,
    PROPORCION_DESNUTRICION_AGUDA_MODERADA,
    PROPORCION_DESNUTRICION_AGUDA_SEVERA
  )


nutricion_5_17 <- malnutricion5_17anos %>%
  select(
    CODIGO_lOCALIDAD,
    PROPORCION_IMC_ADECUADO,
    PROPORCION_SOBREPESO,
    PROPORCION_OBESIDAD,
    PROPORCION_DELGADEZ
  )



infantil_2026 <- osb_tm_infantil %>%
  filter(ANIO == 2026) %>%
  mutate(
    asegurado = if_else(
      REGIMEN.SEGURIDAD.SOCIAL == "No Asegurado",
      0,
      1
    )
  )

mortalidad_localidad <- infantil_2026 %>%
  group_by(`CÓDIGO.LOCALIDAD`, LOCALIDAD) %>%
  summarise(
    casos = sum(CASOS, na.rm = TRUE),
    NACIDOS.VIVOS = sum(NACIDOS.VIVOS, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    tasa_mortalidad = casos / NACIDOS.VIVOS * 1000
  )

aseguramiento_localidad <- infantil_2026 %>%
  group_by(`CÓDIGO.LOCALIDAD`, LOCALIDAD) %>%
  summarise(
    casos_asegurados = sum(CASOS[asegurado == 1], na.rm = TRUE),
    casos_totales = sum(CASOS, na.rm = TRUE),
    proporcion_asegurada = casos_asegurados / casos_totales,
    .groups = "drop"
  )


salud_infantil <- mortalidad_localidad %>%
  left_join(
    aseguramiento_localidad %>%
      select(
        `CÓDIGO.LOCALIDAD`,
        proporcion_asegurada
      ),
    by = "CÓDIGO.LOCALIDAD"
  )


#ACP

datos_salud <- acceso_salud %>%
  select(
    CODIGO_lOCALIDAD,
    porcentaje_afiliado
  ) %>%
  left_join(
    salud_infantil %>%
      select(
        `CÓDIGO.LOCALIDAD`,
        tasa_mortalidad
      ),
    by = c("CODIGO_lOCALIDAD" = "CÓDIGO.LOCALIDAD")
  ) %>%
  left_join(
    nutricion_5,
    by = c("CODIGO_lOCALIDAD" = "CODIGO_lOCALIDAD")
  ) %>%
  left_join(
    nutricion_5_17,
    by = c("CODIGO_lOCALIDAD" = "CODIGO_lOCALIDAD")
  )


salud <- datos_salud %>%
  mutate(
    LocCodigo = sprintf("%02d", as.integer(CODIGO_lOCALIDAD))
  )


datos_acp <- datos_salud %>%
  select(
    porcentaje_afiliado,
    tasa_mortalidad,
    PROPORCION_PESO_ADECUADO_PARA_LA_EDAD,
    PROPORCION_RIESGO_DNT_GLOBAL,
    PROPORCION_DESNUTRICION_AGUDA_MODERADA,
    PROPORCION_DESNUTRICION_AGUDA_SEVERA,
    PROPORCION_IMC_ADECUADO,
    PROPORCION_SOBREPESO,
    PROPORCION_OBESIDAD,
    PROPORCION_DELGADEZ
  )


acp_salud <- prcomp(
  datos_acp,
  center = TRUE,
  scale. = TRUE
)

indice_salud <- acp_salud$x[,1]

salud <- salud %>%
  mutate(
    indice_salud = acp_salud$x[,1]
  )

# Normalización 0-100

salud <- salud %>%
  mutate(
    indice_salud_100 = 
      (indice_salud - min(indice_salud, na.rm = TRUE)) /
      (max(indice_salud, na.rm = TRUE) - min(indice_salud, na.rm = TRUE)) * 100
  )

#Guardar

saveRDS(
  salud,
  "data/processed/salud.rds"
)
