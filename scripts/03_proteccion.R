
#Datos

setwd("C:/Users/manue/OneDrive/Escritorio/a borrar/DataJam")

source("scripts/01_carga_datos.R")

#Tratamiento

localidades <- data.frame(
  LocCodigo = 1:20
)

localidades_unicas <- localidades %>%
  distinct(LocCodigo, .keep_all = TRUE)

todos <- bind_rows(centro_proteger, espacios_rurales, jardin_infantil_nocturno, centro_renacer, centro_abrazar,
                   jardin_infantil, centro_amar,centro_crecer, casa_pensamiento_intercultural, comisarias_familia
)

codigos_localidad <- osb_malnutricion5_17anos %>%
  select(
    CODIGO_lOCALIDAD,
    LOCALIDAD_RESIDENCIA
  ) %>%
  distinct()

matriz_centros <- localidades_unicas %>%
  st_drop_geometry() %>%
  select(LocCodigo) %>%
  left_join(
    todos %>%
      st_drop_geometry() %>%
      count(LocCodigo, OSSSimbol),
    by = "LocCodigo"
  ) %>%
  mutate(n = replace_na(n, 0)) %>%
  pivot_wider(
    names_from = OSSSimbol,
    values_from = n,
    values_fill = 0
  )


violencia <- violencia %>%
  left_join(
    codigos_localidad,
    by = c("NOMBRE_LOCALIDAD" = "LOCALIDAD_RESIDENCIA")
  ) %>%
  filter(
    grupoedad %in% c(
      "Menor 1 año",
      "De 1 - 5 años",
      "De 6 - 13 años",
      "De 14 - 17 años"
    )) %>%
  filter(ano == 2026)


violencia_localidad <- violencia %>%
  group_by(CODIGO_lOCALIDAD) %>%
  summarise(
    casos_violencia = n(),
    .groups = "drop"
  ) %>% filter(
    CODIGO_lOCALIDAD %in% c(1:20)
  )


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
  )



tasa_violencia <- violencia_localidad %>%
  mutate(
    CODIGO_lOCALIDAD = sprintf("%02d", CODIGO_lOCALIDAD)
  ) %>%
  left_join(
    poblacion2026 %>%
      select(COD_LOC, NNA),
    by = c("CODIGO_lOCALIDAD" = "COD_LOC")
  ) %>%
  mutate(
    tasa_violencia = casos_violencia / NNA * 10000
  )

matriz_centros <- localidades_unicas %>%
  st_drop_geometry() %>%
  select(LocCodigo) %>%
  mutate(
    LocCodigo = as.numeric(LocCodigo)
  ) %>%
  left_join(
    todos %>%
      st_drop_geometry() %>%
      mutate(
        LocCodigo = as.numeric(LocCodigo)
      ) %>%
      count(LocCodigo, OSSSimbol),
    by = "LocCodigo"
  ) %>%
  mutate(n = replace_na(n, 0)) %>%
  pivot_wider(
    names_from = OSSSimbol,
    values_from = n,
    values_fill = 0
  )


matriz_centros <- matriz_centros %>%
  left_join(
    poblacion2026 %>%
      mutate(
        COD_LOC = as.numeric(COD_LOC)
      ) %>%
      select(COD_LOC, NNA),
    by = c("LocCodigo" = "COD_LOC")
  )


tasa_centros <- matriz_centros %>%
  mutate(
    across(
      where(is.numeric) & !matches("NNA"),
      ~ .x / NNA * 10000,
      .names = "{.col}_10k"
    )
  )

tasa_centros <- tasa_centros %>%
  select(-LocCodigo_10k)

#ACP

proteccion <- tasa_centros %>%
  mutate(
    LocCodigo = sprintf("%02d", as.integer(LocCodigo))
  ) %>%
  left_join(
    tasa_violencia %>%
      select(
        CODIGO_lOCALIDAD,
        tasa_violencia
      ),
    by = c("LocCodigo" = "CODIGO_lOCALIDAD")
  )


datos_proteccion <- tasa_centros  %>%
  select(ends_with("_10k"))


datos_proteccion <- datos_proteccion %>%
  bind_cols(
    tasa_violencia %>%
      select(tasa_violencia)
  )

acp_proteccion <- prcomp(
  datos_proteccion,
  center = TRUE,
  scale. = TRUE
)

proteccion <- proteccion %>%
  mutate(
    indice_proteccion = acp_proteccion$x[, 1]
  )


# Normalización 0-100

proteccion <- proteccion %>%
  mutate(
    indice_proteccion_100 = 
      (indice_proteccion - min(indice_proteccion, na.rm = TRUE)) /
      (max(indice_proteccion, na.rm = TRUE) - min(indice_proteccion, na.rm = TRUE)) * 100
  )

#Guardar

saveRDS(
  proteccion,
  "data/processed/proteccion.rds"
)
