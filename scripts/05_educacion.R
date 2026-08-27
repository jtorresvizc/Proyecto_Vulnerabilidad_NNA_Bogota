
#Datos

setwd("C:/Users/manue/OneDrive/Escritorio/a borrar/DataJam")

source("scripts/01_carga_datos.R")

#Tratamiento

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


# APROBACION

aprobacion_localidad <- tasaaceptacion %>%
  st_drop_geometry() %>%
  mutate(
    aprobacion =
      rowMeans(
        select(
          .,
          starts_with("H_"),
          starts_with("M_")
        ),
        na.rm=TRUE
      )
  ) %>%
  select(
    COD_LOCA,
    aprobacion
  ) %>%
  mutate(
    COD_LOCA=as.numeric(COD_LOCA)
  )


# DESERCION

desercion_localidad <- tasadesercion %>%
  st_drop_geometry() %>%
  mutate(
    desercion =
      rowMeans(
        select(
          .,
          starts_with("H_"),
          starts_with("M_")
        ),
        na.rm=TRUE
      )
  ) %>%
  select(
    COD_LOCA,
    desercion
  ) %>%
  mutate(
    COD_LOCA=as.numeric(COD_LOCA)
  )

# REPROBACION

reprobacion_localidad <- tasareprobacion %>%
  st_drop_geometry() %>%
  mutate(
    reprobacion =
      rowMeans(
        select(
          .,
          starts_with("H_"),
          starts_with("M_")
        ),
        na.rm=TRUE
      )
  ) %>%
  select(
    COD_LOCA,
    reprobacion
  ) %>%
  mutate(
    COD_LOCA=as.numeric(COD_LOCA)
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


# ACP

datos_educacion <- educacion %>%
  select(
    colegios_10k,
    beneficiarios_10k,
    aprobacion,
    desercion,
    reprobacion
  )


# quitar localidades con faltantes
datos_educacion <- na.omit(datos_educacion)



# invertir indicadores negativos

datos_educacion <- datos_educacion %>%
  mutate(
    desercion=-desercion,
    reprobacion=-reprobacion
  )



acp_educacion <- prcomp(
  datos_educacion,
  center=TRUE,
  scale.=TRUE
)



#########################
# INDICE
#########################

educacion <- educacion %>%
  filter(
    complete.cases(
      colegios_10k,
      beneficiarios_10k,
      aprobacion,
      desercion,
      reprobacion
    )
  ) %>%
  mutate(
    indice_educacion =
      acp_educacion$x[,1]
  )



#########################
# NORMALIZACION 0-100
#########################

educacion <- educacion %>%
  mutate(
    indice_educacion_100 =
      (indice_educacion-min(indice_educacion))/
      (max(indice_educacion)-min(indice_educacion))*100
  )


#Guardar

saveRDS(
  educacion,
  "data/processed/educacion.rds"
)
