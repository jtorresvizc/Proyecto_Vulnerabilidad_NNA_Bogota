# Nota técnica de integración de datos públicos

**Visor de vulnerabilidad infantil (NNA) — Bogotá · DataJam Edición 3, 2026**

Documenta cómo se integran diez fuentes públicas heterogéneas en una única base de 20 localidades, qué transformaciones se aplican y qué decisiones se tomaron ante inconsistencias reales de las fuentes.

---

## 1. Llave de integración

Toda la integración se hace sobre el **código de localidad de Bogotá (01–20)**, normalizado a **cadena de dos dígitos con cero a la izquierda** (`sprintf("%02d", as.integer(x))`).

Esto es necesario porque las fuentes lo entregan de forma inconsistente:

| Fuente | Columna | Tipo original |
|---|---|---|
| Encuesta Multipropósito | `COD_LOCALIDAD` | carácter, sin cero inicial |
| Observatorio de Salud (malnutrición) | `CODIGO_lOCALIDAD` | entero |
| Observatorio de Salud (mortalidad) | `CÓDIGO.LOCALIDAD` | entero, nombre con tilde y punto |
| Shapefiles SDIS | `LocCodigo` | carácter |
| Shapefiles Educación | `COD_LOCA` | carácter |
| Anexo de población | `COD_LOC` | carácter con cero inicial |
| `Loca.shp` | `LocCodigo` | carácter con cero inicial |

Los nombres de localidad **no** se usan como llave. Se asignan al final, una sola vez, desde un diccionario explícito de 20 entradas en `06_indice_general.R`, y el pipeline verifica con `stopifnot()` que las 20 queden resueltas.

**Razón:** los nombres llegan con codificaciones rotas en los microdatos de la EMB (`Antonio Nari\xf1o`, `Ciudad Bol\xedvar`, `Engativ\xe1`, `Fontib\xf3n`, `Los M\xe1rtires`, `San Crist\xf3bal`, `Usaqu\xe9n`). Unir por nombre habría producido pérdidas silenciosas de filas.

---

## 2. Integración por tipo de fuente

### 2.1 Microdatos de encuesta (Encuesta Multipropósito de Bogotá)

Los capítulos se leen por separado y se unen por los identificadores jerárquicos de la encuesta:

- `DIRECTORIO` — vivienda
- `DIRECTORIO_HOG` — hogar
- `DIRECTORIO_PER` — persona

**Filtro de población objetivo.** Se identifican los NNA por edad (`NPCEP4 ≤ 17`) en el capítulo de demografía, y los hogares con NNA por presencia de al menos un miembro en ese rango.

**Ponderación.** Todos los agregados usan el factor de expansión `FEX_C`. Se implementaron tres estimadores ponderados:

- `prop_ponderada()` — proporción de una categoría
- `razon_ponderada()` — razón entre dos subpoblaciones (ocupación, desempleo, informalidad)
- `mediana_ponderada()` — mediana ponderada (ingreso y consumo per cápita)

Se usa mediana y no media para ingreso y consumo por la asimetría de esas distribuciones.

**Gasto en alimentos.** El capítulo M1 registra el gasto con periodicidades distintas por rubro. Se anualizan a base mensual con el factor 30,44 días/mes: semanal `×30,44/7`, quincenal `×30,44/15`, y para el rubro de periodicidad declarada se usa `30,44/NHCMP1FB`.

### 2.2 Registros administrativos (Observatorio de Salud de Bogotá)

Archivos CSV separados por `;`. Se filtra el año de referencia (2026) y, para la mortalidad infantil, se agregan casos y nacidos vivos por localidad **antes** de calcular la tasa —nunca se promedian tasas ya calculadas—.

Para violencia se filtran los grupos de edad correspondientes a NNA (`Menor 1 año`, `De 1 - 5 años`, `De 6 - 13 años`, `De 14 - 17 años`) y se restringe a códigos de localidad 1–20, descartando registros sin localidad asignable.

### 2.3 Cartografía puntual (infraestructura de protección y educación)

Diez capas de puntos de la SDIS se apilan con `bind_rows()`, se les quita la geometría y se cuentan por localidad y por tipo de equipamiento (`OSSSimbol`), produciendo una matriz ancha de conteos.

Los colegios oficiales y los beneficiarios escolares se agregan por localidad desde sus propios shapefiles.

### 2.4 Cartografía poligonal con atributos (tasas escolares)

Los shapefiles `TasasAprobacion2024`, `TDesercionOf` y `TReprobacionOf` traen las tasas desagregadas por grado y sexo (`H_Trans`, `M_Trans`, `H_Primero`, …) **y además los totales por sexo** (`Thombre`, `Tmujer`).

**Se usan los totales.** Ver sección 4.2.

### 2.5 Denominador poblacional

El anexo de población de la Secretaría Distrital de Planeación se lee del rango `A12:KX1092`, se filtra `AREA == "Total"` y el año de referencia, y se suman las columnas `Hombres_0`…`Hombres_17` y `Mujeres_0`…`Mujeres_17` para obtener la población NNA por localidad.

Esta cifra cumple **dos funciones distintas** que el visor mantiene separadas:

1. **Denominador** de todas las tasas por 10.000 NNA.
2. **Población de referencia** para dimensionar el alcance potencial de la política.

Se etiqueta siempre como *población de referencia* del año utilizado. **No se extrapola, no se proyecta a años futuros y no se presenta ninguna cifra como estimación propia del equipo.**

---

## 3. Normalización de tasas

Todos los conteos de infraestructura y de beneficiarios se convierten a **tasas por 10.000 NNA** usando el denominador poblacional de la localidad:

```
tasa_10k = conteo / NNA * 10.000
```

Esto hace comparables localidades de tamaño muy distinto (Sumapaz tiene del orden de mil NNA; Ciudad Bolívar más de ciento ochenta mil).

**Efecto colateral documentado.** En localidades con muy pocos NNA el denominador pequeño amplifica la tasa. Sumapaz, La Candelaria y Los Mártires presentan valores extremos en varias tasas por esta razón. Es una propiedad de la normalización, no un error, y se tiene en cuenta al interpretar esas localidades.

---

## 4. Inconsistencias encontradas en las fuentes y su tratamiento

Esta sección documenta problemas reales detectados durante la integración. Ninguno se resolvió imputando o simulando valores.

### 4.1 Bloques nutricionales cerrados

Las proporciones nutricionales del Observatorio son **composiciones cerradas**:

| Bloque | Suma observada | Desviación estándar |
|---|---|---|
| Peso adecuado + riesgo DNT + DNT aguda moderada + DNT aguda severa | 84,3 – 89,0 | 1,01 |
| IMC adecuado + sobrepeso + obesidad + delgadez | 87,5 – 93,8 | 1,21 |

La categoría "adecuada" es, en la práctica, el complemento de las demás. Incluir ambas en el mismo análisis duplica la información con signo opuesto y genera correlaciones artificiales (r = 0,85 entre peso adecuado invertido y riesgo de DNT).

**Tratamiento:** se excluyen `PROPORCION_PESO_ADECUADO_PARA_LA_EDAD` y `PROPORCION_IMC_ADECUADO`. La información no se pierde: sigue representada por las categorías de malnutrición.

### 4.2 Ceros estructurales en las tasas escolares por grado

Las columnas `H_Trece` y `M_Trece` valen **0 en las 20 localidades**, y `H_Doce`/`M_Doce` valen 0 en varias. Son **ausencia de grado**, no tasas de cero.

Promediar las columnas de grado, además de dar el mismo peso a cada grado sin importar su matrícula, arrastraba esos ceros al resultado: la aprobación de Teusaquillo pasaba de **83,2% real a 72,1% calculado**.

**Tratamiento:** se usan los totales oficiales `Thombre`/`Tmujer`.

**Validación:** con los totales, `aprobación + deserción + reprobación = 100,0` en las 20 localidades (desviación máxima 0,1). Con el promedio por grado esa identidad no se cumple. La verificación está implementada como `stopifnot()` en `05_educacion.R`.

### 4.3 Las tres tasas escolares son exhaustivas

Consecuencia de la validación anterior: `aprobacion = 100 − desercion − reprobacion`. Incluir las tres es el mismo problema composicional de la sección 4.1.

**Tratamiento:** el índice usa solo `desercion` y `reprobacion`. `aprobacion` se conserva como variable descriptiva.

### 4.4 Oferta pública focalizada correlacionada con la pobreza

Las tasas de oferta pública por 10.000 NNA son **más altas en las localidades más pobres**, porque el Distrito las focaliza donde hay más necesidad:

| Variable | r con el índice económico |
|---|---|
| `beneficiarios_10k` | +0,58 |
| `colegios_10k` | +0,25 |

Tratarlas como factor protector invertido hacía que la dimensión midiera *ausencia de oferta focalizada* en lugar de vulnerabilidad, con resultados contrarios a toda la evidencia distrital (Teusaquillo y Chapinero como las más vulnerables en educación).

**Tratamiento:** salen del índice y se conservan como **capa de oferta institucional** (`oferta_proteccion.rds`, `oferta_educacion.rds`), que el visor contrasta contra el índice.

### 4.5 Las tasas escolares cubren solo el sector oficial

Incluso con el cálculo corregido, la aprobación oficial correlaciona **+0,62** con el índice de vulnerabilidad económica.

La causa es un **efecto de composición**: en localidades de ingreso alto la matrícula oficial es pequeña y atiende una población seleccionada, mientras la mayoría de NNA residentes asiste a colegios privados invisibles en esta fuente.

**Esto no es corregible con los datos disponibles.** Se declara como limitación explícita en el visor y en el README, y explica por qué la dimensión de educación correlaciona negativamente con las demás.

### 4.6 Capítulos de la EMB ausentes

Los capítulos C, D, E, F, G, I, L, M1 y M2 pueden no estar presentes en la copia local de los datos.

**Tratamiento:** `01_carga_datos.R` usa una función de lectura tolerante que devuelve `NULL` y registra el archivo faltante en `emb_faltantes`, emitiendo una advertencia con la lista completa. Los scripts `02_salud.R` y `04_economia.R` comprueban la disponibilidad de los capítulos que necesitan y, si faltan, reutilizan la **capa de insumos persistida** (`insumos_salud.rds`, `insumos_economia.rds`) informándolo por consola. Si tampoco existe esa capa, se detienen con un mensaje que nombra los archivos exactos que faltan.

En ningún caso se imputan, promedian ni simulan valores para cubrir la ausencia.

---

## 5. Verificaciones automáticas del pipeline

El procesamiento incluye comprobaciones que **detienen la ejecución** si fallan:

| Verificación | Dónde |
|---|---|
| Identidad aprobación + deserción + reprobación = 100 | `05_educacion.R` |
| Salud sube con mortalidad, baja con afiliación | `02_salud.R` |
| Protección sube con violencia, baja con comisarías | `03_proteccion.R` |
| Educación sube con deserción y reprobación | `05_educacion.R` |
| Economía baja con ingreso, sube con desempleo | `04_economia.R` |
| Las 20 localidades tienen índice en las 4 dimensiones | `06_indice_general.R` |
| Los 20 códigos resuelven a un nombre de localidad | `06_indice_general.R` |
| El shapefile y la base de índices coinciden en filas | `07_espacial.R` |
| Ninguna variable entra al ACP con varianza nula | `00_funciones.R` |

Adicionalmente se emiten **advertencias** (sin detener) cuando alguna dimensión no correlaciona positivamente con el índice general, cuando el CP1 tiene cargas de signo mixto, y cuando alguna localidad queda sin vecinas en la matriz de contigüidad.

---

## 6. Trazabilidad

Cada objeto persistido en `data/processed/` guarda, además de los índices, las **variables de entrada** que los produjeron y el diagnóstico del ACP correspondiente (`cargas`, `varianza_cp1`, `variables`, `protectoras`). Eso permite auditar cualquier índice sin volver a ejecutar el pipeline y es lo que alimenta la tabla de variables por dimensión de la pestaña de Metodología del visor.
