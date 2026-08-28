# Visor de vulnerabilidad infantil (NNA) — Bogotá

**DataJam Edición 3 — 2026**

Visor territorial que mide y diagnostica la vulnerabilidad de niños, niñas y adolescentes (NNA) en las 20 localidades de Bogotá, examina si esa vulnerabilidad forma patrones espaciales, y traduce el diagnóstico en una priorización territorial para una estrategia distrital con intensidad diferenciada.

---

## Problema

Las condiciones asociadas al bienestar de los NNA difieren de forma importante entre las localidades de Bogotá. Esas diferencias no son solo de nivel sino de **composición**: dos territorios con vulnerabilidad general parecida pueden concentrar problemas muy distintos —uno en salud y nutrición, otro en violencia y protección, otro en condiciones económicas del hogar—.

Una respuesta distrital que trate a todas las localidades igual desperdicia recursos donde no son la restricción activa, y una que se limite a un ranking no dice **qué** hacer en cada territorio. El problema que aborda este proyecto es producir un diagnóstico territorial que sirva para graduar la intensidad y el contenido de una misma arquitectura de política pública.

## Pregunta analítica

> **¿Qué perfiles territoriales de vulnerabilidad infantil existen en Bogotá, cómo se distribuyen espacialmente y qué implicaciones tienen para una respuesta distrital con intensidad diferenciada según las necesidades territoriales?**

La pregunta es descriptiva y territorial, no predictiva: el estudio es transversal y no hay serie temporal del índice compuesto.

## Hipótesis

> **La vulnerabilidad infantil no se distribuye aleatoriamente entre las localidades de Bogotá y presenta patrones de dependencia espacial identificables mediante Moran global y LISA.**

El resultado obtenido **respalda parcialmente** esta hipótesis. Ver *Resultados*.

---

## Datos

Todas las fuentes son públicas. La nota técnica de integración está en [`docs/nota_tecnica_integracion_datos.md`](docs/nota_tecnica_integracion_datos.md).

| Fuente | Entidad | Uso |
|---|---|---|
| Encuesta Multipropósito de Bogotá (microdatos por capítulos) | DANE / SDP | Afiliación en salud de NNA; indicadores laborales, de ingreso y consumo alimentario de hogares con NNA |
| `osb_tm_infantil` | Observatorio de Salud de Bogotá | Tasa de mortalidad infantil por localidad; serie 2007–2026 |
| `osb_malnutricion5agnos` | Observatorio de Salud de Bogotá | Riesgo de desnutrición global y desnutrición aguda (0–5 años); serie 2015–2026 |
| `osb_malnutricion5_17anos` | Observatorio de Salud de Bogotá | Sobrepeso, obesidad y delgadez (5–17 años) |
| `osb_saludmental-vintrafamiliar` | Observatorio de Salud de Bogotá | Casos de violencia contra NNA → tasa por 10.000 NNA |
| 10 capas de infraestructura de protección | SDIS / Datos Abiertos Bogotá | Centros AMAR, CRECER, PROTEGER, ABRAZAR, RENACER, jardines infantiles, jardín nocturno, casas de pensamiento intercultural, espacios rurales, comisarías de familia |
| `Colegios12_2024`, `Beneficiarios_032025` | Secretaría de Educación | Capa de oferta institucional (fuera del índice) |
| `TasasAprobacion2024`, `TDesercionOf`, `TReprobacionOf` | Secretaría de Educación | Dimensión de educación (deserción y reprobación oficiales) |
| Anexo de proyecciones de población por localidad | Secretaría Distrital de Planeación | Denominador de todas las tasas por 10.000 NNA y población de referencia |
| `Loca.shp` | IDECA | Geometría de las 20 localidades; matriz de contigüidad reina |

Son **10 fuentes públicas** integradas, muy por encima del mínimo de 3 exigido.

---

## Metodología

### Unidad de análisis y diseño

20 localidades de Bogotá. Diseño **transversal y territorial**.

### Construcción de las dimensiones

Las cuatro dimensiones se construyen con el mismo procedimiento, implementado en [`scripts/00_funciones.R`](scripts/00_funciones.R):

1. **Estandarización.** Cada variable se convierte a puntaje z entre localidades.
2. **Armonización de signo.** Las variables protectoras (alto = mejor situación) se invierten, de modo que **todas** queden con el sentido *alto = peor situación*.
3. **Índice dimensional.** Promedio de los puntajes z armonizados.
4. **Normalización.** Reescalado 0–100 sobre el rango observado.

La escala 0–100 es **relativa**: 0 es la localidad menos vulnerable observada y 100 la más vulnerable observada.

| Dimensión | Variables en el índice | Invertidas |
|---|---|---|
| Salud | afiliación, mortalidad infantil, riesgo DNT global, DNT aguda moderada y severa, sobrepeso, obesidad, delgadez | afiliación |
| Protección | tasa de violencia contra NNA, comisarías de familia por 10.000 NNA | comisarías |
| Economía | ingreso bajo, consumo alimentario bajo, baja ocupación, desempleo, informalidad | ninguna |
| Educación | deserción oficial, reprobación oficial | ninguna |

### Papel del ACP

El proyecto se planteó originalmente construir cada índice con el primer componente principal. **Al corregir la orientación de las variables ese enfoque dejó de ser defendible**, y la decisión está documentada:

- En **salud**, el CP1 resultaba ser un contraste entre el bloque nutricional 0–5 y el 5–17, con correlación de apenas **0,07** frente al promedio estandarizado de vulnerabilidad; con una correlación tan cercana a cero el criterio de reorientación por signo queda indeterminado.
- En el **índice general**, el CP1 sobre las cuatro dimensiones corregidas tiene **cargas de signo mixto**. Ponderar con él restaría unas dimensiones a otras.
- Con **N = 20** y pocas variables por dimensión, el promedio de puntajes z es más estable que un componente principal y garantiza la orientación por construcción.

Por eso el **índice general pondera las cuatro dimensiones por igual (25% cada una)** y el **ACP se conserva y se reporta como diagnóstico** (cargas y varianza explicada, visibles en el visor). El pipeline emite una advertencia explícita si alguna dimensión no correlaciona positivamente con el índice general.

### Variables excluidas por redundancia composicional

Se retiraron variables que son el complemento aritmético de otras del mismo bloque:

- **Salud:** `PROPORCION_PESO_ADECUADO_PARA_LA_EDAD` e `PROPORCION_IMC_ADECUADO`. Los bloques suman 84,3–89,0 y 87,5–93,8 con desviación estándar de 1,01 y 1,21, es decir son cerradas.
- **Educación:** `aprobacion`, que equivale a `100 − desercion − reprobacion` (verificado: la suma da 100,0 en las 20 localidades).

### Tratamiento de la oferta pública focalizada

Las tasas de colegios oficiales, beneficios escolares y centros de protección por 10.000 NNA **no entran al índice**. Son oferta focalizada: el Distrito las ubica donde hay más necesidad, de modo que son **más altas en las localidades más pobres**. Tratarlas como factor protector invertía por completo la dimensión de educación y dejaba educación correlacionando −0,74 con economía.

Se conservan como **capa de oferta institucional** y el visor las contrasta contra el índice. El cuadrante de alta vulnerabilidad con baja oferta es el insumo prescriptivo.

*Excepción:* la tasa de comisarías de familia sí entra en protección, porque responde al diseño administrativo de acceso a la justicia y no a focalización socioeconómica.

### Análisis espacial

- **Matriz de vecindad:** contigüidad tipo reina, `poly2nb(queen = TRUE)`.
- **Matriz de pesos:** `nb2listw(style = "W", zero.policy = TRUE)`.
- **Moran global:** `moran.test()` bajo aleatorización.
- **Moran local (LISA):** `localmoran()`. La clasificación en Alto-Alto, Bajo-Bajo, Alto-Bajo y Bajo-Alto se deriva del signo del índice estandarizado y de su rezago espacial. **Ningún cluster se asigna manualmente.**
- **Umbral local:** p < 0,05 **sin** corrección por comparaciones múltiples (decisión documentada como limitación exploratoria).

### Priorización territorial

Combina cuatro criterios: nivel de vulnerabilidad (terciles), patrón espacial LISA solo cuando es significativo, perfil dimensional (define el *contenido*, no la intensidad) y alcance poblacional (dimensiona la escala operativa).

Categorías: **Intensificación prioritaria**, **Prevención reforzada**, **Sostenimiento y monitoreo**.

---

## Resultados

### Dónde se concentra la vulnerabilidad

Las localidades con mayor índice general son **Los Mártires (100,0), Usme (96,2), Ciudad Bolívar (95,9), Santa Fe (81,4), Rafael Uribe Uribe (77,4), San Cristóbal (75,1) y Barrios Unidos (72,5)**.

### Perfiles dimensionales

El perfil no coincide con el nivel. Entre las de mayor índice, la dimensión de mayor vulnerabilidad relativa es **salud** en Los Mártires, Rafael Uribe Uribe y San Cristóbal; **protección** en Usme y Ciudad Bolívar; **economía** en Santa Fe; y **educación** en Barrios Unidos. Territorios con índices similares requieren respuestas distintas.

### Estructura entre dimensiones

Las cuatro dimensiones **no se mueven juntas**. El CP1 del ACP general explica 46,4% de la varianza pero con cargas de signo mixto: separa a las localidades por el *tipo* de vulnerabilidad más que por su nivel.

### Autocorrelación espacial

**Moran global: I = 0,206**, frente a E[I] = −0,053, con z = 1,760 y **p = 0,039**. Hay evidencia de **autocorrelación espacial positiva**: las localidades vecinas se parecen más entre sí de lo que cabría esperar por azar.

### Conglomerados LISA

El análisis local identifica **1 de 20 localidades** con autocorrelación significativa a p < 0,05: **Barrios Unidos**, con patrón **Alto-Bajo** (foco de vulnerabilidad alta en un entorno menos vulnerable). **No se identifica ningún conglomerado Alto-Alto.**

**Lectura conjunta.** Los dos niveles apuntan en direcciones distintas y deben leerse juntos. A escala de ciudad hay evidencia de agrupación, pero no se concreta en conglomerados locales significativos. La explicación más plausible es de **potencia estadística**: con 20 unidades y un promedio de 4,3 vecinas por localidad, el estadístico local se apoya en muy pocas observaciones. La conclusión defendible es que la vulnerabilidad infantil **no se distribuye de forma aleatoria**, pero los datos no permiten delimitar con confianza los bordes de cada conglomerado. La priorización se apoya principalmente en el nivel del índice y el perfil dimensional, usando el patrón espacial como criterio complementario.

### Priorización resultante

7 localidades en **intensificación prioritaria**, 6 en **prevención reforzada**, 7 en **sostenimiento y monitoreo**.

### Tendencia histórica (componente de perspectiva)

Ventana de visualización: **2017–2026**, una década comparable entre indicadores. El inventario de series reporta el rango completo disponible en cada fuente (mortalidad desde 2007, malnutrición 0–5 desde 2015).

- **Mortalidad infantil (2017–2026):** a la baja en **10** localidades y al alza en **9**, sobre las 19 con serie estable. La caída sostenida de la mortalidad infantil en Bogotá ocurrió principalmente **antes** de 2017; dentro de esta ventana el panorama está dividido.
- **Riesgo de desnutrición global en menores de 5 años (2017–2026):** al alza en **8** localidades, a la baja en **7** y estable en **5**.

Son señales direccionales descriptivas de indicadores concretos, **no un pronóstico del índice**.

**Estabilidad del denominador.** La tasa de mortalidad de **Sumapaz** se calcula sobre una mediana de **28 nacidos vivos al año**: dos defunciones en 2024 producen una tasa de 133 por 1.000. Con denominadores así, la serie refleja variación aleatoria y no tendencia. Se marca como inestable y se excluye del gráfico por defecto, con una casilla para incluirla; la tabla sigue mostrando las 20 localidades, señalando cuál es inestable.

### Implicaciones de política

1. **Priorización territorial de recursos** en las 7 localidades de intensificación prioritaria, manteniendo cobertura universal.
2. **Fortalecimiento diferencial según perfil**: el contenido de la intervención sigue la dimensión dominante de cada territorio.
3. **Prevención reforzada** en las 6 localidades del tramo intermedio: capacidad anticipatoria antes que respuesta correctiva.
4. **Coordinación intersectorial** sobre un mismo territorio, dado que las dimensiones no se mueven juntas.
5. **Fortalecimiento de oferta donde hay brecha**, usando el contraste entre índice y oferta instalada.
6. **Monitoreo diferencial por dimensión**, porque el índice agregado oculta perfiles muy distintos.

---

## Limitaciones

1. **N = 20 localidades.** Limita la estabilidad de cualquier técnica multivariada y del análisis espacial. Con 4,3 vecinas en promedio, el Moran local tiene poca potencia.
2. **Diseño transversal.** El índice describe el periodo de referencia y no permite inferir trayectorias futuras.
3. **No existe modelo predictivo del índice compuesto.** No es defendible con 20 observaciones territoriales y sin serie del índice.
4. **Población de referencia, no proyección propia.** El denominador proviene del anexo oficial de población. Se reporta como población de referencia del año utilizado; no se presenta ninguna cifra futura ni estimación propia.
5. **LISA exploratorio.** p < 0,05 sin corrección por comparaciones múltiples; con 20 pruebas la tasa de falsos positivos supera el nivel nominal.
6. **Orientación del ACP y decisión de ponderación.** La orientación requirió una decisión conceptual explícita; al aplicarla el CP1 dejó de ser un factor común, por lo que el índice usa pesos iguales y el ACP se reporta como diagnóstico.
7. **Redundancia composicional.** Se excluyeron complementos aritméticos que generaban correlaciones artificiales.
8. **Cobertura como factor protector.** Las tasas de oferta pública son más altas en las localidades más pobres: miden respuesta institucional, no necesidad. Se reportan como capa separada y su relación con la vulnerabilidad no implica causalidad en ninguna dirección.
9. **Educación mide solo el sector oficial.** En localidades donde la mayoría de NNA asiste a colegios privados, la matrícula oficial es pequeña y no representa a la población residente. Esto explica que educación correlacione negativamente con las demás dimensiones.
10. **Falacia ecológica.** Los resultados son de localidades y no permiten inferir características de individuos u hogares.
11. **Asociación no implica causalidad.** El índice y los patrones espaciales son herramientas descriptivas y de priorización.
12. **Reproducibilidad parcial de la etapa de microdatos.** Ver *Ejecución*.

---

## Ejecución

### Requisitos

R ≥ 4.5.0. Dependencias en [`renv.lock`](renv.lock):

```r
install.packages("renv")
renv::restore()
```

### 1. Procesamiento (genera todo `data/processed/`)

Desde la **raíz del proyecto**:

```bash
Rscript scripts/00_ejecutar_todo.R
```

Ejecuta en orden `00_funciones` → `01_carga_datos` → `02_salud` → `03_proteccion` → `04_economia` → `05_educacion` → `06_indice_general` → `07_espacial` → `08_perspectiva`, y verifica al final que existan las 20 localidades con índice completo.

### 2. Aplicación

```r
shiny::runApp("scripts/app.R")
```

La app funciona tanto desde la raíz del proyecto como desde `scripts/`, y falla con un mensaje explícito si falta algún insumo del procesamiento.

### Nota sobre los microdatos de la Encuesta Multipropósito

Las dimensiones de **salud** y **economía** se derivan de capítulos de la EMB que deben estar en `data/raw/Encuesta Multiproposito Bogota/`. Los capítulos requeridos son **E** y **F** (salud) y **E**, **K** y **M1** (economía).

Si esos archivos no están presentes, el pipeline **no falla ni imputa**: detecta la ausencia, la informa por consola y reutiliza la capa de insumos ya calculada (`data/processed/insumos_salud.rds`, `data/processed/insumos_economia.rds`), que contiene exactamente los mismos indicadores por localidad que produce la etapa cruda. La etapa ACP se recalcula íntegramente en ambos casos.

Para reproducir el pipeline **completo desde microdatos**, descargue los capítulos faltantes de la EMB y vuelva a ejecutar `00_ejecutar_todo.R`.

---

## Estructura

```
.
├── data/
│   ├── raw/                     Datos crudos (EMB, OSB, anexo de población)
│   ├── shapefiles/              Cartografía: localidades, colegios,
│   │                            infraestructura de protección, tasas escolares
│   └── processed/               Salidas del procesamiento (.rds)
├── scripts/
│   ├── 00_funciones.R           Armonización y construcción de índices
│   ├── 00_ejecutar_todo.R       Runner del pipeline completo
│   ├── 01_carga_datos.R         Lectura de datos crudos (tolerante a ausencias)
│   ├── 02_salud.R               Dimensión salud
│   ├── 03_proteccion.R          Dimensión protección + capa de oferta
│   ├── 04_economia.R            Dimensión economía
│   ├── 05_educacion.R           Dimensión educación + capa de oferta
│   ├── 06_indice_general.R      Índice general y perfiles dimensionales
│   ├── 07_espacial.R            Moran global, LISA y priorización
│   ├── 08_perspectiva.R         Series temporales y alcance poblacional
│   └── app.R                    Aplicación Shiny
├── docs/
│   └── nota_tecnica_integracion_datos.md
├── README.md
└── renv.lock
```

### Arquitectura

```
datos crudos → scripts 01–05 → índices dimensionales
                                      ↓
                            06 → vulnerabilidad.rds + acp_general.rds
                                      ↓
                            07 → analisis_espacial.rds
                            08 → perspectiva.rds
                                      ↓
                                   app.R (solo visualiza)
```

Todo el cálculo pesado ocurre en el procesamiento y se persiste en `.rds`. La aplicación se encarga únicamente de visualización, filtros, interacción e interpretación.
