# Estudio SML Paraguay — App web (Shiny)

Dashboard interactivo para analizar la relación entre el **Salario Mínimo Legal (SML)** y variables del mercado laboral usando microdatos **EPHC (INE)**.

## Requisitos

- R >= 4.2
- Paquetes: `shiny`, `bslib`, `dplyr`, `tidyr`, `ggplot2`, `plotly`, `DT`, `readr`, `janitor`, `stringr`, `lubridate`, `scales`, `survey`, `fixest`, `modelsummary`

## Datos

Por defecto, este repo **no versiona microdatos** (ver `.gitignore`).

- Colocar los CSV REG02 (personas) en: `data/personas/`
  - Nombres esperados (flexible): `REG02*_Trim_20YY.csv` (ej. `REG02_EPHC_2do_Trim_2025.csv`)

## Ejecutar

0) (Opcional) Setup de carpetas/paquetes:

```r
source("setup.R")
```

1) (Opcional) Preparar datos para la app:

```r
source("data-prep.R")
```

2) Iniciar el dashboard:

```r
shiny::runApp("app.R")
```

## PDF (opcional)

Para generar el informe en PDF (requiere LaTeX):

```r
rmarkdown::render("reporte_SML_R02only.Rmd")
```

El script fuente del informe es `estudioSML_R02only.R`.

## Qué incluye

- Serie de SML (incluye 2025 oficial y 2026 proyección preliminar)
- Distribución por franjas relativas al SML (tolerancia configurable)
- Descriptivos ponderados (edad, educación, formalidad, salarios, etc.)
- Econometría con diseño de encuesta (`svyglm`)
- Event study alrededor del ajuste de julio (ponderado y con clustering por UPM)
- Bunching / masa alrededor de 1 SML
