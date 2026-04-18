# =====================================================================
#  DATA PREP - EPHC REG02 (personas) -> dataset para app Shiny
#  Salidas:
#   - data/db_full.rds (para modelos y análisis)
#   - data/db_app.csv  (vista liviana para tablas/descargas)
# =====================================================================

cat("📦 DATA PREP — EPHC REG02 (personas)\n")

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(readr)
  library(janitor)
})

BASE_DIR <- local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) >= 1) {
    dirname(normalizePath(sub("^--file=", "", file_arg[1])))
  } else if (!is.null(sys.frame(1)$ofile)) {
    dirname(normalizePath(sys.frame(1)$ofile))
  } else {
    getwd()
  }
})

setwd(BASE_DIR)

source("R/utils.R")
source("R/sml.R")
source("R/reg02.R")

ANIOS_ANALISIS <- 2022:2025
TOL_DEFAULT <- 0.10

DATA_DIR_PERSONAS <- file.path(BASE_DIR, "data", "personas")
OUT_FULL <- file.path(BASE_DIR, "data", "db_full.rds")
OUT_APP <- file.path(BASE_DIR, "data", "db_app.csv")

if (!dir.exists(DATA_DIR_PERSONAS)) {
  stop("No existe: ", DATA_DIR_PERSONAS, " (crear y colocar CSV REG02).")
}

csv_files <- list.files(DATA_DIR_PERSONAS, pattern = "^REG02.*\\.csv$", full.names = TRUE)
if (length(csv_files) == 0) {
  stop("No se detectaron CSV REG02 en: ", DATA_DIR_PERSONAS)
}

cat("✓ Archivos encontrados:", length(csv_files), "\n")

db <- read_reg02_files(csv_files, anios = ANIOS_ANALISIS)

sml_data <- get_sml_data(include_projection = TRUE)
sml_monthly <- build_sml_monthly(sml_data, end_date = as.Date(paste0(max(ANIOS_ANALISIS), "-12-01")))
db <- add_sml_to_db(db, sml_monthly)

db <- derive_reg02_vars(db, tol = TOL_DEFAULT)

# Dataset completo (para app/modelos)
saveRDS(db, OUT_FULL)
cat("✅ Guardado:", OUT_FULL, "\n")

# Dataset liviano (para descargas rápidas)
db_app <- db %>%
  transmute(
    anio, q, trimestredesc,
    sexo, ingoc1sml_cat,
    edad, educacion = anoest_val,
    salario = salario,
    sal_hora = sal_hora,
    cotiza_bin, area_urb,
    tam, rama_pea,
    w, sml, ratio_sml
  )
readr::write_csv(db_app, OUT_APP)
cat("✅ Guardado:", OUT_APP, "\n")

cat("📊 Filas:", nrow(db), " | Columnas:", ncol(db), "\n")
cat("🗓️  Años:", paste(sort(unique(db$anio)), collapse = ", "), "\n")
