# =====================================================================
#  SETUP - Configuración inicial del proyecto
#  Ejecutar una vez: source("setup.R")
# =====================================================================

cat("🚀 Setup — Estudio SML Paraguay\n\n")

dirs_required <- c(
  "data",
  "data/personas",
  "output",
  "output/figures",
  "output/tables"
)

for (dir in dirs_required) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    cat("✓ Creado:", dir, "\n")
  }
}

required_packages <- c(
  "shiny", "bslib", "dplyr", "tidyr", "ggplot2", "plotly", "DT",
  "readr", "janitor", "stringr", "lubridate", "scales",
  "survey", "fixest", "modelsummary", "rmarkdown"
)

missing <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  cat("\n📦 Instalando paquetes faltantes:\n")
  cat(paste(" -", missing), sep = "\n")
  install.packages(missing)
}

cat("\n✅ Listo.\n")
cat("Siguiente:\n")
cat(" - Preparar datos: source('data-prep.R')\n")
cat(" - Ejecutar app: shiny::runApp('app.R')\n\n")

