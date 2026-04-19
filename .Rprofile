# Mensaje y checks rápidos al abrir R en el repo
cat("\n")
cat("📊 Proyecto: Estudio SML Paraguay (Shiny)\n")
cat("🚀 Ejecutar app: shiny::runApp('app.R')\n")
cat("📦 Preparar datos: source('data-prep.R')\n")
cat("\n")

options(
  repos = c(CRAN = "https://cloud.r-project.org"),
  digits = 4,
  width = 120,
  scipen = 10,
  dplyr.print_max = 50
)

required_packages <- c(
  "shiny", "bslib", "dplyr", "tidyr", "ggplot2", "plotly", "DT",
  "readr", "janitor", "stringr", "lubridate", "scales",
  "survey", "fixest", "modelsummary"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  cat("⚠️  Paquetes faltantes:\n")
  cat(paste("  -", missing_packages), sep = "\n")
  cat("\n💡 Instalar con:\n")
  cat(paste0("install.packages(c('", paste(missing_packages, collapse = "', '"), "'))\n\n"))
}
