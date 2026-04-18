# =====================================================================
#  EL SALARIO MÍNIMO LEGAL Y SU IMPACTO (EPHC 2022–2025) — SCRIPT R
#  VERSIÓN R02 ONLY (sin datos de viviendas REG01)
#  Limpieza + Descriptivos ponderados + Econometría + Export a Overleaf
# =====================================================================

# -----------------------------
# CONFIG: rutas y años/trimestres
# -----------------------------

ANIOS_ANALISIS <- 2022:2025

BASE_DIR <- local({
  # Funciona con `Rscript estudioSML_R02only.R` y cae a getwd() si es interactivo.
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) >= 1) {
    dirname(normalizePath(sub("^--file=", "", file_arg[1])))
  } else if (!is.null(sys.frame(1)$ofile)) {
    dirname(normalizePath(sub("^--file=", "", sys.frame(1)$ofile)))
  } else {
    getwd()
  }
})

LOCAL_R_LIB <- file.path(BASE_DIR, "r_lib")
if (dir.exists(LOCAL_R_LIB)) .libPaths(c(LOCAL_R_LIB, .libPaths()))

DATA_DIR_PERSONAS <- file.path(BASE_DIR, "data", "personas")  # CSV (REG02_*_Trim_*.csv)
OUT_DIR <- file.path(BASE_DIR, "output")                      # se crean subcarpetas
FIG_DIR <- file.path(OUT_DIR, "figures")
TAB_DIR <- file.path(OUT_DIR, "tables")

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(TAB_DIR, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# Paquetes
# -----------------------------
library("dplyr")
library("tidyr")
library("stringr")
library("lubridate")
library("haven")
library("readr")
library("janitor")
library("ggplot2")
library("scales")
library("xtable")
library("survey")
library("srvyr")
library("fixest")

# -----------------------------
# Helpers
# -----------------------------

w_mean <- function(x, w) { sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE) }

w_share <- function(cond, w) { sum(w * (cond), na.rm = TRUE) / sum(w, na.rm = TRUE) }

get_num <- function(df, nm) {
  if (!nm %in% names(df)) return(rep(NA_real_, nrow(df)))
  suppressWarnings(as.numeric(df[[nm]]))
}

# Estilo (tablas/figuras)
COL_SML <- c(
  "Menos de 1 SML" = "#B0BEC5",
  "1 SML" = "#2E7D32",
  "Más de 1 SML" = "#1565C0"
)

CAPTION_BASE <- "Fuente: elaboración propia con microdatos EPHC (INE). Notas: ponderado por 'w'; franjas relativas al SML definidas en ±10%."

THEME_SML <- function() {
  theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      plot.title = element_text(face = "bold"),
      plot.caption = element_text(hjust = 0, face = "italic")
    )
}

save_plot <- function(plot, filename, width = 8, height = 6, dpi = 300) {
  ggsave(file.path(FIG_DIR, paste0(filename, ".png")), plot, width = width, height = height, dpi = dpi, bg = "white")
  ggsave(file.path(FIG_DIR, paste0(filename, ".pdf")), plot, width = width, height = height, dpi = dpi, bg = "white")
}

# -----------------------------
# Ingesta PERSONAS (CSV) 2022-2025
# -----------------------------
if (!dir.exists(DATA_DIR_PERSONAS)) {
  stop("No existe DATA_DIR_PERSONAS: ", DATA_DIR_PERSONAS)
}

csv_files <- list.files(DATA_DIR_PERSONAS, pattern = "^REG02.*Trim_20\\d{2}\\.csv$", full.names = TRUE)
if (length(csv_files) == 0) stop("No se detectaron CSV REG02 en: ", DATA_DIR_PERSONAS)

csv_files <- csv_files[grepl(paste0("(", paste(ANIOS_ANALISIS, collapse = "|"), ")\\.csv$"), csv_files)]
if (length(csv_files) == 0) stop("No se detectaron CSV REG02 para ANIOS_ANALISIS en: ", DATA_DIR_PERSONAS)

parse_file_info <- function(fp) {
  b <- basename(fp)
  anio_file <- suppressWarnings(as.integer(stringr::str_extract(b, "(20\\d{2})")))
  q_file <- dplyr::case_when(
    stringr::str_detect(b, "1er_Trim_") ~ 1L,
    stringr::str_detect(b, "2do_Trim_") ~ 2L,
    stringr::str_detect(b, "3er_Trim_") ~ 3L,
    stringr::str_detect(b, "4to_Trim_") ~ 4L,
    TRUE ~ NA_integer_
  )
  tibble::tibble(file = fp, anio_file = anio_file, q_file = q_file)
}

csv_meta <- dplyr::bind_rows(lapply(csv_files, parse_file_info)) %>%
  filter(!is.na(anio_file), !is.na(q_file)) %>%
  filter(anio_file %in% ANIOS_ANALISIS) %>%
  arrange(anio_file, q_file)

if (nrow(csv_meta) == 0) stop("No se pudieron parsear (anio, trimestre) desde los nombres de archivo en: ", DATA_DIR_PERSONAS)

if (nrow(csv_meta) != length(ANIOS_ANALISIS) * 4) {
  warning(
    "Se esperaban ", length(ANIOS_ANALISIS) * 4, " archivos (", min(ANIOS_ANALISIS), "–", max(ANIOS_ANALISIS),
    "), pero se encontraron ", nrow(csv_meta), "."
  )
  print(csv_meta)
}

read_personas <- function(fp, anio_file, q_file) {
  # Leer con todas las columnas como character primero
  df <- suppressMessages(readr::read_csv2(fp, show_col_types = FALSE, col_types = cols(.default = col_character())))
  df <- janitor::clean_names(df)

  # Convertir columnas numéricas específicas, manejando formato como "123,45"
  numeric_cols <- c("upm", "nvivi", "nhoga", "p02", "p06", "b10", "anoest", "area",
                    "tama_pea", "e01aimde", "rama_pea", "fex_2022", "horab")
  for (col in numeric_cols) {
    if (col %in% names(df)) {
      df[[col]] <- suppressWarnings(as.numeric(gsub(",", ".", df[[col]])))
    }
  }

  df$anio_file <- anio_file
  df$q_file <- q_file
  df
}

db <- bind_rows(mapply(read_personas, csv_meta$file, csv_meta$anio_file, csv_meta$q_file, SIMPLIFY = FALSE)) %>%
  mutate(
    anio = as.integer(anio_file),
    q = as.integer(q_file),
    trim_seq = (anio - min(ANIOS_ANALISIS)) * 4L + q,
    trimestredesc = paste0(anio, "Trim", q),
    t_idx = trim_seq
  )

# Peso (cambia según año/fuente: p.ej. `fex_2022` vs `Factor`)
if ("fex_2022" %in% names(db)) {
  db$w <- suppressWarnings(as.numeric(gsub(",", ".", db$fex_2022)))
} else if ("factor" %in% names(db)) {
  db$w <- suppressWarnings(as.numeric(gsub(",", ".", db$factor)))
} else {
  db$w <- 1
}

# Si todos son NA o hay muchos NAs, asignar 1
if (all(is.na(db$w)) || mean(is.na(db$w)) > 0.1) {
  db$w <- 1
  warning("Pesos missing detectados. Asignando w=1 a todos.")
} else if (any(is.na(db$w))) {
  # Reemplazar NAs con la media
  db$w[is.na(db$w)] <- mean(db$w, na.rm = TRUE)
}

# Eliminar filas con IDs missing para svydesign
db <- db %>% filter(!is.na(upm))

# -----------------------------
# Serie SML mensual (julio de cada año aplicado)
# -----------------------------
sml_data <- tibble::tribble(
  ~year, ~mes, ~sml,
  1980, 1, 20520, 1980, 7, 23610,
  1981, 5, 27180, 1983, 7, 29910,
  1984, 6, 34410, 1984, 9, 39570,
  1985, 2, 43530, 1985, 10, 52230,
  1986, 1, 60060, 1986, 7, 72072,
  1987, 1, 86486, 1987, 10, 103783,
  1988, 3, 119350, 1988, 10, 143220,
  1989, 6, 164640, 1990, 7, 213000,
  1990, 10, 244950, 1992, 7, 269445,
  1993, 4, 300000, 1994, 7, 379500,
  1995, 5, 436425, 1996, 4, 480068,
  1997, 1, 528076, 1998, 3, 591445,
  2000, 1, 680162, 2001, 5, 782186,
  2002, 8, 876048, 2003, 2, 972413,
  2005, 4, 1089103, 2006, 4, 1219795,
  2007, 10, 1341775, 2009, 5, 1408864,
  2010, 7, 1507484, 2011, 4, 1658232,
  2014, 3, 1824055, 2016, 12, 1964507,
  2017, 7, 2041123, 2018, 7, 2112562,
  2019, 7, 2192839, 2021, 7, 2289324,
  2022, 7, 2550307, 2023, 7, 2680373,
  2024, 7, 2798309,      # vigente desde 01/07/2024
  2025, 7, 2899048,      # vigente desde 01/07/2025 (Decreto)
  2026, 7, 2954130       # proyección preliminar jul/2026 (no oficial)
) %>%
  mutate(
    fecha = make_date(year, mes, 1),
    tipo = case_when(
      year == 2024 & mes == 7 ~ "Vigente desde 01/07/2024",
      year == 2025 & mes == 7 ~ "Vigente desde 01/07/2025",
      year == 2026 & mes == 7 ~ "Proyección preliminar (no oficial)",
      TRUE ~ NA_character_
    )
  )


end_year_sml <- max(c(max(ANIOS_ANALISIS), max(sml_data$year, na.rm = TRUE)))
end_sml <- as.Date(paste0(end_year_sml, "-12-01"))
serie_mensual <- tibble(fecha = seq.Date(as.Date("1980-01-01"), end_sml, by = "month"))
sml_mensual <- serie_mensual %>%
  left_join(select(sml_data, fecha, sml), by = "fecha") %>%
  tidyr::fill(sml, .direction = "down")

# Fecha intermedia por trimestre: feb/may/ago/nov
db <- db %>%
  mutate(
    mes_intermedio = c(2, 5, 8, 11)[q],
    fecha = make_date(anio, mes_intermedio, 1)
  ) %>%
  left_join(sml_mensual, by = "fecha")

# -----------------------------
# Recodificaciones clave
# -----------------------------
# sexo (EPHC suele codificar 1=Hombres, 6=Mujeres)
db <- db %>%
  mutate(
    sexo = case_when(p06 == 1 ~ "Hombres",
                     p06 == 6 ~ "Mujeres",
                     TRUE ~ NA_character_)
  )

# cotiza (b10): 1=Sí, 6=No (ajusta si tu diccionario difiere)
db <- db %>% mutate(cotizacaja = case_when(b10 == 1 ~ "Si",
                                           b10 == 6 ~ "No",
                                           TRUE ~ NA_character_),
                    cotiza_bin = as.integer(cotizacaja == "Si"))

# años de estudio: 99/98 = missing
db$anoest_val <- get_num(db, "anoest")
db$anoest_val[db$anoest_val %in% c(98, 99)] <- NA_real_

# área (1=urbano, 6=rural)
db <- db %>% mutate(area_urb = as.integer(area == 1))

# tamaño empresa (descarta 98/99)
db <- db %>% mutate(tam = ifelse(tama_pea %in% c(98, 99), NA, as.numeric(tama_pea)))

# rama de actividad
db <- db %>% mutate(rama_pea = as.integer(rama_pea))

# Ingreso relativo a SML y franjas
db <- db %>%
  mutate(
    ratio_sml = e01aimde / sml,
    ingoc1sml = case_when(
      ratio_sml < 0.90 ~ 0L,
      ratio_sml >= 0.90 & ratio_sml <= 1.10 ~ 1L,
      ratio_sml > 1.10 ~ 2L,
      TRUE ~ NA_integer_
    ),
    ingoc1sml_cat = factor(
      ingoc1sml,
      levels = 0:2,
      labels = c("Menos de 1 SML", "1 SML", "Más de 1 SML")
    ),
    # índice numérico de trimestre para gráficas/loess
    t_idx = trim_seq
  )

# -----------------------------
# DISEÑO DE ENCUESTA (EPHC)
# -----------------------------
# Usa estrato si existe; si no, sin estratificación
if ("estrato" %in% names(db)) {
  des_ephc <- svydesign(ids = ~upm, strata = ~estrato, weights = ~w, data = db, nest = TRUE)
} else {
  des_ephc <- svydesign(ids = ~upm, weights = ~w, data = db, nest = TRUE)
}

# -----------------------------
# TABLAS DESCRIPTIVAS (ponderadas) y EXPORT
# -----------------------------
# Distribución por franja SML y trimestre
dist_sml <- db %>%
  filter(!is.na(ingoc1sml_cat)) %>%
  group_by(trim_seq, trimestredesc, ingoc1sml_cat) %>%
  summarise(
    n_w = sum(w, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  group_by(trim_seq, trimestredesc) %>%
  mutate(pct = 100 * n_w / sum(n_w)) %>%
  ungroup() %>%
  arrange(trim_seq, ingoc1sml_cat) %>%
  select(-trim_seq)

write_csv(dist_sml, file.path(TAB_DIR, "dist_sml_trimestre.csv"))
print(xtable(dist_sml, caption = "Distribución ponderada por franja SML y trimestre",
             label = "tab:dist_sml"), file = file.path(TAB_DIR, "dist_sml_trimestre.tex"),
      include.rownames = FALSE)

# Proporción de mujeres por franja y trimestre (ponderada)
prop_muj <- db %>%
  filter(!is.na(ingoc1sml_cat), !is.na(sexo)) %>%
  group_by(trim_seq, trimestredesc, ingoc1sml_cat) %>%
  summarise(
    pct_muj = w_mean(as.integer(sexo == "Mujeres"), w),
    .groups = "drop"
  ) %>%
  arrange(trim_seq, ingoc1sml_cat) %>%
  select(-trim_seq)
write_csv(prop_muj, file.path(TAB_DIR, "prop_mujeres_sml.csv"))
prop_muj_tex <- prop_muj %>% mutate(pct_muj = 100 * pct_muj)
print(xtable(prop_muj_tex, caption = "Porcentaje de mujeres por franja SML",
             label = "tab:prop_muj"), file = file.path(TAB_DIR, "prop_mujeres_sml.tex"),
      include.rownames = FALSE)

# Edad y educación promedio por franja y trimestre
edad_edu <- db %>%
  filter(!is.na(ingoc1sml_cat)) %>%
  group_by(trim_seq, trimestredesc, ingoc1sml_cat) %>%
  summarise(
    edad_prom = w_mean(as.numeric(p02), w),
    anios_edu_prom = w_mean(anoest_val, w),
    .groups = "drop"
  ) %>%
  arrange(trim_seq, ingoc1sml_cat) %>%
  select(-trim_seq)
write_csv(edad_edu, file.path(TAB_DIR, "edad_edu_sml.csv"))
print(xtable(edad_edu, caption = "Edad y años de estudio promedio por franja SML",
             label = "tab:edad_edu"), file = file.path(TAB_DIR, "edad_edu_sml.tex"),
      include.rownames = FALSE)

# Proporción de cotizantes por franja y trimestre
prop_cotiza <- db %>%
  filter(!is.na(ingoc1sml_cat)) %>%
  group_by(trim_seq, trimestredesc, ingoc1sml_cat) %>%
  summarise(
    prop_cotiz = w_mean(as.integer(cotiza_bin == 1), w),
    .groups = "drop"
  ) %>%
  arrange(trim_seq, ingoc1sml_cat) %>%
  select(-trim_seq)
write_csv(prop_cotiza, file.path(TAB_DIR, "prop_cotiza_sml.csv"))
prop_cotiza_tex <- prop_cotiza %>% mutate(prop_cotiz = 100 * prop_cotiz)
print(xtable(prop_cotiza_tex, caption = "Porcentaje de cotizantes por franja SML",
             label = "tab:cotiza"), file = file.path(TAB_DIR, "prop_cotiza_sml.tex"),
      include.rownames = FALSE)

# Tamaño empresa promedio por franja y trimestre
tama <- db %>%
  filter(!is.na(ingoc1sml_cat), !is.na(tam)) %>%
  group_by(trim_seq, trimestredesc, ingoc1sml_cat) %>%
  summarise(
    tam_prom = w_mean(tam, w),
    .groups = "drop"
  ) %>%
  arrange(trim_seq, ingoc1sml_cat) %>%
  select(-trim_seq)
write_csv(tama, file.path(TAB_DIR, "tama_prom_sml.csv"))
print(xtable(tama, caption = "Tamaño promedio de empresa por franja SML",
             label = "tab:tama"), file = file.path(TAB_DIR, "tama_prom_sml.tex"),
      include.rownames = FALSE)

# Salario promedio por franja y género
sal_prom <- db %>%
  filter(!is.na(ingoc1sml_cat), !is.na(sexo)) %>%
  group_by(trim_seq, trimestredesc, ingoc1sml_cat, sexo) %>%
  summarise(
    sal_prom = w_mean(e01aimde, w),
    .groups = "drop"
  ) %>%
  arrange(trim_seq, ingoc1sml_cat, sexo) %>%
  select(-trim_seq)
write_csv(sal_prom, file.path(TAB_DIR, "salario_prom_sml.csv"))
print(xtable(sal_prom, caption = "Salario promedio por franja SML y género",
             label = "tab:sal_prom"), file = file.path(TAB_DIR, "salario_prom_sml.tex"),
      include.rownames = FALSE)

# Salario por hora promedio (ingreso mensual / (horas semanales * 52/12))
db <- db %>%
  mutate(
    horabc_val = as.numeric(horabc),
    horas_mes = horabc_val * (52 / 12),
    sal_hora = ifelse(is.finite(e01aimde) & is.finite(horas_mes) & horas_mes > 0,
                      e01aimde / horas_mes, NA_real_)
  )

sal_hora <- db %>%
  filter(!is.na(ingoc1sml_cat), !is.na(sexo), is.finite(sal_hora)) %>%
  group_by(trim_seq, trimestredesc, ingoc1sml_cat, sexo) %>%
  summarise(
    sal_hora_prom = w_mean(sal_hora, w),
    .groups = "drop"
  ) %>%
  arrange(trim_seq, ingoc1sml_cat, sexo) %>%
  select(-trim_seq)
write_csv(sal_hora, file.path(TAB_DIR, "salario_hora_prom_sml.csv"))
print(xtable(sal_hora, caption = "Salario por hora promedio por franja SML y género",
             label = "tab:sal_hora"), file = file.path(TAB_DIR, "salario_hora_prom_sml.tex"),
      include.rownames = FALSE)

# -----------------------------
# FIGURAS (guardamos PNG + PDF)
# -----------------------------
# Orden temporal en eje X
trim_levels <- db %>%
  distinct(trim_seq, trimestredesc) %>%
  arrange(trim_seq) %>%
  pull(trimestredesc)

dist_sml$trimestredesc <- factor(dist_sml$trimestredesc, levels = trim_levels)
prop_muj$trimestredesc <- factor(prop_muj$trimestredesc, levels = trim_levels)
edad_edu$trimestredesc <- factor(edad_edu$trimestredesc, levels = trim_levels)
prop_cotiza$trimestredesc <- factor(prop_cotiza$trimestredesc, levels = trim_levels)
tama$trimestredesc <- factor(tama$trimestredesc, levels = trim_levels)
sal_prom$trimestredesc <- factor(sal_prom$trimestredesc, levels = trim_levels)

# 1) Distribución % por franja SML
g1 <- ggplot(dist_sml, aes(trimestredesc, n_w, fill = ingoc1sml_cat)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = COL_SML, name = "Franja") +
  labs(
    title = "Distribución porcentual por franja SML",
    x = "Trimestre", y = "%", caption = CAPTION_BASE
  ) +
  THEME_SML()
save_plot(g1, "distribucion_sml", width = 10, height = 7)

# 2) % Mujeres por franja
g2 <- ggplot(prop_muj, aes(trimestredesc, pct_muj, color = ingoc1sml_cat, group = ingoc1sml_cat)) +
  geom_point(size = 2) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, span = 0.6, linewidth = 1.2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_color_manual(values = COL_SML, name = "Franja") +
  labs(title = "Proporción de mujeres por franja SML", x = "Trimestre", y = "% Mujeres", caption = CAPTION_BASE) +
  THEME_SML()
save_plot(g2, "porcen_mujeres_por_tramo_sml")

# 3) Edad promedio
g3 <- ggplot(edad_edu, aes(trimestredesc, edad_prom, color = ingoc1sml_cat, group = ingoc1sml_cat)) +
  geom_point(size = 2) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, span = 0.6, linewidth = 1.2) +
  scale_color_manual(values = COL_SML, name = "Franja") +
  labs(title = "Edad promedio por franja SML", x = "Trimestre", y = "Edad", caption = CAPTION_BASE) +
  THEME_SML()
save_plot(g3, "edad_promedio_por_tramo_sml")

# 4) Años de estudio promedio
g4 <- ggplot(edad_edu, aes(trimestredesc, anios_edu_prom, color = ingoc1sml_cat, group = ingoc1sml_cat)) +
  geom_point(size = 2) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, span = 0.6, linewidth = 1.2) +
  scale_color_manual(values = COL_SML, name = "Franja") +
  labs(title = "Años de estudio promedio por franja SML", x = "Trimestre", y = "Años de estudio", caption = CAPTION_BASE) +
  THEME_SML()
save_plot(g4, "prom_anioestudios_por_tramo_sml")

# 5) % Cotizantes
g5 <- ggplot(prop_cotiza, aes(trimestredesc, prop_cotiz, color = ingoc1sml_cat, group = ingoc1sml_cat)) +
  geom_point(size = 2) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, span = 0.6, linewidth = 1.2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_color_manual(values = COL_SML, name = "Franja") +
  labs(title = "Proporción de cotizantes por franja SML", x = "Trimestre", y = "% Cotizantes", caption = CAPTION_BASE) +
  THEME_SML()
save_plot(g5, "porcen_cotiz_por_tramo_sml")

# 6) Tamaño de empresa
g6 <- ggplot(tama, aes(trimestredesc, tam_prom, color = ingoc1sml_cat, group = ingoc1sml_cat)) +
  geom_point(size = 2) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, span = 0.6, linewidth = 1.2) +
  scale_color_manual(values = COL_SML, name = "Franja") +
  labs(title = "Tamaño promedio de empresa por franja SML", x = "Trimestre", y = "Promedio (tama_pea)", caption = CAPTION_BASE) +
  THEME_SML()
save_plot(g6, "prom_tamaempresa_por_tramo_sml")

# 7) Salario promedio
g7 <- ggplot(sal_prom, aes(trimestredesc, sal_prom, color = ingoc1sml_cat, group = ingoc1sml_cat)) +
  geom_line(linewidth = 1.1) + geom_point(size = 2) +
  facet_wrap(~sexo) +
  scale_color_manual(values = COL_SML, name = "Franja") +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  labs(title = "Salario promedio por franja SML y género", x = "Trimestre", y = "Salario (Gs.)", caption = CAPTION_BASE) +
  THEME_SML()
save_plot(g7, "salario_prom_por_tramo_sml", width = 10, height = 6)

# -----------------------------
# ECONOMETRÍA — svyglm (quasibinomial y LPM)
# -----------------------------
# Reestablece diseño con variables derivadas
des_ephc <- update(des_ephc,
                   edad = as.numeric(p02),
                   edu = anoest_val,
                   tam = db$tam,
                   area_urb = db$area_urb,
                   sexo = db$sexo,
                   ingoc1sml_cat = relevel(db$ingoc1sml_cat, ref = "1 SML"),
                   rama_pea = db$rama_pea
)

# 1) Cotizar ~ franja SML + controles (quasibinomial)
m_cotiza_qb <- svyglm(
  cotiza_bin ~ ingoc1sml_cat + edad + I(edad^2) + edu + sexo +
    tam + area_urb + factor(rama_pea) + factor(trimestredesc),
  design = des_ephc,
  family = quasibinomial()
)

# 2) LPM para interpretar en puntos porcentuales
m_cotiza_lpm <- svyglm(
  cotiza_bin ~ ingoc1sml_cat + edad + I(edad^2) + edu + sexo +
    tam + area_urb + factor(rama_pea) + factor(trimestredesc),
  design = des_ephc,
  family = gaussian()
)

# Export tablas de resultados (LaTeX y HTML)
modelsummary::modelsummary(
  list(`Quasi-binomial` = m_cotiza_qb, `LPM` = m_cotiza_lpm),
  output = file.path(TAB_DIR, "svyglm_cotiza.tex"),
  gof_omit = "IC|AIC|BIC"
)
modelsummary::modelsummary(
  list(`Quasi-binomial` = m_cotiza_qb, `LPM` = m_cotiza_lpm),
  output = file.path(TAB_DIR, "svyglm_cotiza.html"),
  gof_omit = "IC|AIC|BIC"
)

# -----------------------------
# EVENT STUDY (alrededor de julio; t_rel con ref = -1)
# -----------------------------
db_es <- db %>%
  mutate(
    # Q3≈agosto → t_rel=0 en 2022Trim3; ventana [-4,4]
    t_rel = (anio - 2022L) * 4L + (q - 3L),
    y_formal = cotiza_bin,
    y_1sml = as.integer(ingoc1sml_cat == "1 SML")
  ) %>%
  filter(!is.na(t_rel), abs(t_rel) <= 4L)

# FE territoriales: usa estgeo si existe; si no, dptorep; si no, area
fe_terr <- if ("estgeo" %in% names(db_es)) "estgeo" else if ("dptorep" %in% names(db_es)) "dptorep" else "area"

# Modelo sobre formalidad
fml1 <- as.formula(paste0(
  "y_formal ~ i(t_rel, ref=-1) + sexo + as.numeric(p02) + I(as.numeric(p02)^2) + ",
  "anoest_val + area_urb | ", fe_terr
))

est1 <- fixest::feols(
  fml1,
  data = db_es,
  weights = ~ w,
  cluster = ~ upm
)

# Tabla coeficientes event study
etable(est1, se = "cluster",
       file = file.path(TAB_DIR, "event_study_formalidad.txt"))

# Gráfico coeficientes (guardamos como PDF y PNG)
pdf(file.path(FIG_DIR, "event_study_formalidad.pdf"), width = 8, height = 5)
iplot(est1, main = "Event study: formalidad alrededor de ajustes del SML",
      xlab = "Trimestres relativos al ajuste (ref = -1)",
      ylab = "Diferencia (pp) vs. ref")
dev.off()
png(file.path(FIG_DIR, "event_study_formalidad.png"), width = 1000, height = 600, res = 150)
iplot(est1, main = "Event study: formalidad alrededor de ajustes del SML",
      xlab = "Trimestres relativos al ajuste (ref = -1)",
      ylab = "Diferencia (pp) vs. ref")
dev.off()

# -----------------------------
# DISTRIBUCIONAL / BUNCHING "proxy"
# -----------------------------
dens_df <- db %>%
  filter(is.finite(ratio_sml), ratio_sml > 0, ratio_sml < 3, is.finite(w)) %>%
  mutate(bin = cut(ratio_sml, breaks = seq(0, 3, by = 0.02), include.lowest = TRUE)) %>%
  group_by(bin) %>%
  summarise(w_mass = sum(w, na.rm = TRUE), .groups = "drop") %>%
  mutate(mid = as.numeric(sub("^[\\[(]([^,]+),.*$", "\\1", as.character(bin))) + 0.01) %>%
  filter(!is.na(mid))

g_bunch <- ggplot(dens_df, aes(mid, w_mass / sum(w_mass))) +
  geom_col(width = 0.02, fill = "#1565C0", alpha = 0.8) +
  geom_vline(xintercept = 1.00, linetype = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(
    title = "Masa salarial ponderada alrededor del SML",
    x = "Ingreso / SML", y = "Proporción", caption = CAPTION_BASE
  ) +
  THEME_SML()
save_plot(g_bunch, "densidad_ratio_sml")

share_spike <- db %>%
  filter(is.finite(ratio_sml), ratio_sml > 0, ratio_sml < 3, is.finite(w)) %>%
  summarise(
    pct_spike = 100 * sum(w * (ratio_sml >= 0.98 & ratio_sml <= 1.02), na.rm = TRUE) /
      sum(w, na.rm = TRUE)
  )
write_csv(share_spike, file.path(TAB_DIR, "share_spike_around_SML.csv"))

# Tabla de composición por franja
comp_sml_tab <- db %>%
  filter(!is.na(ingoc1sml_cat)) %>%
  group_by(trimestredesc) %>%
  mutate(
    pct_menos1 = 100 * sum(w * (ingoc1sml_cat == "Menos de 1 SML"), na.rm = TRUE) / sum(w, na.rm = TRUE),
    pct_1      = 100 * sum(w * (ingoc1sml_cat == "1 SML"), na.rm = TRUE) / sum(w, na.rm = TRUE),
    pct_mas1   = 100 * sum(w * (ingoc1sml_cat == "Más de 1 SML"), na.rm = TRUE) / sum(w, na.rm = TRUE)
  ) %>%
  select(trimestredesc, pct_menos1, pct_1, pct_mas1) %>%
  distinct()

write_csv(comp_sml_tab, file.path(TAB_DIR, "comp_sml.csv"))
print(xtable(comp_sml_tab, caption = "Composición porcentual por franja SML",
             label = "tab:comp_sml"), file = file.path(TAB_DIR, "comp_sml.tex"),
      include.rownames = FALSE)

# -----------------------------
# Session info
# -----------------------------
writeLines(capture.output(sessionInfo()), con = file.path(OUT_DIR, "session_info.txt"))

message("✅ Listo (R02 ONLY). Tablas en: ", normalizePath(TAB_DIR), " | Figuras en: ", normalizePath(FIG_DIR))
