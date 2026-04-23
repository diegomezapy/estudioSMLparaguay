#!/usr/bin/env Rscript

# =====================================================================
# ESTUDIO SALARIO MINIMO LEGAL PARAGUAY - APLICACION SHINY
# Dashboard Interactivo (2022-2025)
# =====================================================================

cat("\n[INFO] Inicializando Shiny App...\n")

library(shiny)
library(shinydashboard)
library(plotly)
library(readr)
library(readxl)
library(DT)
library(dplyr)
library(stringr)
library(lubridate)
library(haven)

# Detectar directorio
BASE_DIR <- {
  args <- commandArgs(trailingOnly = FALSE)
  match_idx <- grep("--file=", args)
  if (length(match_idx) > 0) {
    dirname(sub("--file=", "", args[match_idx]))
  } else {
    tryCatch({
      dirname(rstudioapi::getActiveDocumentContext()$path)
    }, error = function(e) {
      getwd()
    })
  }
}
setwd(BASE_DIR)

# ---------------------------------------------------------------------
# Helpers generales
# ---------------------------------------------------------------------
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || is.na(x)) y else x

safe_mean <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

safe_sum <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return(NA_real_)
  sum(x, na.rm = TRUE)
}

weighted_share_safe <- function(cond, w) {
  idx <- !is.na(cond) & !is.na(w)
  if (!any(idx)) return(NA_real_)
  sw <- sum(w[idx], na.rm = TRUE)
  if (is.na(sw) || sw == 0) return(NA_real_)
  100 * sum(as.numeric(cond[idx]) * w[idx], na.rm = TRUE) / sw
}

month_diff <- function(fecha, fecha_evento) {
  12L * (lubridate::year(fecha) - lubridate::year(fecha_evento)) +
    (lubridate::month(fecha) - lubridate::month(fecha_evento))
}

clean_names_simple <- function(nm) {
  nm <- iconv(nm, to = "ASCII//TRANSLIT")
  nm <- tolower(gsub("[^a-zA-Z0-9]+", "_", nm))
  nm <- gsub("^_+|_+$", "", nm)
  nm
}

pick_col <- function(cols, candidates, required = TRUE, label = "") {
  hit <- intersect(candidates, cols)
  if (length(hit) == 0) {
    if (!required) return(NA_character_)
    stop(
      "No se encontro columna para '", label, "'. Candidatas: ",
      paste(candidates, collapse = ", ")
    )
  }
  hit[1]
}

parse_num_localized <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))

  s <- trimws(as.character(x))
  s[s %in% c("", "NA", "NaN", "NULL", "N/A", "na", "nan", "null")] <- NA_character_

  s <- str_replace_all(s, "\\s+", "")
  s <- str_replace_all(s, fixed("\u00A0"), "")

  has_comma <- str_detect(s, ",")
  has_dot <- str_detect(s, "\\.")
  both <- !is.na(has_comma) & !is.na(has_dot) & has_comma & has_dot

  s[both] <- str_replace_all(s[both], "\\.", "")
  s <- str_replace_all(s, ",", ".")

  suppressWarnings(as.numeric(s))
}

parse_pct_points <- function(x) {
  if (is.numeric(x)) {
    num <- as.numeric(x)
    return(ifelse(is.na(num), NA_real_, ifelse(abs(num) <= 1.5, num * 100, num)))
  }

  raw <- trimws(as.character(x))
  raw[raw %in% c("", "NA", "NaN", "NULL", "N/A", "na", "nan", "null")] <- NA_character_

  has_pct <- str_detect(raw, "%")
  cleaned <- str_replace_all(raw, "%", "")
  num <- parse_num_localized(cleaned)

  ifelse(
    is.na(num),
    NA_real_,
    ifelse(has_pct, num, ifelse(abs(num) <= 1.5, num * 100, num))
  )
}

parse_periodo <- function(x) {
  s <- trimws(tolower(as.character(x)))
  s[s %in% c("", "na", "nan", "null", "n/a")] <- NA_character_
  out <- rep(as.Date(NA), length(s))

  idx_ym <- !is.na(s) & str_detect(s, "^[0-9]{4}[-/][0-9]{1,2}$")
  if (any(idx_ym)) {
    parts <- str_split_fixed(s[idx_ym], "[-/]", 2)
    out[idx_ym] <- as.Date(sprintf("%04d-%02d-01", as.integer(parts[, 1]), as.integer(parts[, 2])))
  }

  idx_mes_yy <- is.na(out) & !is.na(s) & str_detect(s, "^[a-záéíóúñ]+-[0-9]{2}$")
  if (any(idx_mes_yy)) {
    mes_txt <- str_extract(s[idx_mes_yy], "^[a-záéíóúñ]+")
    anio2 <- str_extract(s[idx_mes_yy], "[0-9]{2}$")

    mes_num <- dplyr::case_when(
      mes_txt %in% c("ene") ~ 1L,
      mes_txt %in% c("feb") ~ 2L,
      mes_txt %in% c("mar") ~ 3L,
      mes_txt %in% c("abr") ~ 4L,
      mes_txt %in% c("may") ~ 5L,
      mes_txt %in% c("jun") ~ 6L,
      mes_txt %in% c("jul") ~ 7L,
      mes_txt %in% c("ago") ~ 8L,
      mes_txt %in% c("sep", "sept") ~ 9L,
      mes_txt %in% c("oct") ~ 10L,
      mes_txt %in% c("nov") ~ 11L,
      mes_txt %in% c("dic") ~ 12L,
      TRUE ~ NA_integer_
    )

    anio2_num <- suppressWarnings(as.integer(anio2))
    anio4 <- dplyr::case_when(
      is.na(anio2_num) ~ NA_integer_,
      anio2_num >= 90 ~ 1900L + anio2_num,
      TRUE ~ 2000L + anio2_num
    )

    out[idx_mes_yy] <- as.Date(sprintf("%04d-%02d-01", anio4, mes_num))
  }

  out
}

flag_overlap_events <- function(fechas, h) {
  if (length(fechas) <= 1) return(rep(FALSE, length(fechas)))
  vapply(seq_along(fechas), function(i) {
    dif <- month_diff(fechas, fechas[i])
    any(dif != 0 & dif >= -h & dif <= (h - 1))
  }, logical(1))
}

calc_event_metrics <- function(datos, eventos, h) {
  if (nrow(eventos) == 0) return(tibble())

  out <- lapply(seq_len(nrow(eventos)), function(i) {
    fe <- eventos$fecha[i]
    adj <- eventos$ajuste_pp[i]
    rel <- month_diff(datos$fecha, fe)

    pre <- datos[rel >= -h & rel <= -1, ]
    post <- datos[rel >= 0 & rel <= (h - 1), ]

    infl_gen_pre <- safe_mean(pre$ipc_general_m)
    infl_gen_post <- safe_mean(post$ipc_general_m)
    infl_alim_pre <- safe_mean(pre$ipc_alim_m)
    infl_alim_post <- safe_mean(post$ipc_alim_m)

    ei_gen <- infl_gen_post - infl_gen_pre
    ei_alim <- infl_alim_post - infl_alim_pre

    data.frame(
      id_evento = eventos$id_evento[i],
      fecha_evento = fe,
      ajuste_pp = adj,
      n_pre = nrow(pre),
      n_post = nrow(post),
      infl_gen_pre = infl_gen_pre,
      infl_gen_post = infl_gen_post,
      infl_alim_pre = infl_alim_pre,
      infl_alim_post = infl_alim_post,
      ei_gen = ei_gen,
      ei_alim = ei_alim,
      ct_gen = ifelse(is.na(adj) || adj == 0, NA_real_, ei_gen / adj),
      ct_alim = ifelse(is.na(adj) || adj == 0, NA_real_, ei_alim / adj),
      persist_gen = safe_sum(post$ipc_general_m > infl_gen_pre),
      persist_alim = safe_sum(post$ipc_alim_m > infl_alim_pre),
      stringsAsFactors = FALSE
    )
  })

  bind_rows(out)
}

calc_event_average <- function(datos, eventos, h) {
  if (nrow(eventos) == 0) return(tibble())

  cross <- merge(
    eventos[, c("id_evento", "fecha")],
    datos[, c("fecha", "ipc_general_m", "ipc_alim_m")],
    by = NULL
  )
  names(cross)[names(cross) == "fecha.x"] <- "fecha_evento"
  names(cross)[names(cross) == "fecha.y"] <- "fecha"

  cross$rel_month <- month_diff(cross$fecha, cross$fecha_evento)
  cross <- cross[cross$rel_month >= -h & cross$rel_month <= h, ]

  cross %>%
    group_by(rel_month) %>%
    summarise(
      ipc_general_m = safe_mean(ipc_general_m),
      ipc_alim_m = safe_mean(ipc_alim_m),
      n_eventos = n_distinct(id_evento),
      .groups = "drop"
    ) %>%
    arrange(rel_month)
}

calc_ei_idx <- function(series, idx, h) {
  pre <- series[(idx - h):(idx - 1)]
  post <- series[idx:(idx + h - 1)]
  safe_mean(post) - safe_mean(pre)
}

calc_placebo <- function(datos, eventos, h, reps = 1000, seed = 123) {
  n <- nrow(datos)
  idx_events <- match(eventos$fecha, datos$fecha)
  idx_obs <- idx_events[idx_events > h & idx_events <= (n - (h - 1))]

  idx_candidates <- seq.int(h + 1, n - (h - 1))
  idx_candidates <- setdiff(idx_candidates, idx_events)

  if (length(idx_obs) == 0 || length(idx_candidates) < length(idx_obs)) {
    return(list(
      obs_gen = NA_real_,
      obs_alim = NA_real_,
      placebo_gen = numeric(0),
      placebo_alim = numeric(0),
      p_gen = NA_real_,
      p_alim = NA_real_
    ))
  }

  obs_gen <- safe_mean(vapply(idx_obs, function(i) calc_ei_idx(datos$ipc_general_m, i, h), numeric(1)))
  obs_alim <- safe_mean(vapply(idx_obs, function(i) calc_ei_idx(datos$ipc_alim_m, i, h), numeric(1)))

  set.seed(seed + h)
  placebo_gen <- rep(NA_real_, reps)
  placebo_alim <- rep(NA_real_, reps)

  for (b in seq_len(reps)) {
    idx_s <- sample(idx_candidates, size = length(idx_obs), replace = FALSE)
    placebo_gen[b] <- safe_mean(vapply(idx_s, function(i) calc_ei_idx(datos$ipc_general_m, i, h), numeric(1)))
    placebo_alim[b] <- safe_mean(vapply(idx_s, function(i) calc_ei_idx(datos$ipc_alim_m, i, h), numeric(1)))
  }

  list(
    obs_gen = obs_gen,
    obs_alim = obs_alim,
    placebo_gen = placebo_gen,
    placebo_alim = placebo_alim,
    p_gen = safe_mean(abs(placebo_gen) >= abs(obs_gen)),
    p_alim = safe_mean(abs(placebo_alim) >= abs(obs_alim))
  )
}

simulate_shock_path <- function(base_gen, base_alim, adj_pp, beta_gen, beta_alim, phi, months) {
  t <- 0:months
  shock <- adj_pp * (phi ^ t)
  tibble(
    mes = t,
    ipc_general_sim = base_gen + beta_gen * shock,
    ipc_alim_sim = base_alim + beta_alim * shock
  )
}

parse_qyear_from_name_r01 <- function(fp) {
  b <- tolower(basename(fp))

  # Formato 1: REG01_EPHC_T1_2024.SAV
  m1 <- stringr::str_match(b, "t([1-4])_(20[0-9]{2})")
  q1 <- suppressWarnings(as.integer(m1[, 2]))
  y1 <- suppressWarnings(as.integer(m1[, 3]))
  if (!is.na(q1) && !is.na(y1)) return(list(year = y1, quarter = q1))

  # Formato 2: REG01_EPHC2025_T1.SAV
  m2 <- stringr::str_match(b, "(20[0-9]{2})_t([1-4])")
  y2 <- suppressWarnings(as.integer(m2[, 2]))
  q2 <- suppressWarnings(as.integer(m2[, 3]))
  if (!is.na(q2) && !is.na(y2)) return(list(year = y2, quarter = q2))

  list(year = NA_integer_, quarter = NA_integer_)
}

build_context_series <- function(db_micro, base_dir) {
  # R02 (micro persona) desde db_analisis.csv
  ctx_r02 <- db_micro %>%
    filter(!is.na(trimestredesc)) %>%
    group_by(trimestredesc) %>%
    summarise(
      pct_1sml = 100 * mean(ingoc1sml_cat == "1 SML", na.rm = TRUE),
      pct_cotiza = 100 * mean(cotiza_bin, na.rm = TRUE),
      n_personas = n(),
      .groups = "drop"
    ) %>%
    mutate(
      year = suppressWarnings(as.integer(substr(trimestredesc, 1, 4))),
      quarter = suppressWarnings(as.integer(stringr::str_extract(trimestredesc, "(?<=Trim)\\d+")))
    )

  # R01 (viviendas): buscar en dos posibles carpetas
  r01_dirs <- c(
    file.path(base_dir, "data", "viviendas"),
    file.path(base_dir, "estudioSMLparaguay", "data", "viviendas")
  )
  r01_dirs <- unique(r01_dirs[dir.exists(r01_dirs)])

  r01_files <- unlist(lapply(r01_dirs, function(dd) {
    list.files(dd, pattern = "REG01.*\\.(sav|SAV)$", full.names = TRUE)
  }))
  r01_files <- unique(r01_files)
  if (length(r01_files) > 0) {
    meta <- lapply(r01_files, parse_qyear_from_name_r01)
    r01_map <- data.frame(
      file = r01_files,
      year = vapply(meta, function(z) z$year, integer(1)),
      quarter = vapply(meta, function(z) z$quarter, integer(1)),
      dir_priority = match(dirname(r01_files), r01_dirs),
      stringsAsFactors = FALSE
    )
    r01_map <- r01_map[!is.na(r01_map$year) & !is.na(r01_map$quarter), ]
    r01_map <- r01_map[order(r01_map$year, r01_map$quarter, r01_map$dir_priority, r01_map$file), ]
    r01_map <- r01_map[!duplicated(paste(r01_map$year, r01_map$quarter)), ]
    r01_files <- r01_map$file
  }

  if (length(r01_files) == 0) {
    return(ctx_r02 %>%
      mutate(
        pct_hogares_material_apto = NA_real_,
        pct_hogares_internet = NA_real_,
        pct_hogares_movilidad = NA_real_,
        n_hogares = NA_real_
      ) %>%
      arrange(year, quarter))
  }

  r01_res <- lapply(r01_files, function(fp) {
    meta <- parse_qyear_from_name_r01(fp)
    if (is.na(meta$year) || is.na(meta$quarter)) return(NULL)

    dd <- haven::read_sav(fp)
    names(dd) <- tolower(names(dd))

    req_cols <- c("v03", "v04", "v05", "v23b", "v2413", "v2414")
    if (!all(req_cols %in% names(dd))) return(NULL)

    for (cc in req_cols) dd[[cc]] <- suppressWarnings(as.numeric(dd[[cc]]))

    get_num_col <- function(df, nm) {
      if (!nm %in% names(df)) return(rep(NA_real_, nrow(df)))
      suppressWarnings(as.numeric(df[[nm]]))
    }

    w <- dplyr::coalesce(
      get_num_col(dd, "fex.2022"),
      get_num_col(dd, "fex_2022"),
      get_num_col(dd, "fex2022"),
      rep(1, nrow(dd))
    )

    pared_apto <- dd$v03 %in% c(4, 5)
    piso_apto <- dd$v04 %in% c(3, 5, 6)
    techo_apto <- dd$v05 %in% c(1, 3, 4, 6)
    vivienda_material_apto <- pared_apto & piso_apto & techo_apto
    tiene_internet <- dd$v23b == 1
    movilidad <- dd$v2413 == 1 | dd$v2414 == 1

    tibble(
      year = meta$year,
      quarter = meta$quarter,
      n_hogares = nrow(dd),
      pct_hogares_material_apto = weighted_share_safe(vivienda_material_apto, w),
      pct_hogares_internet = weighted_share_safe(tiene_internet, w),
      pct_hogares_movilidad = weighted_share_safe(movilidad, w)
    )
  })

  ctx_r01 <- bind_rows(r01_res)

  ctx <- full_join(
    ctx_r02 %>% select(-trimestredesc),
    ctx_r01,
    by = c("year", "quarter")
  ) %>%
    filter(!is.na(year), !is.na(quarter)) %>%
    mutate(
      trimestredesc = paste0(year, "Trim", quarter)
    ) %>%
    arrange(year, quarter)

  ctx
}

prepare_spiral_data <- function(path) {
  if (!file.exists(path)) {
    stop("No existe la base de espiral en: ", path)
  }

  raw <- readxl::read_excel(path)
  names(raw) <- clean_names_simple(names(raw))

  cols <- names(raw)

  c_periodo <- pick_col(cols, c("ano", "periodo", "fecha"), TRUE, "periodo")
  c_alim <- pick_col(cols, c("alimentacion_bebidas_no_alchoholicas", "alim_indice", "indice_alimentos"), TRUE, "indice alimentos")
  c_gen <- pick_col(cols, c("indice_general", "general_indice"), TRUE, "indice general")
  c_aj <- pick_col(cols, c("ajuste", "percent_ajuste", "x_ajuste", "ajuste_pct"), TRUE, "ajuste")

  datos <- tibble(
    periodo_txt = raw[[c_periodo]],
    alim_indice = parse_num_localized(raw[[c_alim]]),
    general_indice = parse_num_localized(raw[[c_gen]]),
    ajuste_pp = parse_pct_points(raw[[c_aj]])
  ) %>%
    mutate(
      fecha = parse_periodo(periodo_txt)
    ) %>%
    filter(!is.na(fecha)) %>%
    arrange(fecha) %>%
    mutate(
      ipc_alim_m = 100 * (alim_indice / dplyr::lag(alim_indice) - 1),
      ipc_general_m = 100 * (general_indice / dplyr::lag(general_indice) - 1),
      brecha_m = ipc_alim_m - ipc_general_m
    )

  eventos <- datos %>%
    filter(!is.na(ajuste_pp), ajuste_pp > 0) %>%
    transmute(
      id_evento = row_number(),
      fecha,
      ajuste_pp
    )

  list(datos = datos, eventos = eventos, ruta = path)
}

# ---------------------------------------------------------------------
# Carga de datos base de la app
# ---------------------------------------------------------------------

# Datos micro de app original
if (!file.exists("data/db_analisis.csv")) {
  stop("No existe data/db_analisis.csv. Ejecutar data-prep.R primero.")
}

db <- read_csv("data/db_analisis.csv", show_col_types = FALSE)
context_series <- build_context_series(db, BASE_DIR)

# Datos de espiral
spiral_path <- file.path(BASE_DIR, "Efecto_espiralSML", "Data_Original.xlsx")
spiral_obj <- tryCatch(
  prepare_spiral_data(spiral_path),
  error = function(e) structure(list(message = conditionMessage(e), path = spiral_path), class = "spiral_error")
)
spiral_ok <- !inherits(spiral_obj, "spiral_error")

# Colores
color_map <- c(
  "Menos de 1 SML" = "#c0392b",
  "1 SML" = "#f5c542",
  "Más de 1" = "#1e8449",
  "NA" = "#bbbbbb"
)

# =====================================================================
# UI
# =====================================================================

ui <- dashboardPage(
  dashboardHeader(title = "Estudio SML Paraguay"),
  dashboardSidebar(
    h4("Filtros microdatos"),
    selectInput(
      "trim_sel", "Trimestres:",
      choices = sort(unique(db$trimestredesc)),
      selected = sort(unique(db$trimestredesc)), multiple = TRUE
    ),
    selectInput(
      "sexo_sel", "Genero:",
      choices = c("Todos", unique(na.omit(db$sexo))), selected = "Todos"
    ),
    selectInput(
      "franja_sel", "Franja:",
      choices = c("Todas", unique(na.omit(db$ingoc1sml_cat))), selected = "Todas"
    ),
    tags$hr(),
    p("La nueva vista econometrica esta en la pestana 'Espiral'.")
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("\n        .small-note { font-size: 12px; color: #5f6a6a; }\n        .hero-box { background: linear-gradient(120deg, #f7fbff 0%, #eef6f0 100%); border-radius: 10px; padding: 14px 16px; border: 1px solid #dfe6e9; margin-bottom: 14px; }\n        .metric-label { font-size: 13px; color: #566573; }\n      "))
    ),
    tabsetPanel(
      tabPanel(
        "Dashboard",
        fluidRow(
          valueBoxOutput("vbox1", width = 3),
          valueBoxOutput("vbox2", width = 3),
          valueBoxOutput("vbox3", width = 3),
          valueBoxOutput("vbox4", width = 3)
        ),
        fluidRow(
          column(6, plotlyOutput("plot1")),
          column(6, plotlyOutput("plot2"))
        )
      ),
      tabPanel("Franjas", plotlyOutput("plot3"), dataTableOutput("tbl1")),
      tabPanel(
        "Espiral",
        uiOutput("espiral_status"),
        div(
          class = "hero-box",
          h4("Tablero Dinamico: Espiral Salario-Precios"),
          p("Esta vista permite probar hipotesis econometricas con controles interactivos: horizonte de evento, filtro de shocks, limpieza de solapamientos, placebo Monte Carlo y simulador de traspaso.")
        ),
        fluidRow(
          column(
            width = 3,
            box(
              width = 12,
              title = "Controles del Analisis",
              status = "primary",
              solidHeader = TRUE,
              sliderInput("esp_h", "Horizonte pre/post (meses)", min = 2, max = 12, value = 6, step = 1),
              sliderInput("esp_min_adj", "Ajuste salarial minimo (pp)", min = 0, max = 20, value = 0, step = 0.5),
              radioButtons(
                "esp_mode", "Modo de lectura",
                choices = c("Analitico" = "analitico", "Presentacion guiada" = "presentacion"),
                selected = "analitico"
              ),
              conditionalPanel(
                condition = "input.esp_mode == 'presentacion'",
                sliderInput("esp_story_step", "Paso de la historia", min = 1, max = 5, value = 1, step = 1)
              ),
              radioButtons(
                "esp_overlap", "Eventos cercanos",
                choices = c("Incluir todos" = "all", "Excluir solapados" = "clean"),
                selected = "all"
              ),
              checkboxInput("esp_use_food", "Mostrar IPC alimentos", TRUE),
              checkboxGroupInput(
                "esp_ctx_vars", "Indicadores de contexto",
                choices = c(
                  "% en 1 SML (R02)" = "pct_1sml",
                  "% formalidad laboral (R02)" = "pct_cotiza",
                  "% hogares con internet (R01)" = "pct_hogares_internet",
                  "% hogares con material adecuado (R01)" = "pct_hogares_material_apto",
                  "% hogares con movilidad (R01)" = "pct_hogares_movilidad"
                ),
                selected = c("pct_1sml", "pct_cotiza", "pct_hogares_internet")
              ),
              sliderInput("esp_placebo_reps", "Placebo Monte Carlo (reps)", min = 200, max = 5000, value = 1000, step = 100),
              actionButton("esp_run", "Actualizar analisis", class = "btn-success"),
              tags$hr(),
              h4("Simulador de Shock"),
              sliderInput("sim_adj", "Shock salarial hipotetico (pp)", min = 1, max = 20, value = 8, step = 0.5),
              sliderInput("sim_beta_gen", "CT IPC general", min = -0.2, max = 0.5, value = 0.05, step = 0.01),
              sliderInput("sim_beta_alim", "CT IPC alimentos", min = -0.2, max = 0.8, value = 0.10, step = 0.01),
              sliderInput("sim_phi", "Persistencia mensual", min = 0, max = 0.95, value = 0.50, step = 0.05),
              sliderInput("sim_months", "Meses simulados", min = 3, max = 18, value = 12, step = 1)
            )
          ),
          column(
            width = 9,
            fluidRow(
              valueBoxOutput("esp_vbox1", width = 3),
              valueBoxOutput("esp_vbox2", width = 3),
              valueBoxOutput("esp_vbox3", width = 3),
              valueBoxOutput("esp_vbox4", width = 3)
            ),
            fluidRow(
              column(7, box(width = 12, title = "Evento Promedio", status = "info", solidHeader = TRUE, plotlyOutput("esp_plot_event", height = 320))),
              column(5, box(width = 12, title = "Diagnostico Placebo", status = "warning", solidHeader = TRUE, plotlyOutput("esp_plot_placebo", height = 320)))
            ),
            fluidRow(
              column(6, box(width = 12, title = "Simulador de Traspaso", status = "success", solidHeader = TRUE, plotlyOutput("esp_plot_sim", height = 300))),
              column(6, box(width = 12, title = "Contexto Laboral y Hogares (R02 + R01)", status = "primary", solidHeader = TRUE, plotlyOutput("esp_plot_micro", height = 300)))
            ),
            fluidRow(
              column(12, box(width = 12, title = "Guion de Presentacion", status = "warning", solidHeader = TRUE, htmlOutput("esp_story_panel")))
            ),
            fluidRow(
              column(12, box(width = 12, title = "Lectura Guiada", status = "primary", solidHeader = TRUE, htmlOutput("esp_text")))
            ),
            fluidRow(
              column(12, box(width = 12, title = "Metricas por Evento", status = "info", solidHeader = TRUE, dataTableOutput("esp_tbl_events")))
            )
          )
        )
      ),
      tabPanel("Datos", downloadButton("dl", "Descargar CSV"), dataTableOutput("tbl2"))
    )
  )
)

# =====================================================================
# SERVER
# =====================================================================

server <- function(input, output, session) {

  # Datos filtrados micro (tabs originales)
  datos <- reactive({
    d <- db
    if (!is.null(input$trim_sel)) d <- d[d$trimestredesc %in% input$trim_sel, ]
    if (input$sexo_sel != "Todos") d <- d[d$sexo == input$sexo_sel, ]
    if (input$franja_sel != "Todas") d <- d[d$ingoc1sml_cat == input$franja_sel, ]
    d
  })

  # -------------------------------------------------------------------
  # Outputs tabs originales
  # -------------------------------------------------------------------
  output$vbox1 <- renderValueBox({
    valueBox(nrow(datos()), "Registros", icon = icon("users"), color = "blue")
  })

  output$vbox2 <- renderValueBox({
    sal <- mean(datos()$salario_prom, na.rm = TRUE)
    valueBox(paste0("Gs ", format(round(sal, 0), big.mark = ".", scientific = FALSE)), "Sal. Promedio", icon = icon("money-bill"), color = "green")
  })

  output$vbox3 <- renderValueBox({
    pct <- 100 * mean(datos()$cotiza_bin, na.rm = TRUE)
    valueBox(paste0(round(pct, 1), "%"), "Formalidad", icon = icon("briefcase"), color = "orange")
  })

  output$vbox4 <- renderValueBox({
    d <- datos()
    pct <- 100 * sum(d$ingoc1sml_cat == "1 SML", na.rm = TRUE) / max(nrow(d), 1)
    valueBox(paste0(round(pct, 1), "%"), "Franja 1 SML", icon = icon("chart-pie"), color = "red")
  })

  output$plot1 <- renderPlotly({
    d <- datos()
    d <- d[!is.na(d$ingoc1sml_cat), ]

    tmp <- aggregate(list(n = rep(1, nrow(d))), list(trim = d$trimestredesc, franja = d$ingoc1sml_cat), length)
    tmp$pct <- NA_real_
    for (t in unique(tmp$trim)) {
      idx <- tmp$trim == t
      tmp$pct[idx] <- 100 * tmp$n[idx] / sum(tmp$n[idx])
    }

    p <- plot_ly(data = tmp, x = ~trim, y = ~pct, color = ~franja, type = "bar", colors = color_map)
    layout(
      p,
      title = "Distribucion por Franja",
      barmode = "stack",
      xaxis = list(title = "Trimestre"),
      yaxis = list(title = "Porcentaje (%)"),
      hovermode = "x unified"
    )
  })

  output$plot2 <- renderPlotly({
    d <- datos()
    d <- d[!is.na(d$ingoc1sml_cat), ]

    tmp <- aggregate(
      list(cotiza = d$cotiza_bin),
      list(trim = d$trimestredesc, franja = d$ingoc1sml_cat),
      function(x) 100 * mean(x, na.rm = TRUE)
    )

    p <- plot_ly(data = tmp, x = ~trim, y = ~cotiza, color = ~franja, type = "scatter", mode = "lines+markers", colors = color_map)
    layout(
      p,
      title = "Cotizacion por Franja",
      xaxis = list(title = "Trimestre"),
      yaxis = list(title = "% que Cotiza"),
      hovermode = "x unified"
    )
  })

  output$plot3 <- renderPlotly({
    d <- datos()
    d <- d[!is.na(d$ingoc1sml_cat), ]

    tmp <- aggregate(list(n = rep(1, nrow(d))), list(trim = d$trimestredesc, franja = d$ingoc1sml_cat), length)

    p <- plot_ly(data = tmp, x = ~trim, y = ~n, color = ~franja, type = "bar", colors = color_map)
    layout(
      p,
      title = "Cantidad de Trabajadores",
      barmode = "group",
      xaxis = list(title = "Trimestre"),
      yaxis = list(title = "N"),
      hovermode = "x unified"
    )
  })

  output$tbl1 <- renderDataTable({
    d <- datos()
    d <- d[!is.na(d$ingoc1sml_cat), ]

    tmp <- aggregate(
      list(n = rep(1, nrow(d)), salario = d$salario_prom, cotiza = d$cotiza_bin),
      list(trim = d$trimestredesc, franja = d$ingoc1sml_cat),
      function(x) if (mean(is.na(x)) < 1) mean(x, na.rm = TRUE) else NA
    )

    tmp$salario <- round(tmp$salario, 0)
    tmp$cotiza <- round(100 * tmp$cotiza, 1)
    names(tmp) <- c("Trimestre", "Franja", "N", "Sal. Prom", "% Cotiza")

    datatable(tmp, options = list(pageLength = 10))
  })

  output$tbl2 <- renderDataTable({
    datatable(
      datos()[, c("trimestredesc", "sexo", "ingoc1sml_cat", "edad", "educacion", "salario_prom", "cotiza_bin", "area_urb")],
      options = list(pageLength = 10)
    )
  })

  output$dl <- downloadHandler(
    filename = function() paste0("sml_", Sys.Date(), ".csv"),
    content = function(file) write.csv(datos(), file, row.names = FALSE)
  )

  # -------------------------------------------------------------------
  # Vista econometrica de espiral
  # -------------------------------------------------------------------

  output$espiral_status <- renderUI({
    if (spiral_ok) {
      div(
        class = "alert alert-success",
        HTML(paste0(
          "<b>Base de espiral cargada:</b> ", spiral_obj$ruta,
          "<br><span class='small-note'>Eventos detectados: ", nrow(spiral_obj$eventos),
          " | Rango: ", min(spiral_obj$datos$fecha, na.rm = TRUE), " a ", max(spiral_obj$datos$fecha, na.rm = TRUE),
          "</span>"
        ))
      )
    } else {
      div(
        class = "alert alert-danger",
        HTML(paste0(
          "<b>No se pudo cargar la base de espiral.</b><br>",
          spiral_obj$message,
          "<br><span class='small-note'>Ruta esperada: ", spiral_obj$path, "</span>"
        ))
      )
    }
  })

  observeEvent(
    list(input$esp_mode, input$esp_story_step),
    {
      if (!isTRUE(input$esp_mode == "presentacion")) return(NULL)

      st <- input$esp_story_step %||% 1

      if (st == 1) {
        updateSliderInput(session, "esp_h", value = 6)
        updateSliderInput(session, "esp_min_adj", value = 0)
        updateRadioButtons(session, "esp_overlap", selected = "all")
        updateCheckboxInput(session, "esp_use_food", value = TRUE)
      } else if (st == 2) {
        updateSliderInput(session, "esp_h", value = 6)
        updateSliderInput(session, "esp_min_adj", value = 5)
        updateRadioButtons(session, "esp_overlap", selected = "clean")
      } else if (st == 3) {
        updateSliderInput(session, "esp_h", value = 3)
        updateCheckboxInput(session, "esp_use_food", value = TRUE)
        updateCheckboxGroupInput(
          session, "esp_ctx_vars",
          selected = c("pct_1sml", "pct_cotiza", "pct_hogares_internet")
        )
      } else if (st == 4) {
        updateSliderInput(session, "esp_placebo_reps", value = 2500)
      } else if (st == 5) {
        updateSliderInput(session, "sim_adj", value = 10)
        updateSliderInput(session, "sim_beta_gen", value = 0.07)
        updateSliderInput(session, "sim_beta_alim", value = 0.13)
        updateSliderInput(session, "sim_phi", value = 0.55)
        updateSliderInput(session, "sim_months", value = 12)
      }
    },
    ignoreNULL = FALSE
  )

  esp_model <- reactive({
    input$esp_run
    req(spiral_ok)

    h <- input$esp_h
    ev <- spiral_obj$eventos
    ev <- ev[ev$ajuste_pp >= input$esp_min_adj, ]

    if (nrow(ev) == 0) {
      return(list(empty = TRUE, msg = "No hay eventos con el filtro actual de ajuste minimo."))
    }

    if (input$esp_overlap == "clean") {
      overlap <- flag_overlap_events(ev$fecha, h)
      ev <- ev[!overlap, ]
    }

    if (nrow(ev) == 0) {
      return(list(empty = TRUE, msg = "Todos los eventos fueron excluidos por solapamiento con el horizonte actual."))
    }

    met <- calc_event_metrics(spiral_obj$datos, ev, h)
    avg_evt <- calc_event_average(spiral_obj$datos, ev, h)
    placebo <- calc_placebo(spiral_obj$datos, ev, h, reps = input$esp_placebo_reps, seed = 123)

    list(
      empty = FALSE,
      h = h,
      eventos = ev,
      metrics = met,
      avg_evt = avg_evt,
      placebo = placebo
    )
  })

  output$esp_vbox1 <- renderValueBox({
    if (!spiral_ok) return(valueBox("-", "Eventos usados", icon = icon("bolt"), color = "light-blue"))
    em <- esp_model()
    if (isTRUE(em$empty)) return(valueBox(0, "Eventos usados", icon = icon("bolt"), color = "light-blue"))
    valueBox(nrow(em$eventos), "Eventos usados", icon = icon("bolt"), color = "light-blue")
  })

  output$esp_vbox2 <- renderValueBox({
    if (!spiral_ok) return(valueBox("-", "EI IPC general", icon = icon("chart-line"), color = "teal"))
    em <- esp_model()
    if (isTRUE(em$empty)) return(valueBox("NA", "EI IPC general", icon = icon("chart-line"), color = "teal"))
    ei <- safe_mean(em$metrics$ei_gen)
    valueBox(paste0(round(ei, 3), " pp"), "EI IPC general", icon = icon("chart-line"), color = "teal")
  })

  output$esp_vbox3 <- renderValueBox({
    if (!spiral_ok) return(valueBox("-", "p-value placebo (gen)", icon = icon("dice"), color = "purple"))
    em <- esp_model()
    if (isTRUE(em$empty)) return(valueBox("NA", "p-value placebo (gen)", icon = icon("dice"), color = "purple"))
    pv <- em$placebo$p_gen
    valueBox(ifelse(is.na(pv), "NA", round(pv, 3)), "p-value placebo (gen)", icon = icon("dice"), color = "purple")
  })

  output$esp_vbox4 <- renderValueBox({
    if (!spiral_ok) return(valueBox("-", "EI IPC alimentos", icon = icon("apple-alt"), color = "olive"))
    em <- esp_model()
    if (isTRUE(em$empty)) return(valueBox("NA", "EI IPC alimentos", icon = icon("apple-alt"), color = "olive"))
    ei <- safe_mean(em$metrics$ei_alim)
    valueBox(paste0(round(ei, 3), " pp"), "EI IPC alimentos", icon = icon("apple-alt"), color = "olive")
  })

  output$esp_plot_event <- renderPlotly({
    req(spiral_ok)
    em <- esp_model()

    if (isTRUE(em$empty) || nrow(em$avg_evt) == 0) {
      return(plot_ly() %>% layout(title = em$msg %||% "Sin datos"))
    }

    p <- plot_ly(em$avg_evt, x = ~rel_month, y = ~ipc_general_m, type = "scatter", mode = "lines+markers", name = "IPC general", line = list(color = "#2c3e50", width = 3))

    if (isTRUE(input$esp_use_food)) {
      p <- add_trace(p, y = ~ipc_alim_m, mode = "lines+markers", name = "IPC alimentos", line = list(color = "#d35400", width = 3))
    }

    p %>%
      add_segments(
        x = 0, xend = 0,
        y = min(c(em$avg_evt$ipc_general_m, em$avg_evt$ipc_alim_m), na.rm = TRUE),
        yend = max(c(em$avg_evt$ipc_general_m, em$avg_evt$ipc_alim_m), na.rm = TRUE),
        line = list(color = "#7f8c8d", dash = "dash"),
        showlegend = FALSE,
        inherit = FALSE
      ) %>%
      layout(
        title = paste0("Inflacion mensual promedio alrededor del ajuste (h=", em$h, ")"),
        xaxis = list(title = "Mes relativo al ajuste"),
        yaxis = list(title = "Variacion mensual (%)"),
        hovermode = "x unified"
      )
  })

  output$esp_plot_placebo <- renderPlotly({
    req(spiral_ok)
    em <- esp_model()
    if (isTRUE(em$empty)) {
      return(plot_ly() %>% layout(title = em$msg %||% "Sin datos"))
    }

    pg <- em$placebo$placebo_gen
    pa <- em$placebo$placebo_alim
    pg <- pg[is.finite(pg)]
    pa <- pa[is.finite(pa)]

    if (length(pg) == 0) {
      return(plot_ly() %>% layout(title = "Placebo no disponible con estos parametros"))
    }

    p <- plot_ly()
    p <- add_histogram(
      p,
      x = pg,
      name = "Placebo EI general",
      marker = list(color = "#5dade2"),
      opacity = 0.65,
      nbinsx = 40
    )

    if (isTRUE(input$esp_use_food)) {
      p <- add_histogram(
        p,
        x = pa,
        name = "Placebo EI alimentos",
        marker = list(color = "#f5b041"),
        opacity = 0.55,
        nbinsx = 40
      )
    }

    y_top <- max(hist(pg, plot = FALSE)$counts) * 1.1

    p <- p %>%
      add_segments(
        x = em$placebo$obs_gen,
        xend = em$placebo$obs_gen,
        y = 0,
        yend = y_top,
        line = list(color = "#1b4f72", width = 3),
        name = "EI observado general",
        showlegend = TRUE,
        inherit = FALSE
      )

    if (isTRUE(input$esp_use_food)) {
      p <- p %>%
        add_segments(
          x = em$placebo$obs_alim,
          xend = em$placebo$obs_alim,
          y = 0,
          yend = y_top,
          line = list(color = "#7d6608", width = 3, dash = "dash"),
          name = "EI observado alimentos",
          showlegend = TRUE,
          inherit = FALSE
        )
    }

    layout(
      p,
      barmode = "overlay",
      title = paste0("Distribucion placebo (", input$esp_placebo_reps, " reps)"),
      xaxis = list(title = "EI promedio"),
      yaxis = list(title = "Frecuencia")
    )
  })

  output$esp_plot_sim <- renderPlotly({
    req(spiral_ok)
    em <- esp_model()
    if (isTRUE(em$empty)) {
      return(plot_ly() %>% layout(title = em$msg %||% "Sin datos"))
    }

    base_gen <- safe_mean(em$metrics$infl_gen_pre)
    base_alim <- safe_mean(em$metrics$infl_alim_pre)

    sim <- simulate_shock_path(
      base_gen = base_gen,
      base_alim = base_alim,
      adj_pp = input$sim_adj,
      beta_gen = input$sim_beta_gen,
      beta_alim = input$sim_beta_alim,
      phi = input$sim_phi,
      months = input$sim_months
    )

    p <- plot_ly(sim, x = ~mes, y = ~ipc_general_sim, type = "scatter", mode = "lines+markers", name = "Sim IPC general", line = list(color = "#154360", width = 3))

    if (isTRUE(input$esp_use_food)) {
      p <- add_trace(p, y = ~ipc_alim_sim, mode = "lines+markers", name = "Sim IPC alimentos", line = list(color = "#af601a", width = 3))
    }

    p %>% layout(
      title = "Trayectoria simulada tras shock salarial",
      xaxis = list(title = "Mes post-ajuste"),
      yaxis = list(title = "Inflacion mensual simulada (%)"),
      hovermode = "x unified"
    )
  })

  output$esp_plot_micro <- renderPlotly({
    req(spiral_ok)

    ctx <- context_series
    if (nrow(ctx) == 0) {
      return(plot_ly() %>% layout(title = "No hay series de contexto disponibles"))
    }

    selected <- input$esp_ctx_vars
    if (is.null(selected) || length(selected) == 0) selected <- c("pct_1sml")
    selected <- selected[selected %in% names(ctx)]

    if (length(selected) == 0) {
      return(plot_ly() %>% layout(title = "Selecciona al menos un indicador de contexto"))
    }

    label_map <- c(
      pct_1sml = "% en 1 SML (R02)",
      pct_cotiza = "% formalidad laboral (R02)",
      pct_hogares_internet = "% hogares con internet (R01)",
      pct_hogares_material_apto = "% hogares con material adecuado (R01)",
      pct_hogares_movilidad = "% hogares con movilidad (R01)"
    )

    color_ctx <- c(
      pct_1sml = "#117a65",
      pct_cotiza = "#7b241c",
      pct_hogares_internet = "#1f618d",
      pct_hogares_material_apto = "#884ea0",
      pct_hogares_movilidad = "#ca6f1e"
    )

    levels_trim <- ctx %>%
      arrange(year, quarter) %>%
      pull(trimestredesc)
    levels_trim <- unique(levels_trim)

    ctx_long <- bind_rows(lapply(selected, function(vv) {
      tibble(
        trimestredesc = ctx$trimestredesc,
        indicador = vv,
        valor = suppressWarnings(as.numeric(ctx[[vv]]))
      )
    })) %>%
      filter(!is.na(valor))

    if (nrow(ctx_long) == 0) {
      return(plot_ly() %>% layout(title = "No hay valores no nulos para los indicadores seleccionados"))
    }

    ctx_long$trimestredesc <- factor(ctx_long$trimestredesc, levels = levels_trim, ordered = TRUE)

    p <- plot_ly()
    for (vv in unique(ctx_long$indicador)) {
      dd <- ctx_long[ctx_long$indicador == vv, ]
      p <- add_trace(
        p,
        data = dd,
        x = ~trimestredesc,
        y = ~valor,
        type = "scatter",
        mode = "lines+markers",
        name = label_map[[vv]] %||% vv,
        line = list(color = color_ctx[[vv]] %||% "#2c3e50", width = 3)
      )
    }

    evt_marks <- spiral_obj$eventos %>%
      mutate(trimestredesc = paste0(year(fecha), "Trim", quarter(fecha))) %>%
      distinct(trimestredesc)

    y_max <- max(ctx_long$valor, na.rm = TRUE)
    evt_df <- data.frame(
      trimestredesc = evt_marks$trimestredesc,
      valor = y_max * 1.02
    )
    evt_df <- evt_df[evt_df$trimestredesc %in% levels_trim, , drop = FALSE]

    if (nrow(evt_df) > 0) {
      evt_df$trimestredesc <- factor(evt_df$trimestredesc, levels = levels_trim, ordered = TRUE)
      p <- add_markers(
        p,
        data = evt_df,
        x = ~trimestredesc,
        y = ~valor,
        name = "Trimestres con ajuste SML",
        marker = list(symbol = "diamond", size = 9, color = "#1f618d"),
        inherit = FALSE
      )
    }

    layout(
      p,
      title = "Contexto laboral y de hogares con marcas de ajuste",
      xaxis = list(title = "Trimestre", tickangle = -45),
      yaxis = list(title = "Porcentaje (%)"),
      hovermode = "x unified"
    )
  })

  output$esp_story_panel <- renderUI({
    req(spiral_ok)
    em <- esp_model()

    if (!isTRUE(input$esp_mode == "presentacion")) {
      return(HTML(
        "<p class='small-note'><b>Modo analitico:</b> explora libremente los parametros y usa la tabla de eventos para auditar resultados.</p>"
      ))
    }

    st <- input$esp_story_step %||% 1
    if (isTRUE(em$empty)) {
      return(HTML(paste0("<p><b>Sin resultados para este paso:</b> ", em$msg, "</p>")))
    }

    ei_g <- round(safe_mean(em$metrics$ei_gen), 3)
    ei_a <- round(safe_mean(em$metrics$ei_alim), 3)
    pv_g <- round(em$placebo$p_gen, 3)
    pv_a <- round(em$placebo$p_alim, 3)

    txt <- switch(
      as.character(st),
      "1" = paste0(
        "<h4>Paso 1: El fenomeno en una vista</h4>",
        "<p>Partimos del promedio de todos los eventos para ver si la inflacion sube despues del ajuste salarial. ",
        "Observa el grafico <b>Evento Promedio</b> y la distancia entre meses negativos (antes) y positivos (despues).</p>",
        "<p>Con esta configuracion: EI general = <b>", ei_g, " pp</b>, EI alimentos = <b>", ei_a, " pp</b>.</p>"
      ),
      "2" = paste0(
        "<h4>Paso 2: Eventos mas limpios</h4>",
        "<p>Subimos el filtro de ajuste y excluimos eventos solapados para evitar contaminacion entre shocks cercanos.</p>",
        "<p>Esto ayuda a identificar si el patron persiste en episodios de mayor intensidad.</p>"
      ),
      "3" = paste0(
        "<h4>Paso 3: Contexto social y laboral</h4>",
        "<p>Ahora mira el panel de contexto: series R02 y R01 con marcas de trimestres de ajuste.</p>",
        "<p>Sirve para explicar heterogeneidad del traspaso: composicion salarial, formalidad e indicadores de hogares.</p>"
      ),
      "4" = paste0(
        "<h4>Paso 4: Evidencia estadistica (placebo)</h4>",
        "<p>Contrastamos el EI observado contra escenarios no-evento simulados.</p>",
        "<p>Resultados actuales: p-value general = <b>", pv_g, "</b>, p-value alimentos = <b>", pv_a, "</b>.</p>"
      ),
      "5" = paste0(
        "<h4>Paso 5: Simulador de politicas</h4>",
        "<p>Usa el simulador para discutir escenarios hipoteticos: tamano del ajuste, traspaso y persistencia.</p>",
        "<p>Ideal para presentaciones: muestra sensibilidad de resultados sin afirmar causalidad fuerte.</p>"
      ),
      "<p>Selecciona un paso de la historia.</p>"
    )

    HTML(txt)
  })

  output$esp_text <- renderUI({
    req(spiral_ok)
    em <- esp_model()

    if (isTRUE(em$empty)) {
      return(HTML(paste0("<p><b>Sin resultados:</b> ", em$msg, "</p>")))
    }

    ei_gen <- safe_mean(em$metrics$ei_gen)
    ei_alim <- safe_mean(em$metrics$ei_alim)
    ct_gen <- safe_mean(em$metrics$ct_gen)
    ct_alim <- safe_mean(em$metrics$ct_alim)
    pv_gen <- em$placebo$p_gen
    pv_alim <- em$placebo$p_alim

    interp_gen <- ifelse(!is.na(pv_gen) && pv_gen < 0.10, "senial estadistica relevante", "sin evidencia estadistica fuerte")
    interp_alim <- ifelse(!is.na(pv_alim) && pv_alim < 0.10, "senial estadistica relevante", "sin evidencia estadistica fuerte")
    modo_tip <- if (isTRUE(input$esp_mode == "presentacion")) {
      paste0("Estas en <b>modo presentacion</b> (paso ", input$esp_story_step %||% 1, ").")
    } else {
      "Estas en <b>modo analitico</b>."
    }

    HTML(paste0(
      "<p><b>Que estas viendo:</b> comparamos la inflacion media antes y despues de cada ajuste del salario minimo (horizonte = ", em$h, " meses).</p>",
      "<p><b>Resultado IPC general:</b> EI promedio = <b>", round(ei_gen, 3), " pp</b>, CT promedio = <b>", round(ct_gen, 3), "</b>, placebo p-value = <b>", round(pv_gen, 3), "</b> (", interp_gen, ").</p>",
      "<p><b>Resultado IPC alimentos:</b> EI promedio = <b>", round(ei_alim, 3), " pp</b>, CT promedio = <b>", round(ct_alim, 3), "</b>, placebo p-value = <b>", round(pv_alim, 3), "</b> (", interp_alim, ").</p>",
      "<p class='small-note'>", modo_tip, " Tip: proba subir el filtro de ajuste minimo y activar 'Excluir solapados' para ver si las conclusiones cambian en eventos mas limpios.</p>"
    ))
  })

  output$esp_tbl_events <- renderDataTable({
    req(spiral_ok)
    em <- esp_model()
    if (isTRUE(em$empty)) {
      return(datatable(data.frame(Mensaje = em$msg), options = list(dom = "t"), rownames = FALSE))
    }

    tb <- em$metrics %>%
      mutate(
        fecha_evento = as.character(fecha_evento),
        across(c(ajuste_pp, ei_gen, ei_alim, ct_gen, ct_alim, infl_gen_pre, infl_gen_post, infl_alim_pre, infl_alim_post), ~ round(., 3))
      ) %>%
      arrange(fecha_evento)

    datatable(tb, options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })
}

# =====================================================================
# EJECUTAR
# =====================================================================

cat("[OK] App cargada\n")
shinyApp(ui = ui, server = server)
