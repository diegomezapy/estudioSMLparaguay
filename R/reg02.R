read_reg02_files <- function(files, anios = NULL) {
  meta <- dplyr::bind_rows(lapply(files, parse_reg02_name)) %>%
    dplyr::filter(!is.na(year), !is.na(q))

  if (!is.null(anios)) meta <- dplyr::filter(meta, year %in% anios)
  if (nrow(meta) == 0) stop("No se pudieron parsear (año, trimestre) desde nombres de archivo REG02.")

  read_one <- function(fp, year, q) {
    df <- suppressMessages(readr::read_csv2(fp, show_col_types = FALSE, col_types = readr::cols(.default = readr::col_character())))
    df <- janitor::clean_names(df)

    numeric_cols <- c(
      "upm","nvivi","nhoga","p02","p06","b10","anoest","area",
      "tama_pea","e01aimde","rama_pea","fex_2022","factor","horab","horabc","estrato","d01","estgeo"
    )
    for (col in intersect(numeric_cols, names(df))) {
      df[[col]] <- safe_numeric(df[[col]])
    }

    df$anio <- as.integer(year)
    df$q <- as.integer(q)
    df
  }

  db <- dplyr::bind_rows(mapply(read_one, meta$file, meta$year, meta$q, SIMPLIFY = FALSE))
  db <- dplyr::mutate(
    db,
    trim_seq = (anio - min(anio, na.rm = TRUE)) * 4L + q,
    trimestredesc = paste0(anio, "Trim", q)
  )

  # Peso (combina fex_2022 y factor según disponibilidad por trimestre)
  w_vec <- rep(NA_real_, nrow(db))
  if ("fex_2022" %in% names(db)) w_vec <- dplyr::coalesce(w_vec, safe_numeric(db$fex_2022))
  if ("factor" %in% names(db)) w_vec <- dplyr::coalesce(w_vec, safe_numeric(db$factor))
  
  db$w <- w_vec

  if (all(is.na(db$w)) || mean(is.na(db$w)) > 0.1) {
    db$w <- 1
    warning("Pesos missing detectados. Asignando w=1 a todos.")
  } else if (any(is.na(db$w))) {
    db$w[is.na(db$w)] <- mean(db$w, na.rm = TRUE)
  }

  db
}

derive_reg02_vars <- function(db, tol = 0.10) {
  # Sexo: soporta codificaciones comunes (1/6 o 1/2)
  db <- dplyr::mutate(
    db,
    sexo = dplyr::case_when(
      p06 == 1 ~ "Hombres",
      p06 %in% c(2, 6) ~ "Mujeres",
      TRUE ~ NA_character_
    ),
    cotiza_bin = dplyr::case_when(
      b10 == 1 ~ 1L,
      b10 %in% c(2, 6) ~ 0L,
      TRUE ~ NA_integer_
    ),
    anoest_val = dplyr::if_else(anoest %in% c(98, 99), NA_real_, suppressWarnings(as.numeric(anoest))),
    area_urb = as.integer(area == 1),
    tam = dplyr::if_else(tama_pea %in% c(98, 99), NA_real_, suppressWarnings(as.numeric(tama_pea))),
    rama_pea = suppressWarnings(as.integer(rama_pea)),
    edad = suppressWarnings(as.numeric(p02)),
    salario = suppressWarnings(as.numeric(e01aimde))
  )

  db <- dplyr::mutate(db, ratio_sml = salario / sml)
  db <- add_sml_bands(db, tol = tol)

  # Salario por hora (si existe horabc)
  if ("horabc" %in% names(db)) {
    db <- dplyr::mutate(
      db,
      horabc_val = suppressWarnings(as.numeric(horabc)),
      horas_mes = horabc_val * (52 / 12),
      sal_hora = dplyr::if_else(is.finite(salario) & is.finite(horas_mes) & horas_mes > 0, salario / horas_mes, NA_real_)
    )
  } else {
    db$sal_hora <- NA_real_
  }

  db
}

add_sml_bands <- function(db, tol = 0.10) {
  hi <- 1 + abs(tol)
  lo <- 1 - abs(tol)

  dplyr::mutate(
    db,
    ingoc1sml = dplyr::case_when(
      ratio_sml < lo ~ 0L,
      ratio_sml >= lo & ratio_sml <= hi ~ 1L,
      ratio_sml > hi ~ 2L,
      TRUE ~ NA_integer_
    ),
    ingoc1sml_cat = factor(
      ingoc1sml,
      levels = 0:2,
      labels = c("Menos de 1 SML", "1 SML", "Más de 1 SML")
    )
  )
}
