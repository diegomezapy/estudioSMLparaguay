build_survey_design <- function(db) {
  if (!requireNamespace("survey", quietly = TRUE)) stop("Falta paquete 'survey'.")

  ids_formula <- if ("upm" %in% names(db) && any(is.finite(db$upm))) ~upm else ~1
  weights_formula <- if ("w" %in% names(db)) ~w else ~1

  if ("estrato" %in% names(db) && any(is.finite(db$estrato))) {
    survey::svydesign(ids = ids_formula, strata = ~estrato, weights = weights_formula, data = db, nest = TRUE)
  } else {
    survey::svydesign(ids = ids_formula, weights = weights_formula, data = db, nest = TRUE)
  }
}

tab_dist_sml <- function(db) {
  db %>%
    dplyr::filter(!is.na(ingoc1sml_cat), is.finite(w)) %>%
    dplyr::group_by(trimestredesc, ingoc1sml_cat) %>%
    dplyr::summarise(n_w = sum(w, na.rm = TRUE), .groups = "drop_last") %>%
    dplyr::mutate(pct = 100 * n_w / sum(n_w, na.rm = TRUE)) %>%
    dplyr::ungroup()
}

tab_mean_by_sml <- function(db, var, by = c("trimestredesc", "ingoc1sml_cat")) {
  stopifnot(var %in% names(db))
  db %>%
    dplyr::filter(!is.na(.data$ingoc1sml_cat), is.finite(.data$w)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) %>%
    dplyr::summarise(
      mean = w_mean(.data[[var]], w),
      n_w = sum(w, na.rm = TRUE),
      .groups = "drop"
    )
}

has_variation <- function(x) {
  x <- x[!is.na(x)]
  length(unique(x)) >= 2
}

run_svyglm_models <- function(db) {
  dbm <- db %>%
    dplyr::filter(
      !is.na(cotiza_bin),
      !is.na(ingoc1sml_cat),
      is.finite(w)
    )

  if (nrow(dbm) < 200) stop("Muestra insuficiente para estimar svyglm (filtrar menos).")
  if (!("1 SML" %in% unique(dbm$ingoc1sml_cat))) stop("La categoría '1 SML' no está presente en la muestra actual.")

  dbm <- dplyr::mutate(dbm, ingoc1sml_cat = stats::relevel(ingoc1sml_cat, ref = "1 SML"))

  controls <- c("edad", "I(edad^2)", "anoest_val")
  if ("sexo" %in% names(dbm) && has_variation(dbm$sexo)) controls <- c(controls, "sexo")
  if ("tam" %in% names(dbm) && has_variation(dbm$tam)) controls <- c(controls, "tam")
  if ("area_urb" %in% names(dbm) && has_variation(dbm$area_urb)) controls <- c(controls, "area_urb")
  if ("rama_pea" %in% names(dbm) && has_variation(dbm$rama_pea)) controls <- c(controls, "factor(rama_pea)")
  if ("trimestredesc" %in% names(dbm) && has_variation(dbm$trimestredesc)) controls <- c(controls, "factor(trimestredesc)")

  fml <- stats::as.formula(paste0("cotiza_bin ~ ingoc1sml_cat + ", paste(controls, collapse = " + ")))

  des <- build_survey_design(dbm)

  m_qb <- survey::svyglm(fml, design = des, family = stats::quasibinomial())
  m_lpm <- survey::svyglm(fml, design = des, family = stats::gaussian())

  list(qb = m_qb, lpm = m_lpm, formula = fml)
}

run_event_study <- function(db, base_year = 2022L, base_q = 3L, window = 4L) {
  if (!requireNamespace("fixest", quietly = TRUE)) stop("Falta paquete 'fixest'.")

  db_es <- db %>%
    dplyr::mutate(
      t_rel = (anio - base_year) * 4L + (q - base_q),
      y_formal = cotiza_bin
    ) %>%
    dplyr::filter(!is.na(t_rel), abs(t_rel) <= window, is.finite(w))

  if (nrow(db_es) < 200) stop("Muestra insuficiente para event study (filtrar menos).")

  fe_terr <- if ("estgeo" %in% names(db_es) && has_variation(db_es$estgeo)) "estgeo" else if ("dptorep" %in% names(db_es) && has_variation(db_es$dptorep)) "dptorep" else "area"

  rhs <- c("i(t_rel, ref=-1)", "edad", "I(edad^2)")
  if ("sexo" %in% names(db_es) && has_variation(db_es$sexo)) rhs <- c(rhs, "sexo")
  if ("anoest_val" %in% names(db_es) && has_variation(db_es$anoest_val)) rhs <- c(rhs, "anoest_val")
  if ("area_urb" %in% names(db_es) && has_variation(db_es$area_urb)) rhs <- c(rhs, "area_urb")

  fml <- stats::as.formula(paste0("y_formal ~ ", paste(rhs, collapse = " + "), " | ", fe_terr))

  est <- fixest::feols(
    fml,
    data = db_es,
    weights = ~w,
    cluster = if ("upm" %in% names(db_es)) ~upm else NULL
  )

  est
}

tab_bunching <- function(db, binwidth = 0.02, max_ratio = 3) {
  dens <- db %>%
    dplyr::filter(is.finite(ratio_sml), ratio_sml > 0, ratio_sml < max_ratio, is.finite(w)) %>%
    dplyr::mutate(bin = cut(ratio_sml, breaks = seq(0, max_ratio, by = binwidth), include.lowest = TRUE)) %>%
    dplyr::group_by(bin) %>%
    dplyr::summarise(w_mass = sum(w, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(mid = as.numeric(sub("^[\\[(]([^,]+),.*$", "\\1", as.character(bin))) + (binwidth / 2)) %>%
    dplyr::filter(!is.na(mid))

  spike <- db %>%
    dplyr::filter(is.finite(ratio_sml), ratio_sml > 0, ratio_sml < max_ratio, is.finite(w)) %>%
    dplyr::summarise(
      pct_spike = 100 * sum(w * (ratio_sml >= 0.98 & ratio_sml <= 1.02), na.rm = TRUE) / sum(w, na.rm = TRUE)
    )

  list(dens = dens, spike = spike)
}
