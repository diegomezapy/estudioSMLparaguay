make_demo_db <- function(n = 8000, years = 2022:2025, tol = 0.10) {
  set.seed(123)

  base <- tibble::tibble(
    anio = sample(years, size = n, replace = TRUE),
    q = sample(1:4, size = n, replace = TRUE),
    upm = sample(1:400, size = n, replace = TRUE),
    estrato = sample(1:80, size = n, replace = TRUE),
    w = runif(n, 0.5, 2.5),
    p06 = sample(c(1, 6), size = n, replace = TRUE, prob = c(0.55, 0.45)),
    b10 = sample(c(1, 6), size = n, replace = TRUE, prob = c(0.35, 0.65)),
    p02 = pmax(14, pmin(80, round(rnorm(n, mean = 34, sd = 12)))),
    anoest = pmax(0, pmin(20, round(rnorm(n, mean = 10, sd = 4)))),
    area = sample(c(1, 6), size = n, replace = TRUE, prob = c(0.65, 0.35)),
    tama_pea = sample(c(1:6, 98, 99), size = n, replace = TRUE, prob = c(rep(0.14, 6), 0.01, 0.01)),
    rama_pea = sample(1:12, size = n, replace = TRUE),
    horabc = pmax(5, pmin(80, round(rnorm(n, mean = 44, sd = 10))))
  ) %>%
    dplyr::mutate(
      trim_seq = (anio - min(years)) * 4L + q,
      trimestredesc = paste0(anio, "Trim", q)
    )

  sml_data <- get_sml_data(include_projection = TRUE)
  sml_monthly <- build_sml_monthly(sml_data, end_date = as.Date(paste0(max(years), "-12-01")))
  base <- add_sml_to_db(base, sml_monthly)

  # Generar bunching alrededor de 1 SML + cola derecha
  mix <- runif(n)
  ratio <- ifelse(
    mix < 0.35,
    pmax(0.2, rnorm(n, mean = 1.0, sd = 0.02)),
    pmax(0.2, rlnorm(n, meanlog = log(1.15), sdlog = 0.35))
  )

  base$e01aimde <- ratio * base$sml

  derive_reg02_vars(base, tol = tol)
}
