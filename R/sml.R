get_sml_data <- function(include_projection = TRUE) {
  sml <- tibble::tribble(
    ~year, ~mes, ~sml,
    1980, 1, 20520, 1980, 7, 23610,
    1981, 5, 27180, 1983, 7, 29910,
    1984, 6, 34410, 1984, 9, 39570,
    1985, 2, 43530, 1985,10, 52230,
    1986, 1, 60060, 1986, 7, 72072,
    1987, 1, 86486, 1987,10,103783,
    1988, 3,119350, 1988,10,143220,
    1989, 6,164640, 1990, 7,213000,
    1990,10,244950, 1992, 7,269445,
    1993, 4,300000, 1994, 7,379500,
    1995, 5,436425, 1996, 4,480068,
    1997, 1,528076, 1998, 3,591445,
    2000, 1,680162, 2001, 5,782186,
    2002, 8,876048, 2003, 2,972413,
    2005, 4,1089103,2006, 4,1219795,
    2007,10,1341775,2009, 5,1408864,
    2010, 7,1507484,2011, 4,1658232,
    2014, 3,1824055,2016,12,1964507,
    2017, 7,2041123,2018, 7,2112562,
    2019, 7,2192839,2021, 7,2289324,
    2022, 7,2550307,2023, 7,2680373,
    2024, 7,2798309,
    2025, 7,2899048,
    2026, 7,2954130
  ) %>%
    dplyr::mutate(
      fecha = lubridate::make_date(year, mes, 1),
      tipo = dplyr::case_when(
        year == 2024 & mes == 7 ~ "Vigente desde 01/07/2024",
        year == 2025 & mes == 7 ~ "Vigente desde 01/07/2025",
        year == 2026 & mes == 7 ~ "Proyección preliminar (no oficial)",
        TRUE ~ NA_character_
      )
    )

  if (!isTRUE(include_projection)) {
    sml <- dplyr::filter(sml, !(year == 2026 & mes == 7))
  }
  sml
}

build_sml_monthly <- function(sml_data, end_date = NULL) {
  if (is.null(end_date)) {
    end_date <- as.Date(paste0(max(sml_data$year, na.rm = TRUE), "-12-01"))
  }
  
  # Puntos de IPC conocidos (Base Diciembre 2021 = 100) según BCP
  ipc_ref <- tibble::tribble(
    ~fecha, ~ipc,
    as.Date("2021-12-01"), 100.0,
    as.Date("2022-12-01"), 108.1,
    as.Date("2023-12-01"), 112.10,
    as.Date("2024-12-01"), 116.36,
    as.Date("2025-12-01"), 119.96,
    as.Date("2026-12-01"), 123.56
  )

  serie_mensual <- tibble::tibble(fecha = seq.Date(as.Date("1980-01-01"), end_date, by = "month"))
  
  serie_mensual <- serie_mensual %>%
    dplyr::left_join(dplyr::select(sml_data, fecha, sml), by = "fecha") %>%
    tidyr::fill(sml, .direction = "down")
    
  # Interpolar IPC linealmente para todos los meses
  ipc_interp <- approx(x = as.numeric(ipc_ref$fecha), y = ipc_ref$ipc, 
                       xout = as.numeric(serie_mensual$fecha), rule = 2)$y
  serie_mensual$ipc <- ipc_interp
  
  # Calcular SML Real (Deflactado, base 2021)
  serie_mensual$sml_real <- serie_mensual$sml / (serie_mensual$ipc / 100)
  
  serie_mensual
}

add_sml_to_db <- function(db, sml_monthly) {
  # Fecha intermedia por trimestre: feb/may/ago/nov
  db <- db %>%
    dplyr::mutate(
      mes_intermedio = c(2, 5, 8, 11)[q],
      fecha = lubridate::make_date(anio, mes_intermedio, 1)
    )
  dplyr::left_join(db, sml_monthly, by = "fecha")
}
