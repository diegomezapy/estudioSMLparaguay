library(haven)
library(dplyr)
library(janitor)

read_pesos_nuevos <- function(dir_pesos) {
  archivos <- list.files(dir_pesos, pattern = "\\.sav$", full.names = TRUE, ignore.case = TRUE)
  if (length(archivos) == 0) return(NULL)
  
  read_one_peso <- function(fp) {
    df <- suppressWarnings(haven::read_sav(fp))
    df <- janitor::clean_names(df)
    # Identificar columnas relevantes: fex_2022 o factor
    col_fex <- intersect(names(df), c("fex_2022", "factor", "fex"))
    if (length(col_fex) == 0) return(NULL)
    
    # Extraer año y trimestre del nombre del archivo
    bn <- tolower(basename(fp))
    year <- as.numeric(stringr::str_extract(bn, "202[2-5]"))
    q <- as.numeric(stringr::str_extract(bn, "[1-4](?=t)"))
    
    res <- df %>%
      dplyr::select(upm, nvivi, dplyr::any_of(col_fex)) %>%
      dplyr::mutate(
        anio = year,
        q = q,
        w_nuevo = .data[[col_fex[1]]]
      ) %>%
      dplyr::select(upm, nvivi, anio, q, w_nuevo)
    
    # Limpiar UPM y NVIVI para cruzar
    res$upm <- as.numeric(as.character(res$upm))
    res$nvivi <- as.numeric(as.character(res$nvivi))
    res
  }
  
  pesos_df <- dplyr::bind_rows(lapply(archivos, read_one_peso))
  pesos_df
}

read_reg01_files <- function(dir_viviendas) {
  archivos <- list.files(dir_viviendas, pattern = "\\.SAV$", full.names = TRUE, ignore.case = TRUE)
  if (length(archivos) == 0) return(NULL)
  
  read_one_vivienda <- function(fp) {
    df <- suppressWarnings(haven::read_sav(fp))
    df <- janitor::clean_names(df)
    
    # Extraer año y trimestre
    bn <- tolower(basename(fp))
    # REG01_EPHC_T1_2022.SAV o REG01_EPHC2025_T1.SAV
    year <- as.numeric(stringr::str_extract(bn, "202[2-5]"))
    q <- as.numeric(stringr::str_extract(bn, "(?<=t)[1-4]"))
    if (is.na(q)) q <- as.numeric(stringr::str_extract(bn, "[1-4](?=t)"))
    
    numeric_cols <- c("upm", "nvivi", "dptorep", "v01", "v02a", "v02b", "v03", "v04", "v05", "v06", "v13", "v14a", "v14b")
    for (c in intersect(numeric_cols, names(df))) {
      df[[c]] <- as.numeric(as.character(df[[c]]))
    }
    
    df$anio <- year
    
    # IMPORTANTE: Los archivos REG01 de 2022 y 2023 fueron proveídos con la nomenclatura de
    # trimestres invertida (T1 corresponde a Q4, T2 a Q3, etc.) respecto a la base REG02.
    if (year %in% c(2022, 2023)) {
      if (q == 1) q <- 4
      else if (q == 2) q <- 3
      else if (q == 3) q <- 2
      else if (q == 4) q <- 1
    }
    
    df$q <- q
    
    df
  }
  
  viviendas_df <- dplyr::bind_rows(lapply(archivos, read_one_vivienda))
  
  # Variables de vivienda a recodificar
  if (!is.null(viviendas_df)) {
    viviendas_df <- viviendas_df %>%
      dplyr::mutate(
        # Departamento de residencia
        dpto_residencia = dptorep,
        
        # Calidad de la vivienda (simplificado)
        internet = dplyr::if_else((!is.na(v14a) & v14a == 1) | (!is.na(v14b) & v14b == 1), 1L, 0L),
        agua_potable = dplyr::if_else(!is.na(v04) & v04 %in% c(1, 2, 3), 1L, 0L),
        piso_bueno = dplyr::if_else(!is.na(v02a) & v02a < 5, 1L, 0L)
      ) %>%
      dplyr::distinct(upm, nvivi, anio, q, .keep_all = TRUE) %>%
      dplyr::select(upm, nvivi, anio, q, dpto_residencia, internet, agua_potable, piso_bueno)
  }
  
  viviendas_df
}
