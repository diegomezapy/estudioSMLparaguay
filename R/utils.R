fmt_gs <- function(x, digits = 0) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(
    is.na(x),
    NA_character_,
    format(round(x, digits), big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE)
  )
}

w_mean <- function(x, w) {
  x <- suppressWarnings(as.numeric(x))
  w <- suppressWarnings(as.numeric(w))
  ok <- is.finite(x) & is.finite(w)
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

w_share <- function(cond, w) {
  w <- suppressWarnings(as.numeric(w))
  cond <- as.integer(cond %in% TRUE)
  ok <- is.finite(w) & !is.na(cond)
  if (!any(ok)) return(NA_real_)
  sum(w[ok] * cond[ok]) / sum(w[ok])
}

safe_numeric <- function(x) {
  # Acepta "123,45" o "123.45" o "1.234.567"
  if (is.numeric(x)) return(x)
  x <- as.character(x)
  x <- gsub("\\.", "", x)         # miles
  x <- gsub(",", ".", x)          # decimal
  suppressWarnings(as.numeric(x))
}

parse_reg02_name <- function(filename) {
  b <- basename(filename)
  year <- suppressWarnings(as.integer(stringr::str_extract(b, "(20\\d{2})")))
  q <- dplyr::case_when(
    stringr::str_detect(b, "1er[_\\s-]*Trim") ~ 1L,
    stringr::str_detect(b, "2do[_\\s-]*Trim") ~ 2L,
    stringr::str_detect(b, "3er[_\\s-]*Trim") ~ 3L,
    stringr::str_detect(b, "4to[_\\s-]*Trim") ~ 4L,
    TRUE ~ NA_integer_
  )
  if (is.na(q)) {
    q2 <- suppressWarnings(as.integer(stringr::str_match(b, "Trim\\s*([1-4])")[, 2]))
    if (is.finite(q2)) q <- q2
  }
  tibble::tibble(file = filename, year = year, q = q)
}
