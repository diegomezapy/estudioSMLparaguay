#!/usr/bin/env Rscript

# =====================================================================
#  Estudio SML Paraguay — Dashboard Shiny (EPHC REG02)
# =====================================================================

options(shiny.maxRequestSize = 500 * 1024^2) # 500 MB

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(plotly)
  library(DT)
  library(scales)
  library(survey)
  library(fixest)
  library(readr)
  library(janitor)
  library(stringr)
  library(lubridate)
})

APP_DIR <- getwd()
source(file.path(APP_DIR, "R", "utils.R"))
source(file.path(APP_DIR, "R", "sml.R"))
source(file.path(APP_DIR, "R", "reg02.R"))
source(file.path(APP_DIR, "R", "analysis.R"))
source(file.path(APP_DIR, "R", "demo.R"))

COL_SML <- c(
  "Menos de 1 SML" = "#B0BEC5",
  "1 SML" = "#2E7D32",
  "Más de 1 SML" = "#1565C0"
)

theme_sml <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  base_font = font_face("system-ui"),
  heading_font = font_face("system-ui"),
  code_font = font_face("ui-monospace")
)

plot_theme <- function() {
  theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

order_trimestres <- function(db) {
  db %>%
    distinct(anio, q, trimestredesc) %>%
    arrange(anio, q) %>%
    pull(trimestredesc)
}

ui <- page_sidebar(
  title = "SML Paraguay — Dashboard EPHC",
  theme = theme_sml,
  sidebar = sidebar(
    width = 360,
    card(
      card_header("Datos"),
      radioButtons(
        "data_mode", NULL,
        choices = c(
          "Preparado (data/db_full.rds)" = "rds",
          "Subir REG02 (CSV)" = "upload",
          "Demo (simulado)" = "demo"
        ),
        selected = "rds"
      ),
      fileInput("reg02_files", "Archivos REG02 (CSV2)", multiple = TRUE, accept = c(".csv")),
      actionButton("load_upload", "Cargar archivos", class = "btn-primary"),
      hr(),
      uiOutput("data_status")
    ),
    card(
      card_header("Filtros"),
      sliderInput("tol", "Franja 1 SML: tolerancia (±%)", min = 5, max = 20, value = 10, step = 1),
      uiOutput("year_ui"),
      selectInput("sexo", "Género", choices = c("Todos", "Hombres", "Mujeres"), selected = "Todos"),
      selectInput("area", "Área", choices = c("Todos", "Urbano", "Rural"), selected = "Todos")
    ),
    card(
      card_header("Notas"),
      tags$ul(
        tags$li("Franjas por defecto: <0,90 | 0,90–1,10 | >1,10 (configurable)."),
        tags$li("SML 2026 es una proyección preliminar; el ajuste oficial suele confirmarse en junio para regir desde julio."),
        tags$li("El repo ignora microdatos por defecto (ver .gitignore).")
      )
    )
  ),
  navset_card_tab(
    nav_panel(
      "Resumen",
      layout_columns(
        uiOutput("kpi_n"),
        uiOutput("kpi_sal"),
        uiOutput("kpi_formal"),
        uiOutput("kpi_share1"),
        col_widths = c(3, 3, 3, 3)
      ),
      layout_columns(
        card(card_header("Distribución por franja (ponderado)"), plotlyOutput("p_dist", height = "320px")),
        card(card_header("Formalidad por franja (ponderado)"), plotlyOutput("p_formal", height = "320px")),
        col_widths = c(6, 6)
      ),
      layout_columns(
        card(card_header("Salario promedio por franja y género (ponderado)"), plotlyOutput("p_sal", height = "360px")),
        col_widths = c(12)
      )
    ),
    nav_panel(
      "Descriptivos",
      layout_sidebar(
        sidebar = sidebar(
          width = 320,
          selectInput(
            "desc_var", "Indicador",
            choices = c(
              "Edad" = "edad",
              "Años de estudio" = "anoest_val",
              "Formalidad (cotiza)" = "cotiza_bin",
              "Salario mensual" = "salario",
              "Salario por hora" = "sal_hora",
              "Tamaño empresa" = "tam"
            ),
            selected = "edad"
          ),
          checkboxInput("desc_by_sex", "Desagregar por género", value = TRUE)
        ),
        card(
          card_header("Serie temporal (ponderado)"),
          plotlyOutput("p_desc", height = "420px"),
          hr(),
          DTOutput("t_desc")
        )
      )
    ),
    nav_panel(
      "Econometría",
      layout_sidebar(
        sidebar = sidebar(
          width = 340,
          p("Modelos con diseño de encuesta (svyglm). Resultado: probabilidad de cotizar."),
          actionButton("run_models", "Estimar modelos", class = "btn-primary"),
          hr(),
          tags$small("Especificación: franja SML + edad, edad², educación, sexo, tamaño empresa, área, rama y FE de trimestre.")
        ),
        card(
          card_header("Coeficientes principales (vs. 1 SML)"),
          DTOutput("t_models")
        )
      )
    ),
    nav_panel(
      "Event study",
      layout_sidebar(
        sidebar = sidebar(
          width = 340,
          sliderInput("es_window", "Ventana (± trimestres)", min = 2, max = 8, value = 4, step = 1),
          actionButton("run_es", "Estimar event study", class = "btn-primary"),
          hr(),
          tags$small("Definición: t_rel = (año - 2022)*4 + (q - 3). Referencia: t_rel = -1.")
        ),
        card(
          card_header("Efectos dinámicos sobre formalidad"),
          plotlyOutput("p_es", height = "420px"),
          hr(),
          DTOutput("t_es")
        )
      )
    ),
    nav_panel(
      "Bunching",
      layout_sidebar(
        sidebar = sidebar(
          width = 340,
          sliderInput("b_bin", "Ancho de bin (ratio)", min = 0.01, max = 0.05, value = 0.02, step = 0.01),
          sliderInput("b_max", "Ratio máximo", min = 1.5, max = 4, value = 3, step = 0.5)
        ),
        card(
          card_header("Masa ponderada alrededor del SML"),
          plotlyOutput("p_bunch", height = "420px"),
          hr(),
          uiOutput("b_spike")
        )
      )
    ),
    nav_panel(
      "SML",
      layout_columns(
        card(card_header("Valores recientes"), DTOutput("t_sml")),
        card(card_header("Serie histórica (mensual)"), plotlyOutput("p_sml", height = "360px")),
        col_widths = c(6, 6)
      )
    ),
    nav_panel(
      "Datos",
      layout_sidebar(
        sidebar = sidebar(
          width = 320,
          downloadButton("dl_csv", "Descargar datos filtrados (CSV)"),
          br(), br(),
          tags$small("La descarga contiene variables derivadas (y no incluye identificadores extra).")
        ),
        card(card_header("Vista previa"), DTOutput("t_data"))
      )
    )
  )
)

server <- function(input, output, session) {
  db_loaded <- reactiveVal(NULL)
  db_source <- reactiveVal("—")

  load_from_rds <- function() {
    path <- file.path(APP_DIR, "data", "db_full.rds")
    if (!file.exists(path)) return(FALSE)
    db_loaded(readRDS(path))
    db_source("data/db_full.rds")
    TRUE
  }

  # Inicialización: preferir RDS; si no existe, usar demo
  observeEvent(TRUE, {
    ok <- load_from_rds()
    if (!ok) {
      db_loaded(make_demo_db())
      db_source("demo (simulado)")
      updateRadioButtons(session, "data_mode", selected = "demo")
      showNotification("No se encontró data/db_full.rds. Cargando dataset demo.", type = "warning", duration = 6)
    }
  }, once = TRUE)

  observeEvent(input$data_mode, {
    if (input$data_mode == "rds") {
      ok <- load_from_rds()
      if (!ok) showNotification("Falta data/db_full.rds. Ejecutar data-prep.R o usar Upload/Demo.", type = "error")
    }
    if (input$data_mode == "demo") {
      db_loaded(make_demo_db())
      db_source("demo (simulado)")
    }
  })

  observeEvent(input$load_upload, {
    req(input$reg02_files)
    validate(need(nrow(input$reg02_files) > 0, "Subir al menos 1 archivo REG02."))

    withProgress(message = "Leyendo REG02…", value = 0.1, {
      tmp_dir <- file.path(tempdir(), "reg02_upload")
      dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
      dest <- file.path(tmp_dir, input$reg02_files$name)
      file.copy(input$reg02_files$datapath, dest, overwrite = TRUE)

      incProgress(0.3)
      db <- read_reg02_files(dest)
      incProgress(0.6)

      sml_data <- get_sml_data(include_projection = TRUE)
      end_date <- as.Date(paste0(max(db$anio, na.rm = TRUE), "-12-01"))
      sml_monthly <- build_sml_monthly(sml_data, end_date = end_date)
      db <- add_sml_to_db(db, sml_monthly)
      db <- derive_reg02_vars(db, tol = input$tol / 100)

      db_loaded(db)
      db_source("upload REG02 (CSV)")
      updateRadioButtons(session, "data_mode", selected = "upload")
      incProgress(1)
    })
  })

  output$data_status <- renderUI({
    db <- db_loaded()
    req(db)
    yrs <- sort(unique(db$anio))
    tags$div(
      tags$p(tags$b("Fuente:"), db_source()),
      tags$p(tags$b("Filas:"), format(nrow(db), big.mark = ".", scientific = FALSE)),
      tags$p(tags$b("Años:"), paste(yrs, collapse = ", "))
    )
  })

  output$year_ui <- renderUI({
    db <- db_loaded()
    req(db)
    yr_min <- min(db$anio, na.rm = TRUE)
    yr_max <- max(db$anio, na.rm = TRUE)
    sliderInput("year_range", "Años", min = yr_min, max = yr_max, value = c(yr_min, yr_max), step = 1, sep = "")
  })

  db <- reactive({
    d <- db_loaded()
    req(d)

    # Recalcular bandas si cambia tolerancia
    d <- add_sml_bands(d, tol = input$tol / 100)

    # Filtros base
    if (!is.null(input$year_range)) {
      d <- dplyr::filter(d, anio >= input$year_range[1], anio <= input$year_range[2])
    }
    if (!is.null(input$sexo) && input$sexo != "Todos") {
      d <- dplyr::filter(d, sexo == input$sexo)
    }
    if (!is.null(input$area) && input$area != "Todos") {
      d <- dplyr::filter(d, area_urb == as.integer(input$area == "Urbano"))
    }

    trim_levels <- order_trimestres(d)
    d <- dplyr::mutate(d, trimestredesc = factor(trimestredesc, levels = trim_levels))
    d
  })

  # KPIs
  output$kpi_n <- renderUI({
    d <- db()
    value_box(
      title = "Observaciones (ponderadas)",
      value = fmt_gs(sum(d$w, na.rm = TRUE), digits = 0),
      showcase = icon("users"),
      theme_color = "primary"
    )
  })

  output$kpi_sal <- renderUI({
    d <- db()
    value_box(
      title = "Salario promedio (Gs.)",
      value = fmt_gs(w_mean(d$salario, d$w), digits = 0),
      showcase = icon("money-bill-wave"),
      theme_color = "success"
    )
  })

  output$kpi_formal <- renderUI({
    d <- db()
    value_box(
      title = "Formalidad (cotiza)",
      value = paste0(round(100 * w_mean(d$cotiza_bin, d$w), 1), "%"),
      showcase = icon("briefcase"),
      theme_color = "warning"
    )
  })

  output$kpi_share1 <- renderUI({
    d <- db()
    value_box(
      title = "Franja 1 SML",
      value = paste0(round(100 * w_share(d$ingoc1sml_cat == "1 SML", d$w), 1), "%"),
      showcase = icon("chart-pie"),
      theme_color = "info"
    )
  })

  # Plot: distribución por franja
  output$p_dist <- renderPlotly({
    d <- db()
    dist <- tab_dist_sml(d) %>%
      mutate(trimestredesc = factor(trimestredesc, levels = levels(d$trimestredesc)))

    g <- ggplot(dist, aes(trimestredesc, pct, fill = ingoc1sml_cat)) +
      geom_col() +
      scale_fill_manual(values = COL_SML, name = "Franja") +
      scale_y_continuous(labels = label_percent(scale = 1)) +
      labs(x = "Trimestre", y = "%", title = "Distribución por franja SML") +
      plot_theme()

    ggplotly(g, tooltip = c("x", "y", "fill")) %>% layout(legend = list(orientation = "h"))
  })

  # Plot: formalidad por franja
  output$p_formal <- renderPlotly({
    d <- db()
    tmp <- d %>%
      filter(!is.na(ingoc1sml_cat), is.finite(w)) %>%
      group_by(trimestredesc, ingoc1sml_cat) %>%
      summarise(formal = 100 * w_mean(cotiza_bin, w), .groups = "drop") %>%
      mutate(trimestredesc = factor(trimestredesc, levels = levels(d$trimestredesc)))

    g <- ggplot(tmp, aes(trimestredesc, formal, color = ingoc1sml_cat, group = ingoc1sml_cat)) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 2) +
      scale_color_manual(values = COL_SML, name = "Franja") +
      scale_y_continuous(labels = label_percent(scale = 1)) +
      labs(x = "Trimestre", y = "%", title = "Formalidad (cotiza) por franja") +
      plot_theme()

    ggplotly(g, tooltip = c("x", "y", "color")) %>% layout(legend = list(orientation = "h"))
  })

  # Plot: salario por franja y género
  output$p_sal <- renderPlotly({
    d <- db()
    tmp <- d %>%
      filter(!is.na(ingoc1sml_cat), !is.na(sexo), is.finite(w)) %>%
      group_by(trimestredesc, ingoc1sml_cat, sexo) %>%
      summarise(sal = w_mean(salario, w), .groups = "drop") %>%
      mutate(trimestredesc = factor(trimestredesc, levels = levels(d$trimestredesc)))

    g <- ggplot(tmp, aes(trimestredesc, sal, color = ingoc1sml_cat, group = ingoc1sml_cat)) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 2) +
      facet_wrap(~sexo) +
      scale_color_manual(values = COL_SML, name = "Franja") +
      scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
      labs(x = "Trimestre", y = "Gs.", title = "Salario promedio por franja y género") +
      plot_theme()

    ggplotly(g, tooltip = c("x", "y", "color")) %>% layout(legend = list(orientation = "h"))
  })

  # Descriptivos (variable seleccionada)
  output$p_desc <- renderPlotly({
    d <- db()
    req(input$desc_var)

    by_cols <- c("trimestredesc", "ingoc1sml_cat")
    if (isTRUE(input$desc_by_sex)) by_cols <- c(by_cols, "sexo")

    tmp <- d %>%
      filter(!is.na(ingoc1sml_cat), is.finite(w)) %>%
      group_by(across(all_of(by_cols))) %>%
      summarise(val = w_mean(.data[[input$desc_var]], w), .groups = "drop") %>%
      mutate(trimestredesc = factor(trimestredesc, levels = levels(d$trimestredesc)))

    g <- ggplot(tmp, aes(trimestredesc, val, color = ingoc1sml_cat, group = ingoc1sml_cat)) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 2) +
      scale_color_manual(values = COL_SML, name = "Franja") +
      labs(x = "Trimestre", y = NULL, title = "Serie temporal (ponderado)") +
      plot_theme()

    if (isTRUE(input$desc_by_sex)) g <- g + facet_wrap(~sexo)

    if (identical(input$desc_var, "cotiza_bin")) {
      g <- g + scale_y_continuous(labels = label_percent(accuracy = 1))
    } else if (input$desc_var %in% c("salario", "sal_hora")) {
      g <- g + scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ","))
    }

    ggplotly(g, tooltip = c("x", "y", "color"))
  })

  output$t_desc <- renderDT({
    d <- db()
    req(input$desc_var)

    by_cols <- c("trimestredesc", "ingoc1sml_cat")
    if (isTRUE(input$desc_by_sex)) by_cols <- c(by_cols, "sexo")

    tmp <- d %>%
      filter(!is.na(ingoc1sml_cat), is.finite(w)) %>%
      group_by(across(all_of(by_cols))) %>%
      summarise(
        media = w_mean(.data[[input$desc_var]], w),
        n_w = sum(w, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        media = dplyr::case_when(
          input$desc_var %in% c("salario", "sal_hora") ~ fmt_gs(media, 0),
          identical(input$desc_var, "cotiza_bin") ~ paste0(round(100 * media, 1), "%"),
          TRUE ~ as.character(round(media, 3))
        ),
        n_w = fmt_gs(n_w, 0)
      )

    datatable(tmp, options = list(pageLength = 12, scrollX = TRUE))
  })

  # Econometría (svyglm)
  models <- eventReactive(input$run_models, {
    withProgress(message = "Estimando modelos (svyglm)…", value = 0.2, {
      m <- tryCatch(
        run_svyglm_models(db()),
        error = function(e) {
          showNotification(conditionMessage(e), type = "error", duration = 8)
          NULL
        }
      )
      incProgress(1)
      m
    })
  })

  output$t_models <- renderDT({
    m <- models()
    req(m)

    ext <- function(mod) {
      cf <- as.data.frame(summary(mod)$coefficients)
      cf$term <- rownames(cf)
      rownames(cf) <- NULL
      names(cf)[1:4] <- c("estimate", "std_error", "statistic", "p_value")
      cf %>%
        filter(grepl("^ingoc1sml_cat", term)) %>%
        mutate(term = sub("^ingoc1sml_cat", "", term))
    }

    t1 <- ext(m$qb) %>% transmute(franja = term, qb_est = estimate, qb_se = std_error, qb_p = p_value)
    t2 <- ext(m$lpm) %>% transmute(franja = term, lpm_est = estimate, lpm_se = std_error, lpm_p = p_value)

    tab <- full_join(t1, t2, by = "franja") %>%
      mutate(
        qb_est = round(qb_est, 4), qb_se = round(qb_se, 4), qb_p = round(qb_p, 4),
        lpm_est = round(lpm_est, 4), lpm_se = round(lpm_se, 4), lpm_p = round(lpm_p, 4)
      )

    datatable(tab, options = list(dom = "tip", pageLength = 10, scrollX = TRUE))
  })

  # Event study (fixest)
  es_model <- eventReactive(input$run_es, {
    withProgress(message = "Estimando event study (fixest)…", value = 0.2, {
      est <- tryCatch(
        run_event_study(db(), window = input$es_window),
        error = function(e) {
          showNotification(conditionMessage(e), type = "error", duration = 8)
          NULL
        }
      )
      incProgress(1)
      est
    })
  })

  output$p_es <- renderPlotly({
    est <- es_model()
    req(est)

    ct <- as.data.frame(fixest::coeftable(est))
    ct$term <- rownames(ct)
    rownames(ct) <- NULL
    names(ct)[1:4] <- c("estimate", "std_error", "statistic", "p_value")

    es <- ct %>%
      filter(grepl("^t_rel::", term)) %>%
      mutate(
        t_rel = as.integer(sub("^t_rel::", "", term)),
        lo = estimate - 1.96 * std_error,
        hi = estimate + 1.96 * std_error
      ) %>%
      arrange(t_rel)

    g <- ggplot(es, aes(t_rel, estimate)) +
      geom_hline(yintercept = 0, linetype = 2, linewidth = 0.8) +
      geom_vline(xintercept = -1, linetype = 3, linewidth = 0.6) +
      geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.15, alpha = 0.8) +
      geom_point(size = 2, color = "#1565C0") +
      scale_x_continuous(breaks = unique(es$t_rel)) +
      labs(
        title = "Event study: formalidad alrededor de ajuste del SML",
        x = "Trimestres relativos (ref = -1)",
        y = "Diferencia vs. ref (pp)"
      ) +
      plot_theme()

    ggplotly(g, tooltip = c("x", "y"))
  })

  output$t_es <- renderDT({
    est <- es_model()
    req(est)

    ct <- as.data.frame(fixest::coeftable(est))
    ct$term <- rownames(ct)
    rownames(ct) <- NULL
    names(ct)[1:4] <- c("estimate", "std_error", "statistic", "p_value")

    es <- ct %>%
      filter(grepl("^t_rel::", term)) %>%
      mutate(
        t_rel = as.integer(sub("^t_rel::", "", term)),
        estimate = round(estimate, 4),
        std_error = round(std_error, 4),
        p_value = round(p_value, 4)
      ) %>%
      arrange(t_rel) %>%
      select(t_rel, estimate, std_error, p_value)

    datatable(es, options = list(dom = "tip", pageLength = 12))
  })

  # Bunching
  output$p_bunch <- renderPlotly({
    d <- db()
    b <- tab_bunching(d, binwidth = input$b_bin, max_ratio = input$b_max)
    dens <- b$dens

    g <- ggplot(dens, aes(mid, w_mass / sum(w_mass, na.rm = TRUE))) +
      geom_col(width = input$b_bin, fill = "#1565C0", alpha = 0.85) +
      geom_vline(xintercept = 1.0, linetype = 2) +
      scale_y_continuous(labels = label_percent(accuracy = 0.1)) +
      labs(x = "Ingreso / SML", y = "Proporción", title = "Masa ponderada alrededor de 1 SML") +
      plot_theme()

    ggplotly(g, tooltip = c("x", "y"))
  })

  output$b_spike <- renderUI({
    d <- db()
    b <- tab_bunching(d, binwidth = input$b_bin, max_ratio = input$b_max)
    pct <- round(b$spike$pct_spike[1], 2)
    tags$p(tags$b("Share 0,98–1,02 SML (ponderado): "), paste0(pct, "%"))
  })

  # SML tab
  output$t_sml <- renderDT({
    sml <- get_sml_data(include_projection = TRUE) %>%
      filter(year >= 2022, mes == 7) %>%
      transmute(
        Año = year,
        Desde = format(fecha, "%d/%m/%Y"),
        `SML mensual (Gs.)` = fmt_gs(sml, 0),
        `Jornal diario aprox.` = fmt_gs(round(sml / 26, 0), 0),
        Nota = coalesce(tipo, "")
      )
    datatable(sml, options = list(dom = "tip", pageLength = 10))
  })

  output$p_sml <- renderPlotly({
    sml <- get_sml_data(include_projection = TRUE)
    monthly <- build_sml_monthly(sml, end_date = as.Date("2026-12-01"))

    g <- ggplot(monthly, aes(fecha, sml)) +
      geom_line(linewidth = 0.9, color = "#2E7D32") +
      scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
      labs(x = NULL, y = "Gs.", title = "Serie mensual del SML (con relleno hacia adelante)") +
      plot_theme()

    ggplotly(g, tooltip = c("x", "y"))
  })

  # Datos
  output$t_data <- renderDT({
    d <- db()
    keep <- intersect(
      c("anio", "q", "trimestredesc", "sexo", "ingoc1sml_cat", "edad", "anoest_val", "salario", "sal_hora", "cotiza_bin", "area_urb", "tam", "rama_pea", "w", "sml", "ratio_sml"),
      names(d)
    )
    datatable(dplyr::select(d, all_of(keep)), options = list(pageLength = 12, scrollX = TRUE))
  })

  output$dl_csv <- downloadHandler(
    filename = function() paste0("sml_filtrado_", Sys.Date(), ".csv"),
    content = function(file) {
      d <- db()
      keep <- intersect(
        c("anio", "q", "trimestredesc", "sexo", "ingoc1sml_cat", "edad", "anoest_val", "salario", "sal_hora", "cotiza_bin", "area_urb", "tam", "rama_pea", "w", "sml", "ratio_sml"),
        names(d)
      )
      readr::write_csv(dplyr::select(d, all_of(keep)), file)
    }
  )
}

shinyApp(ui, server)
