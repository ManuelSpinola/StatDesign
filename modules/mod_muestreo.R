# ============================================================
# mod_muestreo.R — Módulo: Diseños de muestreo
# Probabilísticos · No probabilísticos · Espaciales
# + Calculadora de tamaño de muestra
# + Esquema visual interactivo
# ============================================================

# ── Función: esquema espacial ──────────────────────────────
esquema_ggplot <- function(tipo, colores) {
  set.seed(123)

  base <- ggplot() +
    coord_fixed(xlim = c(0, 10), ylim = c(0, 10)) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid      = element_blank(),
      axis.text       = element_blank(),
      axis.title      = element_blank(),
      axis.ticks      = element_blank(),
      panel.border    = element_rect(color = colores$borde, fill = NA, linewidth = 0.8),
      plot.background = element_rect(fill = "#FFFFFF", color = NA),
      legend.position = "none",
      plot.title      = element_text(size = 10, color = colores$texto)
    )

  if (tipo == "alea_simple") {
    pts <- data.frame(x = runif(20, 0.5, 9.5), y = runif(20, 0.5, 9.5))
    base +
      geom_point(data = pts, aes(x, y),
                 color = colores$primario, size = 3.5, alpha = 0.85) +
      labs(title = "Aleatorio simple — 20 unidades seleccionadas al azar")

  } else if (tipo == "alea_estrat") {
    pts <- data.frame(
      x       = c(runif(7, 0.3, 3.3), runif(7, 3.7, 6.7), runif(7, 7.0, 9.7)),
      y       = runif(21, 0.5, 9.5),
      estrato = rep(c("Estrato A", "Estrato B", "Estrato C"), each = 7)
    )
    base +
      annotate("rect", xmin = 0,   xmax = 3.5, ymin = 0, ymax = 10,
               fill = colores$primario,   alpha = 0.08) +
      annotate("rect", xmin = 3.5, xmax = 6.8, ymin = 0, ymax = 10,
               fill = colores$acento,     alpha = 0.08) +
      annotate("rect", xmin = 6.8, xmax = 10,  ymin = 0, ymax = 10,
               fill = colores$secundario, alpha = 0.08) +
      geom_vline(xintercept = c(3.5, 6.8), linetype = "dashed",
                 color = colores$borde, linewidth = 0.7) +
      geom_point(data = pts, aes(x, y, color = estrato), size = 3.5, alpha = 0.9) +
      scale_color_manual(values = c(colores$primario, colores$acento, colores$secundario)) +
      annotate("text", x = c(1.75, 5.15, 8.4), y = 9.5,
               label = c("A", "B", "C"), size = 4, fontface = "bold",
               color = colores$texto) +
      labs(title = "Aleatorio estratificado — 3 estratos, 7 unidades c/u")

  } else if (tipo == "sistematico") {
    xs  <- seq(1.2, 9.2, by = 2)
    ys  <- seq(1.2, 9.2, by = 2)
    pts <- expand.grid(x = xs, y = ys)
    base +
      geom_hline(yintercept = ys, linetype = "dotted",
                 color = colores$borde, linewidth = 0.4) +
      geom_vline(xintercept = xs, linetype = "dotted",
                 color = colores$borde, linewidth = 0.4) +
      geom_point(data = pts, aes(x, y),
                 color = colores$primario, size = 3.5, shape = 15, alpha = 0.85) +
      labs(title = "Sistemático — grilla regular con inicio aleatorio")

  } else if (tipo == "conglomerados") {
    centros <- data.frame(cx = c(2, 6, 8), cy = c(7, 3, 7.5))
    pts <- do.call(rbind, lapply(1:3, function(i) {
      data.frame(
        x    = centros$cx[i] + rnorm(6, 0, 0.6),
        y    = centros$cy[i] + rnorm(6, 0, 0.6),
        cong = paste("Conglomerado", i)
      )
    }))
    no_sel <- data.frame(
      x = c(4.5, 4.5, 4.8, 1.5, 1.8, 1.2),
      y = c(1.5, 1.8, 1.2, 3.5, 3.2, 3.8)
    )
    base +
      geom_point(data = no_sel, aes(x, y),
                 color = "#A3ACB9", size = 2.5, alpha = 0.5) +
      geom_point(data = pts, aes(x, y, color = cong), size = 3.5, alpha = 0.9) +
      scale_color_manual(values = c(colores$primario, colores$acento, colores$secundario)) +
      annotate("text", x = centros$cx, y = centros$cy + 1.2,
               label = paste("C", 1:3), size = 3.5,
               color = colores$texto, fontface = "bold") +
      labs(title = "Por conglomerados — grupos seleccionados vs. no seleccionados (gris)")

  } else if (tipo == "conveniencia") {
    pts_acc <- data.frame(x = runif(15, 0.5, 4), y = runif(15, 0.5, 4))
    pts_no  <- data.frame(x = runif(10, 5, 9.5), y = runif(10, 5, 9.5))
    base +
      annotate("rect", xmin = 0, xmax = 4.2, ymin = 0, ymax = 4.2,
               fill = colores$primario, alpha = 0.07) +
      annotate("text", x = 2,   y = 4.6, label = "Zona accesible",
               size = 3.5, color = colores$primario) +
      annotate("text", x = 7.5, y = 9.5, label = "Zona inaccesible",
               size = 3.5, color = "#A3ACB9") +
      geom_point(data = pts_no,  aes(x, y), color = "#A3ACB9",      size = 2.5, alpha = 0.5) +
      geom_point(data = pts_acc, aes(x, y), color = colores$acento, size = 3.5, alpha = 0.85) +
      labs(title = "Por conveniencia — unidades de fácil acceso")

  } else if (tipo == "intencional") {
    ref <- data.frame(x = c(1.5, 3, 5, 8, 9), y = c(8, 6, 9, 7, 4))
    deg <- data.frame(x = c(2, 4, 7, 9, 6),   y = c(2, 3, 2, 1, 4))
    base +
      geom_point(data = ref, aes(x, y),
                 color = colores$primario, size = 4, shape = 17, alpha = 0.9) +
      geom_point(data = deg, aes(x, y),
                 color = colores$peligro,  size = 4, shape = 16, alpha = 0.9) +
      annotate("text", x = 9.5, y = 9.5,
               label = "\u25b2 Referencia", size = 3, color = colores$primario, hjust = 1) +
      annotate("text", x = 9.5, y = 8.8,
               label = "\u25cf Degradado",  size = 3, color = colores$peligro,  hjust = 1) +
      labs(title = "Intencional — sitios seleccionados por criterio experto")

  } else if (tipo == "transectos") {
    trans <- data.frame(x1 = c(1,1,1), x2 = c(9,9,9), y = c(2,5,8))
    pts   <- do.call(rbind, lapply(c(2,5,8), function(yt) {
      data.frame(x = seq(1.5, 8.5, by = 1.5), y = yt + rnorm(6, 0, 0.15))
    }))
    base +
      geom_segment(data = trans, aes(x = x1, xend = x2, y = y, yend = y),
                   color = colores$secundario, linewidth = 1) +
      geom_point(data = pts, aes(x, y),
                 color = colores$primario, size = 3, alpha = 0.85) +
      labs(title = "Transectos — 3 líneas con puntos de registro")

  } else if (tipo == "parcelas") {
    centros <- data.frame(cx = c(1.5, 4, 7, 2.5, 6, 8.5),
                          cy = c(1.5, 2, 1.5, 6,  7, 6.5))
    r <- 0.9
    base +
      geom_rect(data = centros,
                aes(xmin = cx - r, xmax = cx + r, ymin = cy - r, ymax = cy + r),
                fill = colores$primario, alpha = 0.15,
                color = colores$primario, linewidth = 0.7) +
      geom_point(data = centros, aes(cx, cy),
                 color = colores$primario, size = 2, shape = 3) +
      labs(title = "Parcelas — áreas delimitadas distribuidas en el área de estudio")

  } else if (tipo == "puntos") {
    pts <- data.frame(x = c(2, 5, 8, 3, 7, 5), y = c(2, 2, 2, 7, 7, 5))
    p   <- base +
      geom_point(data = pts, aes(x, y),
                 color = colores$primario, size = 4, shape = 16)
    for (i in seq_len(nrow(pts))) {
      p <- p + annotate("path",
                        x = pts$x[i] + 1.5 * cos(seq(0, 2*pi, length.out = 60)),
                        y = pts$y[i] + 1.5 * sin(seq(0, 2*pi, length.out = 60)),
                        color = colores$secundario, alpha = 0.4, linewidth = 0.6)
    }
    p + labs(title = "Muestreo en puntos — puntos fijos con radio de detección")

  } else if (tipo == "adaptativo") {
    xs   <- seq(0.5, 9.5, by = 2)
    ys   <- seq(0.5, 9.5, by = 2)
    grid <- expand.grid(x = xs, y = ys)
    det  <- grid[c(3, 8, 13), ]
    int  <- data.frame(
      x = c(det$x[1] + c(-2,0,2,0), det$x[2] + c(-2,0,2,0)),
      y = c(det$y[1] + c(0,2,0,-2), det$y[2] + c(0,2,0,-2))
    )
    base +
      geom_point(data = grid, aes(x, y), color = "#A3ACB9",          size = 2.5, alpha = 0.5) +
      geom_point(data = int,  aes(x, y), color = colores$secundario, size = 3.5, alpha = 0.8) +
      geom_point(data = det,  aes(x, y), color = colores$peligro,    size = 5,   shape = 8) +
      annotate("text", x = 9.5, y = 9.5,
               label = "\u2731 Detecci\u00f3n inicial", size = 3,
               color = colores$peligro, hjust = 1) +
      annotate("text", x = 9.5, y = 8.8,
               label = "\u25cf Intensificaci\u00f3n", size = 3,
               color = colores$secundario, hjust = 1) +
      labs(title = "Muestreo adaptativo — intensificación en zonas de detección")
  }
}

# ── UI ────────────────────────────────────────────────────
mod_muestreo_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    title = "\U0001f4d0 Diseños de muestreo",

    navset_card_tab(

      # ── Pestaña 1: Diseños ──────────────────────────────
      nav_panel(
        "Diseños de muestreo",

        div(
          class = "p-3",

          p(
            "El diseño de muestreo determina cómo se seleccionan las unidades de
             observación. Una muestra bien diseñada garantiza representatividad y
             permite hacer inferencias válidas sobre la población o el ecosistema
             de interés.",
            class = "text-muted mb-4"
          ),

          h5("Probabilísticos — permiten inferencia estadística",
             style = paste0("color:", colores$primario, "; border-bottom: 2px solid ",
                            colores$borde, "; padding-bottom: 0.4rem; margin-bottom: 1rem;")),
          uiOutput(ns("cards_prob")),

          br(),

          h5("No probabilísticos — exploratorios o con restricciones de acceso",
             style = paste0("color:", colores$acento, "; border-bottom: 2px solid ",
                            colores$borde, "; padding-bottom: 0.4rem; margin-bottom: 1rem;")),
          uiOutput(ns("cards_noprob")),

          br(),

          h5("Diseños espaciales — propios de ecología y ciencias ambientales",
             style = paste0("color:", colores$secundario, "; border-bottom: 2px solid ",
                            colores$borde, "; padding-bottom: 0.4rem; margin-bottom: 1rem;")),
          uiOutput(ns("cards_espacial"))
        )
      ),

      # ── Pestaña 2: Esquema visual ────────────────────────
      nav_panel(
        "Esquema visual",

        div(
          class = "p-3",

          layout_sidebar(
            fillable = FALSE,

            sidebar = sidebar(
              width = 240,
              title = "Tipo de muestreo",

              selectInput(
                ns("esquema_sel"),
                label = NULL,
                choices = list(
                  "Probabilísticos" = c(
                    "Aleatorio simple"        = "alea_simple",
                    "Aleatorio estratificado" = "alea_estrat",
                    "Sistemático"             = "sistematico",
                    "Por conglomerados"       = "conglomerados"
                  ),
                  "No probabilísticos" = c(
                    "Por conveniencia" = "conveniencia",
                    "Intencional"      = "intencional"
                  ),
                  "Espaciales" = c(
                    "Transectos"          = "transectos",
                    "Parcelas"            = "parcelas",
                    "Muestreo en puntos"  = "puntos",
                    "Muestreo adaptativo" = "adaptativo"
                  )
                )
              ),

              hr(),
              div(
                class = "small text-muted",
                "El esquema muestra una representación espacial simplificada del
                 diseño sobre un área ficticia de 10 × 10 unidades."
              )
            ),

            div(
              card(
                card_header("Representación espacial"),
                plotOutput(ns("esquema_plot"), height = "320px")
              ),

              br(),

              uiOutput(ns("esquema_detalle"))
            )
          )
        )
      ),

      # ── Pestaña 3: Calculadora ──────────────────────────
      nav_panel(
        "Calculadora de tamaño de muestra",

        div(
          class = "p-3",

          layout_sidebar(
            fillable = FALSE,

            sidebar = sidebar(
              width = 280,
              title = "Parámetros",

              selectInput(
                ns("calc_tipo"),
                "Tipo de variable respuesta",
                choices = c(
                  "Proporción (presencia/ausencia, cobertura)" = "prop",
                  "Media continua (biomasa, concentración)"    = "media"
                )
              ),

              conditionalPanel(
                condition = paste0("input['", ns("calc_tipo"), "'] == 'prop'"),
                numericInput(ns("calc_p"), "Proporción esperada (p)",
                             value = 0.5, min = 0.01, max = 0.99, step = 0.01)
              ),

              conditionalPanel(
                condition = paste0("input['", ns("calc_tipo"), "'] == 'media'"),
                numericInput(ns("calc_cv"), "Coeficiente de variación esperado (%)",
                             value = 30, min = 1, max = 200, step = 1)
              ),

              selectInput(
                ns("calc_z"),
                "Nivel de confianza",
                choices  = c("95%" = 1.96, "99%" = 2.576, "90%" = 1.645),
                selected = 1.96
              ),

              numericInput(
                ns("calc_e"),
                "Error máximo aceptable (e)",
                value = 0.05, min = 0.01, max = 0.5, step = 0.01
              ),

              numericInput(
                ns("calc_N"),
                "Tamaño de población (N) — 0 = infinita",
                value = 0, min = 0, step = 1
              ),

              hr(),
              div(
                class = "small text-muted",
                strong("Fórmula usada:"), br(),
                uiOutput(ns("formula_texto"))
              )
            ),

            div(
              layout_columns(
                col_widths = c(4, 4, 4),

                value_box(
                  title    = "Muestra base (n\u2080)",
                  value    = textOutput(ns("res_n0")),
                  showcase = bsicons::bs_icon("people-fill"),
                  theme    = "primary"
                ),

                value_box(
                  title    = "Con corrección finita (n)",
                  value    = textOutput(ns("res_nc")),
                  showcase = bsicons::bs_icon("funnel-fill"),
                  theme    = "info"
                ),

                value_box(
                  title    = "Fracción de muestreo",
                  value    = textOutput(ns("res_frac")),
                  showcase = bsicons::bs_icon("percent"),
                  theme    = "secondary"
                )
              ),

              br(),

              card(
                card_header("Interpretación"),
                uiOutput(ns("interpretacion"))
              ),

              card(
                card_header("Guía pedagógica"),
                uiOutput(ns("pedagogico"))
              ),

              card(
                card_header("Fórmula"),
                uiOutput(ns("formula_detalle"))
              )
            )
          )
        )
      )
    )
  )
}

# ── Server ────────────────────────────────────────────────
mod_muestreo_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # ── Renderizar cards de diseños ──────────────────────
    render_cards <- function(grupo) {
      items <- diseños_muestreo[[grupo]]$items
      badge <- diseños_muestreo[[grupo]]$badge

      renderUI({
        layout_columns(
          col_widths = rep(6, length(items)),
          !!!lapply(items, function(d) {
            card(
              class = "card-muestreo",
              card_header(
                div(
                  span(class = paste("badge badge-", badge, " me-2", sep = ""), badge_label(badge)),
                  strong(d$nombre)
                )
              ),
              p(d$desc, class = "text-muted small"),
              tags$dl(
                class = "row small mb-0",
                tags$dt(class = "col-sm-4", "¿Cuándo usar?"),
                tags$dd(class = "col-sm-8", d$cuando),
                tags$dt(class = "col-sm-4", "Ventaja"),
                tags$dd(class = "col-sm-8", d$ventaja),
                tags$dt(class = "col-sm-4", "Limitación"),
                tags$dd(class = "col-sm-8", d$limitacion)
              ),
              hr(class = "my-2"),
              p(class = "fst-italic small text-muted mb-0",
                tags$strong("Ejemplo: "), d$ejemplo)
            )
          })
        )
      })
    }

    output$cards_prob     <- render_cards("probabilisticos")
    output$cards_noprob   <- render_cards("no_probabilisticos")
    output$cards_espacial <- render_cards("espaciales")

    # ── Calculadora ──────────────────────────────────────
    calc <- reactive({
      z  <- as.numeric(input$calc_z)
      e  <- input$calc_e
      N  <- input$calc_N

      n0 <- if (input$calc_tipo == "prop") {
        p <- input$calc_p
        ceiling((z^2 * p * (1 - p)) / e^2)
      } else {
        cv <- input$calc_cv / 100
        ceiling((z * cv / e)^2)
      }

      nc   <- if (N > 0 && N > n0) ceiling(n0 / (1 + n0 / N)) else NA
      frac <- if (N > 0) {
        n_usar <- ifelse(is.na(nc), n0, nc)
        paste0(round(n_usar / N * 100, 1), "%")
      } else "—"

      list(
        n0   = n0, nc = nc, frac = frac, N = N, z = z, e = e,
        tipo = input$calc_tipo,
        p    = if (input$calc_tipo == "prop")  input$calc_p  else NULL,
        cv   = if (input$calc_tipo == "media") input$calc_cv else NULL
      )
    })

    output$res_n0   <- renderText({ format(calc()$n0, big.mark = ",") })
    output$res_nc   <- renderText({
      if (is.na(calc()$nc)) "N/A" else format(calc()$nc, big.mark = ",")
    })
    output$res_frac <- renderText({ calc()$frac })

    output$formula_texto <- renderUI({
      if (input$calc_tipo == "prop") "n\u2080 = z\u00b2 \u00b7 p(1-p) / e\u00b2"
      else                           "n\u2080 = (z \u00b7 CV / e)\u00b2"
    })

    output$formula_detalle <- renderUI({
      r <- calc()
      if (input$calc_tipo == "prop") {
        withMathJax(
          p("$$n_0 = \\frac{z^2 \\cdot p(1-p)}{e^2}$$"),
          p(class = "small text-muted",
            "Donde z = valor crítico normal, p = proporción esperada, e = error máximo aceptable.")
        )
      } else {
        withMathJax(
          p("$$n_0 = \\left(\\frac{z \\cdot CV}{e}\\right)^2$$"),
          p(class = "small text-muted",
            "Donde CV = coeficiente de variación (decimal), e = error máximo aceptable.")
        )
      }
    })

    output$interpretacion <- renderUI({
      r <- calc()
      if (r$N > 0 && !is.na(r$nc) && r$nc < r$n0) {
        div(
          class = "wiz-result",
          p(sprintf(
            "Con una población finita de %s unidades, la corrección reduce
             la muestra de %s a %s unidades (fracción de muestreo: %s).",
            format(r$N,  big.mark = ","),
            format(r$n0, big.mark = ","),
            format(r$nc, big.mark = ","),
            r$frac
          ))
        )
      } else if (r$N > 0 && r$N <= r$n0) {
        div(
          class = "alert alert-warning",
          sprintf(
            "La muestra calculada (%s) es mayor o igual a la población (%s).
             Considera censar toda la población.",
            format(r$n0, big.mark = ","),
            format(r$N,  big.mark = ",")
          )
        )
      } else {
        p("Muestra calculada para población infinita o muy grande.
           Si conoces el tamaño de tu población, ingrésalo para aplicar
           la corrección por población finita.",
          class = "text-muted")
      }
    })

    # ── Card pedagógico ───────────────────────────────────
    output$pedagogico <- renderUI({
      r <- calc()

      nivel_conf <- ifelse(abs(r$z - 1.96)   < 0.01, "95",
                           ifelse(abs(r$z - 2.576)  < 0.01, "99", "90"))

      # — Explicación del error máximo aceptable —
      txt_e <- if (r$tipo == "prop") {
        sprintf(
          "El error máximo aceptable (e = %.2f) es el margen de error a cada lado
           de tu estimación. Significa que la proporción estimada estará a ±%d puntos
           porcentuales del valor real, con un %s%% de confianza. Por ejemplo, si
           estimas que el 40%% de los sitios presenta cierta condición, el intervalo
           sería aproximadamente [%d%%, %d%%].",
          r$e,
          round(r$e * 100),
          nivel_conf,
          round((0.4 - r$e) * 100),
          round((0.4 + r$e) * 100)
        )
      } else {
        sprintf(
          "El error máximo aceptable (e = %.2f) es un error relativo: la media
           estimada no diferirá del valor real en más de un %d%%. Con un CV de %d%%,
           esto implica que la variabilidad del sistema requiere al menos %s unidades
           para alcanzar esa precisión con un %s%% de confianza.",
          r$e,
          round(r$e * 100),
          round(r$cv),
          format(r$n0, big.mark = ","),
          nivel_conf
        )
      }

      # — Explicación de la fracción de muestreo —
      txt_frac <- if (r$N > 0) {
        n_usar   <- ifelse(is.na(r$nc), r$n0, r$nc)
        frac_num <- n_usar / r$N * 100
        if (frac_num >= 20) {
          sprintf(
            "La fracción de muestreo es alta (%s): estás cubriendo más del 20%% de
             la población. Esto activa la corrección por población finita, que reduce
             la muestra necesaria. Con poblaciones tan pequeñas relativas a la muestra,
             conviene evaluar si es factible censar a toda la población.",
            r$frac
          )
        } else if (frac_num >= 5) {
          sprintf(
            "La fracción de muestreo (%s) es moderada. La corrección por población
             finita tiene un efecto apreciable y reduce la muestra de %s a %s unidades
             sin perder precisión estadística. Esto puede representar un ahorro
             importante de recursos en campo.",
            r$frac,
            format(r$n0, big.mark = ","),
            format(r$nc, big.mark = ",")
          )
        } else {
          sprintf(
            "La fracción de muestreo es muy pequeña (%s): la población es mucho mayor
             que la muestra necesaria. En este caso la corrección por población finita
             tiene un efecto mínimo y la muestra base (n\u2080 = %s) es prácticamente
             la definitiva.",
            r$frac,
            format(r$n0, big.mark = ",")
          )
        }
      } else {
        "Ingresa el tamaño de tu población (N) para ver el efecto de la corrección
         por población finita y la fracción de muestreo correspondiente."
      }

      # — Recomendación según precisión —
      txt_rec <- if (r$tipo == "prop") {
        if (r$e <= 0.03) {
          "Con e \u2264 0.03 tienes alta precisión, adecuada para estudios donde
           pequeñas diferencias son ecológicamente relevantes (p.ej. prevalencia
           de enfermedades, cobertura de especies raras)."
        } else if (r$e <= 0.05) {
          "Con e = 0.05 (\u00b15%) tienes la precisión estándar recomendada para
           la mayoría de estudios en ciencias ambientales y recursos naturales."
        } else {
          "Con e > 0.05 la precisión es baja. Es aceptable en estudios exploratorios
           o con recursos limitados, pero los resultados deben interpretarse con cautela."
        }
      } else {
        if (r$e <= 0.10) {
          "Con e \u2264 0.10 (10%) tienes buena precisión para estimar medias continuas.
           Es el estándar en inventarios forestales y estudios de biomasa."
        } else {
          "Con e > 0.10 la estimación de la media tiene baja precisión. Considera
           reducir el error o aumentar el esfuerzo de muestreo si la variable es
           crítica para la toma de decisiones."
        }
      }

      div(
        p(txt_e,    class = "small mb-3"),
        hr(class = "my-2"),
        p(txt_frac, class = "small mb-3"),
        hr(class = "my-2"),
        div(
          class = "wiz-result",
          p(strong("\U0001f4a1 Recomendación: "), txt_rec, class = "small mb-0")
        )
      )
    })

    # ── Esquema visual ────────────────────────────────────
    output$esquema_plot <- renderPlot({
      esquema_ggplot(input$esquema_sel, colores)
    }, res = 110)

    output$esquema_detalle <- renderUI({
      claves <- c("alea_simple", "alea_estrat", "sistematico", "conglomerados",
                  "conveniencia", "intencional",
                  "transectos", "parcelas", "puntos", "adaptativo")
      todos  <- unlist(lapply(diseños_muestreo, function(g) g$items), recursive = FALSE)
      item   <- todos[[which(claves == input$esquema_sel)]]

      div(
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header("Descripción"),
            p(item$desc, class = "text-muted small"),
            tags$dl(
              class = "row small mb-0",
              tags$dt(class = "col-sm-4", "¿Cuándo usar?"),
              tags$dd(class = "col-sm-8", item$cuando),
              tags$dt(class = "col-sm-4", "Ventaja"),
              tags$dd(class = "col-sm-8", item$ventaja),
              tags$dt(class = "col-sm-4", "Limitación"),
              tags$dd(class = "col-sm-8", item$limitacion)
            )
          ),
          card(
            card_header("Ejemplo"),
            div(
              class = "wiz-result",
              p(item$ejemplo, class = "mb-0 fst-italic small")
            )
          )
        )
      )
    })

  })
}

# ── Helper interno ────────────────────────────────────────
badge_label <- function(badge) {
  switch(badge,
         prob     = "Probabilístico",
         noprob   = "No probabilístico",
         espacial = "Espacial"
  )
}
