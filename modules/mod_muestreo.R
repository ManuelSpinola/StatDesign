# ============================================================
# mod_muestreo.R — Módulo: Diseños de muestreo
# Probabilísticos · No probabilísticos · Espaciales
# + Calculadora de tamaño de muestra
# ============================================================

# ── UI ────────────────────────────────────────────────────
mod_muestreo_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    title = "📐 Diseños de muestreo",

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

          # Probabilísticos
          h5("Probabilísticos — permiten inferencia estadística",
             style = paste0("color:", colores$primario, "; border-bottom: 2px solid ",
                            colores$borde, "; padding-bottom: 0.4rem; margin-bottom: 1rem;")),
          uiOutput(ns("cards_prob")),

          br(),

          # No probabilísticos
          h5("No probabilísticos — exploratorios o con restricciones de acceso",
             style = paste0("color:", colores$acento, "; border-bottom: 2px solid ",
                            colores$borde, "; padding-bottom: 0.4rem; margin-bottom: 1rem;")),
          uiOutput(ns("cards_noprob")),

          br(),

          # Espaciales
          h5("Diseños espaciales — propios de ecología y ciencias ambientales",
             style = paste0("color:", colores$secundario, "; border-bottom: 2px solid ",
                            colores$borde, "; padding-bottom: 0.4rem; margin-bottom: 1rem;")),
          uiOutput(ns("cards_espacial"))
        )
      ),

      # ── Pestaña 2: Calculadora ──────────────────────────
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

              # Solo para proporción
              conditionalPanel(
                condition = paste0("input['", ns("calc_tipo"), "'] == 'prop'"),
                numericInput(ns("calc_p"), "Proporción esperada (p)",
                             value = 0.5, min = 0.01, max = 0.99, step = 0.01)
              ),

              # Solo para media
              conditionalPanel(
                condition = paste0("input['", ns("calc_tipo"), "'] == 'media'"),
                numericInput(ns("calc_cv"), "Coeficiente de variación esperado (%)",
                             value = 30, min = 1, max = 200, step = 1)
              ),

              selectInput(
                ns("calc_z"),
                "Nivel de confianza",
                choices = c("95%" = 1.96, "99%" = 2.576, "90%" = 1.645),
                selected = 1.96
              ),

              numericInput(
                ns("calc_e"),
                "Error máximo aceptable (e)",
                value = 0.1, min = 0.01, max = 0.5, step = 0.01
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

            # Resultados
            div(

              layout_columns(
                col_widths = c(4, 4, 4),

                value_box(
                  title    = "Muestra base (n₀)",
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
      z <- as.numeric(input$calc_z)
      e <- input$calc_e
      N <- input$calc_N

      n0 <- if (input$calc_tipo == "prop") {
        p  <- input$calc_p
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

      list(n0 = n0, nc = nc, frac = frac, N = N, z = z, e = e)
    })

    output$res_n0   <- renderText({ format(calc()$n0, big.mark = ",") })
    output$res_nc   <- renderText({
      if (is.na(calc()$nc)) "N/A" else format(calc()$nc, big.mark = ",")
    })
    output$res_frac <- renderText({ calc()$frac })

    output$formula_texto <- renderUI({
      if (input$calc_tipo == "prop") {
        "n₀ = z² · p(1-p) / e²"
      } else {
        "n₀ = (z · CV / e)²"
      }
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
          p(
            sprintf(
              "Con una población finita de %s unidades, la corrección reduce
               la muestra de %s a %s unidades (fracción de muestreo: %s).",
              format(r$N, big.mark = ","),
              format(r$n0, big.mark = ","),
              format(r$nc, big.mark = ","),
              r$frac
            )
          )
        )
      } else if (r$N > 0 && r$N <= r$n0) {
        div(
          class = "alert alert-warning",
          sprintf(
            "La muestra calculada (%s) es mayor o igual a la población (%s).
             Considera censar toda la población.",
            format(r$n0, big.mark = ","),
            format(r$N, big.mark = ",")
          )
        )
      } else {
        p(
          "Muestra calculada para población infinita o muy grande.
           Si conoces el tamaño de tu población, ingrésalo para aplicar
           la corrección por población finita.",
          class = "text-muted"
        )
      }
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
