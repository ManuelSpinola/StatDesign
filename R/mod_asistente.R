# ============================================================
# mod_asistente.R — Módulo: Asistente de diseño
# Árbol condicional:
#   Describir                → Descriptivo (1 pregunta)
#   Identificar relaciones   → ¿Manipulás? → Observacional / Experimental
#   Determinar causa y efecto → Experimental (1 pregunta)
# ============================================================

# ── Pregunta 1: objetivo ──────────────────────────────────
pregunta_objetivo <- list(
  id   = "objetivo",
  q    = "¿Cuál es tu objetivo principal?",
  sub  = "Esto define el tipo de pregunta de investigación que guiará el diseño.",
  opts = list(
    list(val = "desc", t = "Describir un fenómeno o sistema",
         d = "Caracterizar cómo se presenta algo: distribución de especies, calidad ambiental, uso de recursos."),
    list(val = "obs",  t = "Identificar relaciones o asociaciones",
         d = "Saber si dos o más variables están relacionadas: cobertura y sedimentos, temperatura y diversidad."),
    list(val = "exp",  t = "Determinar causa y efecto",
         d = "Comprobar si una intervención genera un cambio medible y atribuible.")
  )
)

# ── Pregunta 2: manipulación (solo si objetivo == "obs") ──
pregunta_manipulacion <- list(
  id   = "manipulacion",
  q    = "¿Vas a manipular alguna variable o aplicar un tratamiento?",
  sub  = "La manipulación intencional es lo que distingue un experimento de un estudio observacional.",
  opts = list(
    list(val = "no",  t = "No, solo voy a observar y medir",
         d = "Registro de variables en el estado natural del sistema, sin intervenir."),
    list(val = "yes", t = "Sí, voy a intervenir (tratamiento, restauración, manejo)",
         d = "El investigador aplica un tratamiento: revegetación, remoción de especie invasora, fertilización, etc.")
  )
)

# ── Lógica de flujo ───────────────────────────────────────
# Devuelve las preguntas que corresponden según las respuestas acumuladas
preguntas_activas <- function(ans) {
  obj <- ans[["objetivo"]]
  if (is.null(obj)) return(list(pregunta_objetivo))
  if (obj == "obs")  return(list(pregunta_objetivo, pregunta_manipulacion))
  return(list(pregunta_objetivo))  # desc o exp: solo 1 pregunta
}

# ── Lógica de recomendación ───────────────────────────────
recomendar_tipo <- function(ans) {
  obj <- ans[["objetivo"]]
  if (obj == "desc") return("descriptivo")
  if (obj == "exp")  return("experimental")
  # obj == "obs": depende de si manipula
  if (!is.null(ans[["manipulacion"]]) && ans[["manipulacion"]] == "yes")
    return("experimental")
  return("observacional")
}

# ── UI ────────────────────────────────────────────────────
mod_asistente_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "p-3",
      style = "max-width: 700px; margin: 0 auto;",

      uiOutput(ns("progreso")),
      br(),
      uiOutput(ns("contenido")),
      br(),
      uiOutput(ns("navegacion"))
    )
  )
}

# ── Server ────────────────────────────────────────────────
mod_asistente_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    paso       <- reactiveVal(1)
    respuestas <- reactiveVal(list())

    # Preguntas activas según respuestas acumuladas
    preguntas <- reactive({
      preguntas_activas(respuestas())
    })

    n_pasos <- reactive({ length(preguntas()) })

    # ── Barra de progreso ──────────────────────────────
    output$progreso <- renderUI({
      p <- paso()
      n <- n_pasos()
      if (p > n) return(NULL)

      div(
        class = "d-flex align-items-center gap-2 mb-2",
        span(class = "small text-muted",
             paste0("Pregunta ", p, " de ", n)),
        div(
          class = "progress flex-grow-1",
          style = "height: 6px;",
          div(
            class = "progress-bar",
            style = paste0(
              "width: ", round(p / n * 100), "%;",
              "background-color: ", colores$acento, ";"
            )
          )
        )
      )
    })

    # ── Contenido principal ────────────────────────────
    output$contenido <- renderUI({
      p <- paso()
      n <- n_pasos()

      # Resultado final
      if (p > n) {
        ans  <- respuestas()
        tipo <- recomendar_tipo(ans)
        t    <- tipos_estudio[[tipo]]

        div(
          div(
            class = "wiz-result mb-3",
            h4(paste("Tipo recomendado:", t$icono, t$nombre),
               style = paste0("color:", colores$primario)),
            p(t$definicion),
            hr(),
            div(
              class = "row g-2",
              div(class = "col-md-6",
                  strong("¿Cuándo usarlo?"), br(),
                  span(class = "small text-muted", t$cuando)
              ),
              div(class = "col-md-6",
                  strong("Análisis estadísticos recomendados"), br(),
                  span(class = "small text-muted", t$estadisticas)
              ),
              div(class = "col-md-6 mt-2",
                  strong("Diseños compatibles"), br(),
                  span(class = "small text-muted",
                       paste(names(t$diseños), collapse = " · "))
              )
            )
          ),
          card(
            card_header("Ejemplo"),
            p(class = "fst-italic text-muted mb-0", t$ejemplo)
          )
        )
      } else {
        # Pregunta actual
        preg <- preguntas()[[p]]
        ans  <- respuestas()
        sel  <- ans[[preg$id]]

        div(
          h5(preg$q, style = paste0("color:", colores$primario)),
          p(preg$sub, class = "text-muted small mb-3"),
          div(
            lapply(preg$opts, function(o) {
              es_sel <- !is.null(sel) && sel == o$val
              actionButton(
                ns(paste0("opt_", o$val)),
                label = div(
                  strong(o$t), br(),
                  span(class = "small text-muted", o$d)
                ),
                class = paste("wiz-opt w-100 text-start mb-2",
                              if (es_sel) "border border-primary" else ""),
                style = if (es_sel) paste0("border-color:", colores$primario,
                                           " !important; background:", colores$fondo) else ""
              )
            })
          )
        )
      }
    })

    # ── Navegación ─────────────────────────────────────
    output$navegacion <- renderUI({
      p <- paso()
      n <- n_pasos()

      if (p > n) {
        div(
          actionButton(ns("reiniciar"), "↺ Reiniciar",
                       class = "btn btn-outline-secondary")
        )
      } else {
        ans        <- respuestas()
        preg       <- preguntas()[[p]]
        tiene_resp <- !is.null(ans[[preg$id]])

        div(
          class = "d-flex justify-content-between",
          actionButton(ns("anterior"), "← Anterior",
                       class = "btn btn-outline-secondary",
                       disabled = (p == 1)),
          actionButton(ns("siguiente"), "Siguiente →",
                       class = "btn btn-primary",
                       disabled = !tiene_resp)
        )
      }
    })

    # ── Observers: selección de opciones ───────────────
    observe({
      p <- paso()
      n <- n_pasos()
      if (p > n) return()
      preg <- preguntas()[[p]]

      lapply(preg$opts, function(o) {
        btn_id <- paste0("opt_", o$val)
        observeEvent(input[[btn_id]], {
          ans <- respuestas()
          ans[[preg$id]] <- o$val
          # Si cambia objetivo, limpiar manipulacion para evitar respuestas huerfanas
          if (preg$id == "objetivo") ans[["manipulacion"]] <- NULL
          respuestas(ans)
        }, ignoreInit = TRUE)
      })
    })

    # ── Siguiente / Anterior ───────────────────────────
    observeEvent(input$siguiente, {
      p    <- paso()
      preg <- preguntas()[[p]]
      ans  <- respuestas()
      if (!is.null(ans[[preg$id]])) paso(p + 1)
    })

    observeEvent(input$anterior, {
      p <- paso()
      if (p > 1) paso(p - 1)
    })

    # ── Reiniciar ──────────────────────────────────────
    observeEvent(input$reiniciar, {
      paso(1)
      respuestas(list())
    })

    # ── Devuelve tipo recomendado ──────────────────────
    tipo_recomendado <- reactive({
      if (paso() > n_pasos()) recomendar_tipo(respuestas()) else NULL
    })

    return(tipo_recomendado)
  })
}
