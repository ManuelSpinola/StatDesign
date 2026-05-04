# ============================================================
# mod_asistente.R — Módulo: Asistente de diseño
# Guía al usuario en 3 preguntas y recomienda el tipo de
# estudio más adecuado para su pregunta de investigación.
# ============================================================

# ── Preguntas del wizard ──────────────────────────────────
preguntas <- list(
  list(
    id   = "objetivo",
    q    = "¿Cuál es tu objetivo principal?",
    sub  = "Esto define el tipo de pregunta de investigación que guiará el diseño.",
    opts = list(
      list(val = "desc", t = "Describir un fenómeno o sistema",
           d = "Caracterizar cómo se presenta algo: distribución de especies, calidad ambiental, uso de recursos."),
      list(val = "obs",  t = "Identificar relaciones o asociaciones",
           d = "Saber si dos o más variables están relacionadas: cobertura y sedimentos, temperatura y diversidad."),
      list(val = "exp",  t = "Determinar causa y efecto",
           d = "Comprobar si una intervención o variable produce un cambio medible en otra.")
    )
  ),
  list(
    id   = "manipulacion",
    q    = "¿Vas a manipular alguna variable o aplicar un tratamiento?",
    sub  = "La manipulación intencional es lo que define un experimento en ciencias ambientales.",
    opts = list(
      list(val = "no",  t = "No, solo voy a observar y medir",
           d = "Registro de variables en el estado natural del sistema, sin intervenir."),
      list(val = "yes", t = "Sí, voy a intervenir (tratamiento, restauración, manejo)",
           d = "El investigador aplica un tratamiento: revegetación, remoción de especie invasora, fertilización, etc.")
    )
  ),
  list(
    id   = "causalidad",
    q    = "¿Buscas establecer causalidad o identificar asociaciones?",
    sub  = "Esto determina el nivel de evidencia requerido y la complejidad del análisis.",
    opts = list(
      list(val = "assoc",  t = "Asociación o correlación",
           d = "Me interesa cuantificar si hay relación y en qué dirección, no necesariamente la causa."),
      list(val = "causal", t = "Causalidad (demostrar que X produce Y)",
           d = "Necesito evidencia de que la variable que manipulé generó el cambio observado.")
    )
  )
)

# ── Lógica de recomendación ───────────────────────────────
recomendar_tipo <- function(ans) {
  if (ans[["manipulacion"]] == "yes" || ans[["causalidad"]] == "causal") return("experimental")
  if (ans[["objetivo"]] == "desc") return("descriptivo")
  return("observacional")
}

# ── UI ────────────────────────────────────────────────────
mod_asistente_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    title = "🧭 Asistente de diseño",

    div(
      class = "p-3",
      style = "max-width: 700px; margin: 0 auto;",

      # Barra de progreso
      uiOutput(ns("progreso")),

      br(),

      # Contenido dinámico: pregunta o resultado
      uiOutput(ns("contenido")),

      br(),

      # Navegación
      uiOutput(ns("navegacion"))
    )
  )
}

# ── Server ────────────────────────────────────────────────
mod_asistente_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Estado reactivo
    paso     <- reactiveVal(1)
    respuestas <- reactiveVal(list())

    n_pasos <- length(preguntas)

    # ── Barra de progreso ──────────────────────────────
    output$progreso <- renderUI({
      p <- paso()
      if (p > n_pasos) return(NULL)

      div(
        class = "d-flex align-items-center gap-2 mb-2",
        span(class = "small text-muted",
             paste0("Pregunta ", p, " de ", n_pasos)),
        div(
          class = "progress flex-grow-1",
          style = "height: 6px;",
          div(
            class = "progress-bar",
            style = paste0(
              "width: ", round(p / n_pasos * 100), "%;",
              "background-color: ", colores$acento, ";"
            )
          )
        )
      )
    })

    # ── Contenido principal ────────────────────────────
    output$contenido <- renderUI({
      p <- paso()

      # Resultado final
      if (p > n_pasos) {
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
                span(class = "small text-muted",
                     "Cuando no es ético o factible experimentar a escala ecosistémica,
                      o cuando se quieren explorar patrones naturales.")
              ),
              div(class = "col-md-6",
                strong("Estadísticas típicas"), br(),
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
            card_header("Ejemplo ambiental"),
            p(class = "fst-italic text-muted mb-0", t$ejemplo)
          )
        )
      } else {
        # Pregunta actual
        preg <- preguntas[[p]]
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

      if (p > n_pasos) {
        # Botones del resultado
        div(
          class = "d-flex gap-2",
          actionButton(ns("reiniciar"), "↺ Reiniciar",
                       class = "btn btn-outline-secondary"),
          actionButton(ns("ver_tipo"), "Ver detalle del tipo →",
                       class = "btn btn-primary")
        )
      } else {
        ans <- respuestas()
        preg <- preguntas[[p]]
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
      p    <- paso()
      if (p > n_pasos) return()
      preg <- preguntas[[p]]

      lapply(preg$opts, function(o) {
        btn_id <- paste0("opt_", o$val)
        observeEvent(input[[btn_id]], {
          ans <- respuestas()
          ans[[preg$id]] <- o$val
          respuestas(ans)
        }, ignoreInit = TRUE)
      })
    })

    # ── Siguiente / Anterior ───────────────────────────
    observeEvent(input$siguiente, {
      p    <- paso()
      preg <- preguntas[[p]]
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

    # ── Ver detalle (devuelve el tipo para navegación) ─
    tipo_recomendado <- reactive({
      if (paso() > n_pasos) recomendar_tipo(respuestas()) else NULL
    })

    return(tipo_recomendado)
  })
}
