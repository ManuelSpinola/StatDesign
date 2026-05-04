# ============================================================
# mod_tipos.R — Módulo: Tipos de estudio
# Descriptivo · Observacional/Correlacional · Experimental
# ============================================================

# ── UI ────────────────────────────────────────────────────
mod_tipos_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    title = "📚 Tipos de estudio",

    layout_sidebar(
      fillable = FALSE,

      # ── Sidebar: selector de tipo ──────────────────────
      sidebar = sidebar(
        width = 220,
        title = "Tipo de estudio",

        radioButtons(
          ns("tipo_sel"),
          label    = NULL,
          choices  = c(
            "📋 Descriptivo"               = "descriptivo",
            "🔗 Observacional / Correlacional" = "observacional",
            "🧪 Experimental"              = "experimental"
          ),
          selected = "descriptivo"
        ),

        hr(),

        div(
          class = "small text-muted",
          "Selecciona un tipo para explorar su definición, características principales, contextos de aplicación y un ejemplo concreto."
        )
      ),

      # ── Contenido principal ────────────────────────────
      div(

        # Encabezado dinámico
        uiOutput(ns("encabezado")),

        hr(),

        # Fila: características + limitaciones
        layout_columns(
          col_widths = c(6, 6),

          card(
            card_header("Características"),
            uiOutput(ns("caracteristicas"))
          ),

          card(
            card_header("Limitaciones"),
            uiOutput(ns("limitaciones"))
          )
        ),

        # Diseños frecuentes
        card(
          card_header("Diseños frecuentes en ciencias ambientales"),
          uiOutput(ns("disenios"))
        ),

        # Estadísticas típicas
        card(
          card_header("Estadísticas típicas"),
          uiOutput(ns("estadisticas"))
        ),

        # Ejemplo
        card(
          card_header("Ejemplo ambiental"),
          uiOutput(ns("ejemplo"))
        )
      )
    )
  )
}

# ── Server ────────────────────────────────────────────────
mod_tipos_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Tipo seleccionado (reactivo)
    tipo_actual <- reactive({
      tipos_estudio[[input$tipo_sel]]
    })

    # Encabezado
    output$encabezado <- renderUI({
      t <- tipo_actual()
      div(
        h3(paste(t$icono, t$nombre),
           style = paste0("color:", colores$primario, "; margin-bottom: 0.25rem;")),
        p(t$definicion, class = "lead", style = "color: #57606C;")
      )
    })

    # Características
    output$caracteristicas <- renderUI({
      tags$ul(
        lapply(tipo_actual()$caracteristicas, function(x) tags$li(x))
      )
    })

    # Limitaciones
    output$limitaciones <- renderUI({
      tags$ul(
        lapply(tipo_actual()$limitaciones, function(x) tags$li(x))
      )
    })

    # Diseños frecuentes
    output$disenios <- renderUI({
      d <- tipo_actual()$diseños
      div(
        lapply(seq_along(d), function(i) {
          div(
            class = "card-muestreo",
            strong(names(d)[i]),
            p(d[[i]], class = "mb-0 small text-muted")
          )
        })
      )
    })

    # Estadísticas típicas
    output$estadisticas <- renderUI({
      p(tipo_actual()$estadisticas)
    })

    # Ejemplo
    output$ejemplo <- renderUI({
      div(
        class = "wiz-result",
        p(tipo_actual()$ejemplo, class = "mb-0 fst-italic")
      )
    })

  })
}
