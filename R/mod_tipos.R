# ============================================================
# mod_tipos.R — Módulo: Tipos de estudio
# Estructura: navset_card_tab (una pestaña por tipo)
# ============================================================

# ── Helpers internos ──────────────────────────────────────

# Construye el contenido de una pestaña dado un item de tipos_estudio
tab_contenido <- function(t) {
  tagList(

    # Definición
    div(
      class = "mb-3 mt-2",
      p(t$definicion, class = "lead", style = "color: #57606C;")
    ),

    # Características + Limitaciones
    layout_columns(
      col_widths = c(6, 6),
      fill       = FALSE,

      card(
        full_screen = FALSE,
        height      = NULL,
        card_header("Características"),
        tags$ul(
          lapply(t$caracteristicas, tags$li)
        )
      ),

      card(
        full_screen = FALSE,
        height      = NULL,
        card_header("Limitaciones"),
        tags$ul(
          lapply(t$limitaciones, tags$li)
        )
      )
    ),

    # ¿Cuándo usar?
    card(
      fill        = FALSE,
      full_screen = FALSE,
      card_header("¿Cuándo usar?"),
      p(t$cuando, class = "mb-0")
    ),

    # Aplicaciones frecuentes
    card(
      fill        = FALSE,
      full_screen = FALSE,
      card_header("Aplicaciones frecuentes en ciencias ambientales y recursos naturales"),
      div(
        lapply(seq_along(t$diseños), function(i) {
          div(
            class = "card-muestreo",
            strong(names(t$diseños)[i]),
            p(t$diseños[[i]], class = "mb-0 small text-muted")
          )
        })
      )
    ),

    # Estadísticos típicos
    card(
      fill        = FALSE,
      full_screen = FALSE,
      card_header("Estadísticos típicos"),
      p(t$estadisticas, class = "mb-0")
    ),

    # Ejemplo
    card(
      fill        = FALSE,
      full_screen = FALSE,
      card_header("Ejemplo ambiental"),
      div(
        class = "wiz-result",
        p(t$ejemplo, class = "mb-0 fst-italic")
      )
    )
  )
}

# ── UI ────────────────────────────────────────────────────
mod_tipos_ui <- function(id) {
  ns <- NS(id)

  tagList(
    navset_card_tab(
      id = ns("tabs_tipos"),

      nav_panel(
        title    = tagList(bsicons::bs_icon("clipboard-data"), " Descriptivo"),
        value    = "descriptivo",
        fillable = FALSE,
        tab_contenido(tipos_estudio$descriptivo)
      ),

      nav_panel(
        title    = tagList(bsicons::bs_icon("diagram-2"), " Observacional / Correlacional"),
        value    = "observacional",
        fillable = FALSE,
        tab_contenido(tipos_estudio$observacional)
      ),

      nav_panel(
        title    = tagList(bsicons::bs_icon("eyedropper"), " Experimental"),
        value    = "experimental",
        fillable = FALSE,
        tab_contenido(tipos_estudio$experimental)
      )
    )
  )
}

# ── Server ────────────────────────────────────────────────
# El contenido es estático (construido en UI desde tipos_estudio),
# por lo que el server no necesita renderUI reactivo.
mod_tipos_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Sin lógica reactiva necesaria — navset_card_tab maneja la navegación.
  })
}
