# ============================================================
# mod_tipos.R — Módulo: Tipos de estudio
# Estructura: navset_card_tab (una pestaña por tipo)
# ============================================================

# ── Helpers internos ──────────────────────────────────────

# Construye el contenido de una pestaña dado un item de tipos_estudio
tab_contenido <- function(t) {

  # Paleta de tinte por tipo de estudio — combina con .card-descriptivo /
  # .card-observacional / .card-experimental en helpers.R, para que
  # definición y tiles compartan identidad visual.
  tinte <- switch(t$color_css,
    descriptivo   = list(bg = "#dceefa", borde = "#5FA2CE"),
    observacional = list(bg = "#ffe6ce", borde = "#FC7D0B"),
    experimental  = list(bg = "#d6e6f7", borde = "#1170AA")
  )

  tagList(

    # Definición — banner de color según el tipo de estudio
    div(
      class = paste0("card-tipo card-", t$color_css, " mb-3 mt-2"),
      p(t$definicion, class = "lead mb-0")
    ),

    # Características + Limitaciones
    bslib::layout_columns(
      col_widths = c(6, 6),
      fill       = FALSE,

      bslib::card(
        full_screen = FALSE,
        height      = NULL,
        bslib::card_header("Características"),
        tags$ul(
          lapply(t$caracteristicas, tags$li)
        )
      ),

      bslib::card(
        full_screen = FALSE,
        height      = NULL,
        bslib::card_header("Limitaciones"),
        tags$ul(
          lapply(t$limitaciones, tags$li)
        )
      )
    ),

    # ¿Cuándo usar? + Estadísticos típicos
    bslib::layout_columns(
      col_widths = c(6, 6),
      fill       = FALSE,

      bslib::card(
        fill        = FALSE,
        full_screen = FALSE,
        bslib::card_header("¿Cuándo usar?"),
        p(t$cuando, class = "mb-0")
      ),

      bslib::card(
        fill        = FALSE,
        full_screen = FALSE,
        bslib::card_header("Estadísticos típicos"),
        p(t$estadisticas, class = "mb-0")
      )
    ),

    # Aplicaciones frecuentes — grid de tarjetas (2 columnas), teñidas
    # con el color del tipo de estudio para reforzar la identidad visual
    bslib::card(
      fill        = FALSE,
      full_screen = FALSE,
      bslib::card_header("Aplicaciones frecuentes en ciencias ambientales y recursos naturales"),
      div(
        style = "display: grid; grid-template-columns: 1fr 1fr; gap: 10px;",
        lapply(seq_along(t$diseños), function(i) {
          div(
            style = paste0("border: 1px solid ", tinte$borde,
                            "; border-radius: 8px; padding: 0.9rem 1rem; background-color: ",
                            tinte$bg, ";"),
            strong(names(t$diseños)[i]),
            p(t$diseños[[i]], class = "mb-0 small text-muted")
          )
        })
      )
    ),

    # Ejemplo — cita destacada, sin card_header para que quede más compacta
    div(
      class = "wiz-result",
      p(class = "small text-uppercase fw-semibold mb-1",
        style = paste0("color:", colores$acento, ";"),
        "Ejemplo ambiental"),
      p(t$ejemplo, class = "mb-0 fst-italic")
    )
  )
}

# Esquema: árbol de decisión para elegir el tipo de estudio.
# Vive arriba de los sub-tabs, visible sin importar cuál esté seleccionado.
esquema_decision_tipos <- function() {
  div(
    class = "mb-4 p-3 rounded-3",
    style = paste0("background:", colores$fondo, "; border: 1px solid ", colores$borde, ";"),

    p(class = "small fw-semibold text-uppercase mb-3",
      style = paste0("color:", colores$texto, "; letter-spacing:.03em;"),
      bsicons::bs_icon("diagram-2", class = "me-1"),
      "¿Qué tipo de estudio necesito?"),

    div(
      class = "text-center mb-1",
      div(class = "d-inline-block px-3 py-2 rounded-3 bg-white border",
          strong("1. ¿Manipulás una variable de forma deliberada?"))
    ),
    div(class = "text-center small text-muted mb-3",
        "No, seguí al paso 2 ↓        Sí, mirá la columna derecha ↘"),

    bslib::layout_columns(
      col_widths = c(7, 5),
      fill       = FALSE,

      div(
        div(
          class = "text-center mb-1",
          div(class = "d-inline-block px-3 py-2 rounded-3 bg-white border",
              strong("2. ¿Buscás relación o asociación entre variables?"))
        ),
        div(class = "text-center small text-muted mb-2", "No ↓        Sí ↓"),
        bslib::layout_columns(
          col_widths = c(6, 6),
          div(class = "text-center px-3 py-3 rounded-3",
              style = "background:#dceefa; border:1px solid #5FA2CE;",
              strong("Descriptivo")),
          div(class = "text-center px-3 py-3 rounded-3",
              style = "background:#ffe6ce; border:1px solid #FC7D0B;",
              strong("Observacional / Correlacional"))
        )
      ),

      div(
        class = "d-flex align-items-center justify-content-center h-100",
        div(class = "text-center px-3 py-3 rounded-3 w-100",
            style = "background:#d6e6f7; border:1px solid #1170AA;",
            strong("Experimental"))
      )
    )
  )
}

# ── UI ────────────────────────────────────────────────────
mod_tipos_ui <- function(id) {
  ns <- NS(id)

  tagList(

    esquema_decision_tipos(),

    bslib::navset_card_tab(
      id = ns("tabs_tipos"),

      bslib::nav_panel(
        title    = tagList(bsicons::bs_icon("clipboard-data"), " Descriptivo"),
        value    = "descriptivo",
        fillable = FALSE,
        tab_contenido(tipos_estudio$descriptivo)
      ),

      bslib::nav_panel(
        title    = tagList(bsicons::bs_icon("diagram-2"), " Observacional / Correlacional"),
        value    = "observacional",
        fillable = FALSE,
        tab_contenido(tipos_estudio$observacional)
      ),

      bslib::nav_panel(
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
