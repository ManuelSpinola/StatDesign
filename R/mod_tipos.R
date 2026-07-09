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

# ── UI ────────────────────────────────────────────────────
mod_tipos_ui <- function(id) {
  ns <- NS(id)

  tagList(
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
