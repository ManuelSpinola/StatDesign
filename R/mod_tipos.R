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

# Esquema: árbol de decisión para elegir el tipo de estudio, como SVG
# embebido (geometría ya verificada visualmente; colores en hex literal
# para no depender de ninguna variable CSS externa a la app).
esquema_decision_tipos <- function() {
  div(
    class = "mb-2 p-3 rounded-3",
    style = paste0("background:", colores$fondo, "; border: 1px solid ", colores$borde, ";"),

    HTML('
<svg viewBox="0 0 700 380" xmlns="http://www.w3.org/2000/svg" role="img" style="width:100%; height:auto; max-width:700px; display:block; margin:0 auto;">
<title>Árbol de decisión para elegir tipo de estudio</title>
<desc>Diagrama que ayuda a elegir entre estudio descriptivo, observacional o experimental según si se manipula una variable y si se busca relación entre variables.</desc>
<defs>
<marker id="arrowTipos" viewBox="0 0 10 10" refX="8" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse">
<path d="M0,0 L10,5 L0,10 z" fill="#8A8F98"></path>
</marker>
</defs>

<rect x="260" y="16" width="220" height="52" rx="10" fill="#ECEFF3" stroke="#B7C2CE"></rect>
<text x="370" y="38" text-anchor="middle" font-size="14" font-weight="600" fill="#33383D">¿Manipulás una variable</text>
<text x="370" y="56" text-anchor="middle" font-size="14" font-weight="600" fill="#33383D">de forma deliberada?</text>

<line x1="370" y1="68" x2="370" y2="100" stroke="#8A8F98" stroke-width="1.5" marker-end="url(#arrowTipos)"></line>
<line x1="370" y1="68" x2="530" y2="100" stroke="#8A8F98" stroke-width="1.5" marker-end="url(#arrowTipos)"></line>

<text x="345" y="90" text-anchor="middle" font-size="13" fill="#57606C">No</text>
<text x="500" y="90" text-anchor="middle" font-size="13" fill="#57606C">Sí</text>

<rect x="150" y="100" width="240" height="52" rx="10" fill="#ECEFF3" stroke="#B7C2CE"></rect>
<text x="270" y="122" text-anchor="middle" font-size="14" font-weight="600" fill="#33383D">¿Buscás relación o</text>
<text x="270" y="140" text-anchor="middle" font-size="14" font-weight="600" fill="#33383D">asociación entre variables?</text>

<rect x="500" y="100" width="180" height="52" rx="10" fill="#d6e6f7" stroke="#1170AA"></rect>
<text x="590" y="126" text-anchor="middle" font-size="14" font-weight="600" fill="#0c3a5c">Manipulación +</text>
<text x="590" y="144" text-anchor="middle" font-size="13" fill="#0c3a5c">control/tratamiento</text>

<line x1="230" y1="152" x2="150" y2="196" stroke="#8A8F98" stroke-width="1.5" marker-end="url(#arrowTipos)"></line>
<line x1="310" y1="152" x2="350" y2="196" stroke="#8A8F98" stroke-width="1.5" marker-end="url(#arrowTipos)"></line>

<text x="165" y="180" text-anchor="middle" font-size="13" fill="#57606C">No</text>
<text x="345" y="180" text-anchor="middle" font-size="13" fill="#57606C">Sí</text>

<line x1="590" y1="152" x2="590" y2="196" stroke="#8A8F98" stroke-width="1.5" marker-end="url(#arrowTipos)"></line>

<rect x="20" y="200" width="180" height="90" rx="10" fill="#dceefa" stroke="#5FA2CE"></rect>
<text x="110" y="234" text-anchor="middle" font-size="15" font-weight="600" fill="#2b5a75">Descriptivo</text>
<text x="110" y="254" text-anchor="middle" font-size="12" fill="#2b5a75">Caracteriza el</text>
<text x="110" y="270" text-anchor="middle" font-size="12" fill="#2b5a75">fenómeno tal cual es</text>

<rect x="250" y="200" width="200" height="90" rx="10" fill="#ffe6ce" stroke="#FC7D0B"></rect>
<text x="350" y="234" text-anchor="middle" font-size="15" font-weight="600" fill="#8a4611">Observacional /</text>
<text x="350" y="252" text-anchor="middle" font-size="15" font-weight="600" fill="#8a4611">Correlacional</text>
<text x="350" y="272" text-anchor="middle" font-size="12" fill="#8a4611">Mide relación sin intervenir</text>

<rect x="500" y="200" width="180" height="90" rx="10" fill="#d6e6f7" stroke="#1170AA"></rect>
<text x="590" y="234" text-anchor="middle" font-size="15" font-weight="600" fill="#0c3a5c">Experimental</text>
<text x="590" y="254" text-anchor="middle" font-size="12" fill="#0c3a5c">Permite inferencia</text>
<text x="590" y="270" text-anchor="middle" font-size="12" fill="#0c3a5c">causal con rigor</text>

<text x="350" y="336" text-anchor="middle" font-size="12" fill="#8A8F98">El grado de control del investigador aumenta de izquierda a derecha</text>
<line x1="20" y1="356" x2="680" y2="356" stroke="#C8D9EC" stroke-width="1.5" marker-end="url(#arrowTipos)"></line>
</svg>
')
  )
}

# ── UI ────────────────────────────────────────────────────
mod_tipos_ui <- function(id) {
  ns <- NS(id)

  tagList(

    bslib::navset_card_tab(
      id = ns("tabs_tipos"),

      bslib::nav_panel(
        title    = tagList(bsicons::bs_icon("signpost-2"), " ¿Qué tipo de estudio necesito?"),
        value    = "decision",
        fillable = FALSE,
        esquema_decision_tipos()
      ),

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
