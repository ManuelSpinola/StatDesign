# ============================================================
# app.R — Punto de entrada de StatDesign
#
# Este archivo SOLO:
#   1. Carga librerías y helpers compartidos
#   2. Carga los módulos
#   3. Define ui y server ensamblando los módulos
#
# La lógica de cada pestaña vive en modules/mod_*.R
# Las funciones y contenidos compartidos viven en R/helpers.R
#
# Suite StatX:
#   StatFlow    — Primeros análisis y visualización
#   StatDesign  — Diseño de estudios y muestreo  ← esta app
#   StatModels  — Modelos avanzados (próximamente)
# ============================================================

# ── 1. Librerías y helpers ─────────────────────────────────
source("R/helpers.R")

# ── 2. Módulos ─────────────────────────────────────────────
source("modules/mod_tipos.R")
source("modules/mod_muestreo.R")
source("modules/mod_asistente.R")
source("modules/mod_ayuda.R")

# ── 3. UI ──────────────────────────────────────────────────
ui <- page_navbar(
  title = div(
    style = "display: flex; align-items: center; gap: 10px; margin-top: 4px;",
    img(
      src    = "hexsticker_StatDesign.png",
      height = "38px"
    ),
    span("StatDesign", style = "font-weight: 600;")
  ),
  theme  = tema_app,
  lang   = "es",
  footer = div(
    class = "text-center text-muted small py-2",
    style = paste0("border-top: 1px solid ", colores$borde, ";"),
    "Manuel Spínola · ICOMVIS · Universidad Nacional · Costa Rica"
  ),

  mod_tipos_ui("tipos"),
  mod_muestreo_ui("muestreo"),
  mod_asistente_ui("asistente"),
  mod_ayuda_ui("ayuda"),

  nav_spacer(),
  nav_item(tags$span(class = "text-muted small", "StatDesign v1.0"))
)

# ── 4. Server ──────────────────────────────────────────────
server <- function(input, output, session) {

  mod_tipos_server("tipos")
  mod_muestreo_server("muestreo")
  mod_asistente_server("asistente")
  mod_ayuda_server("ayuda")

}

# ── 5. Lanzar ──────────────────────────────────────────────
shinyApp(ui, server)
