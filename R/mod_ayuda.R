# ============================================================
# mod_ayuda.R — Módulo: Ayuda y referencias
# ============================================================

mod_ayuda_ui <- function(id) {
  ns <- NS(id)

  tagList(

    div(
      class = "p-3",

      layout_columns(
        col_widths = c(6, 6),

        # ── Cómo usar StatDesign ──────────────────────
        card(
          card_header("¿Cómo usar StatDesign?"),
          tags$ol(
            tags$li(strong("Tipos de estudio:"),
                    " Explora las tres categorías principales — descriptivo,
                    observacional/correlacional y experimental — con ejemplos
                    del área ambiental y de recursos naturales."),
            tags$li(strong("Diseños de muestreo:"),
                    " Revisa los diseños probabilísticos, no probabilísticos y
                    espaciales. Usa la calculadora para estimar el tamaño de
                    muestra de tu estudio."),
            tags$li(strong("Asistente de diseño:"),
                    " Responde tres preguntas y recibe una recomendación sobre
                    el tipo de estudio más adecuado para tu pregunta de
                    investigación.")
          )
        ),

        # ── StatSuite ───────────────────────────────
        card(
          card_header("StatSuite"),
          p("StatDesign forma parte de ",
            tags$a("StatSuite", href = "https://statsuite.netlify.app", target = "_blank"),
            ", una colección de aplicaciones estadísticas para el análisis y visualización de datos."),
          p(class = "text-muted small mt-2",
            "Todas las apps comparten la misma paleta de colores, fuente y
     convenciones, para una experiencia coherente.")
        ),

        # ── Conceptos clave ───────────────────────────
        card(
          card_header("Conceptos clave"),
          tags$dl(
            tags$dt("Variable independiente"),
            tags$dd("Variable que el investigador manipula o que se considera
                     como causa. Ejemplo: tratamiento de revegetación."),
            tags$dt("Variable dependiente"),
            tags$dd("Variable que se mide como respuesta. Ejemplo: tasa de
                     erosión del suelo."),
            tags$dt("Variable de confusión"),
            tags$dd("Variable no controlada que puede afectar la relación
                     observada entre las variables de interés."),
            tags$dt("Validez interna"),
            tags$dd("Grado en que el diseño controla factores que podrían
                     explicar el resultado además de la variable de interés."),
            tags$dt("Validez externa"),
            tags$dd("Grado en que los resultados pueden generalizarse a
                     otras poblaciones, lugares o momentos.")
          )
        ),

        # ── Referencias ───────────────────────────────
        card(
          card_header("Referencias"),
          tags$ul(
            tags$li("Quinn, G. P. & Keough, M. J. (2002).",
                    em("Experimental Design and Data Analysis for Biologists."),
                    " Cambridge University Press."),
            tags$li("Gotelli, N. J. & Ellison, A. M. (2004).",
                    em("A Primer of Ecological Statistics."),
                    " Sinauer Associates."),
            tags$li("Zar, J. H. (2010).",
                    em("Biostatistical Analysis (5th ed.)."),
                    " Prentice Hall."),
            tags$li("Cochran, W. G. (1977).",
                    em("Sampling Techniques (3rd ed.)."),
                    " John Wiley & Sons.")
          )
        )
      ),

      # Pie del módulo
      hr(),
      div(
        class = "text-center text-muted small",
        "StatDesign v1.0 · Manuel Spínola · ICOMVIS · Universidad Nacional · Costa Rica"
      )
    )
  )
}

mod_ayuda_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Sin lógica reactiva por ahora
  })
}
