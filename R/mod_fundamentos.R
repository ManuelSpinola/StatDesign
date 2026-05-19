# ============================================================
# mod_fundamentos.R — Módulo: Fundamentos del diseño
# Aleatorización · Replicación · Control · Pseudoreplicación
#
# StatSuite:
#   StatDesign  — Diseño de estudios y muestreo  ← esta app
#   StatFlow    — Primeros análisis y visualización
#   StatGeo     — Para trabajar con mapas (SIG)
#   StatModels  — Modelos avanzados (próximamente)
# ============================================================

# ── UI ────────────────────────────────────────────────────
mod_fundamentos_ui <- function(id) {
  ns <- NS(id)

  tagList(

    bslib::navset_card_tab(

      # ── Tab 1: Aleatorización ──────────────────────────
      bslib::nav_panel(
        title = tagList(bsicons::bs_icon("shuffle"), " Aleatorización"),
        bslib::card_body(
          bslib::layout_columns(
            col_widths = c(12),

            bslib::card(
              bslib::card_header(bsicons::bs_icon("shuffle"), " ¿Qué es la aleatorización?"),
              bslib::card_body(
                p("La aleatorización es el proceso de asignar las unidades
                  experimentales a los tratamientos —o de seleccionar las unidades
                  de muestreo de una población— mediante un mecanismo de azar."),
                p("Es el elemento que ", strong("distingue un experimento de un
                  estudio observacional"), ": en el experimento, el investigador
                  controla y aleatoriza la asignación de tratamientos."),
                div(
                  class = "wiz-result mb-3",
                  p(class = "small mb-0",
                    "La aleatorización protege contra la influencia sistemática de
                     variables no controladas o desconocidas. Si los grupos son
                     asignados al azar, cualquier factor externo afecta a ambos
                     grupos por igual en promedio.")
                )
              )
            ),

            bslib::layout_columns(
              col_widths = c(6, 6),

              bslib::card(
                bslib::card_header(bsicons::bs_icon("diagram-3"), " Dos tipos de aleatorización"),
                bslib::card_body(
                  tags$dl(
                    class = "small mb-0",
                    tags$dt("1. Selección aleatoria"),
                    tags$dd(class = "mb-2",
                            "Seleccionar al azar las unidades de muestreo de la
                       población. Permite generalizar los resultados a la
                       población objetivo."),
                    tags$dt("2. Asignación aleatoria"),
                    tags$dd(
                      "Asignar al azar los tratamientos a las unidades
                       experimentales. Permite inferencia causal al controlar
                       el efecto de factores no medidos.")
                  )
                )
              ),

              bslib::card(
                bslib::card_header(bsicons::bs_icon("journal-text"), " Ejemplo"),
                bslib::card_body(
                  div(
                    class = "wiz-result",
                    p(class = "small fst-italic mb-2",
                      "Se seleccionan 16 fragmentos de bosque al azar en una región
                       (selección aleatoria) y se asignan al azar 8 a tala selectiva
                       y 8 a control (asignación aleatoria)."),
                    p(class = "small mb-0",
                      "Gracias a la asignación aleatoria, cualquier diferencia en
                       composición del suelo, topografía u otros factores se distribuye
                       por igual entre los grupos.")
                  )
                )
              )
            )
          )
        )
      ),

      # ── Tab 2: Replicación ─────────────────────────────
      bslib::nav_panel(
        title = tagList(bsicons::bs_icon("copy"), " Replicación"),
        bslib::card_body(
          bslib::layout_columns(
            col_widths = c(12),

            bslib::card(
              bslib::card_header(bsicons::bs_icon("layers"), " ¿Qué es la replicación?"),
              bslib::card_body(
                p("La replicación es la asignación del mismo tratamiento —
                  o la aplicación del mismo protocolo de muestreo — a más de
                  una unidad experimental o de muestreo independiente."),
                p("Una réplica verdadera es una unidad independiente que recibe
                  el mismo tratamiento o condición. Las submuestras tomadas
                  dentro de una misma unidad ", strong("no"), " son réplicas
                  independientes."),
                div(
                  class = "wiz-result mb-3",
                  tags$dl(
                    class = "row small mb-0",
                    tags$dt(class = "col-sm-4", "Réplicas (muestras)"),
                    tags$dd(class = "col-sm-8",
                            "Unidades independientes que reciben el mismo tratamiento.
                       Aumentar réplicas incrementa la precisión y el poder
                       estadístico de la prueba."),
                    tags$dt(class = "col-sm-4", "Submuestras"),
                    tags$dd(class = "col-sm-8",
                            "Observaciones tomadas dentro de la misma unidad.
                       Son útiles para estimar la variación interna de la réplica,
                       pero no reemplazan a las réplicas verdaderas.")
                  )
                )
              )
            ),

            bslib::layout_columns(
              col_widths = c(6, 6),

              bslib::card(
                bslib::card_header(bsicons::bs_icon("check-circle"), " ¿Para qué sirve?"),
                bslib::card_body(
                  tags$ul(
                    class = "small",
                    tags$li("Permite estimar la variación natural del sistema."),
                    tags$li("Aumenta la precisión de los estimados de los parámetros."),
                    tags$li("Incrementa el poder estadístico de las pruebas."),
                    tags$li("Permite generalizar los resultados más allá de las
                             unidades muestreadas.")
                  )
                )
              ),

              bslib::card(
                bslib::card_header(bsicons::bs_icon("journal-text"), " Ejemplo"),
                bslib::card_body(
                  div(
                    class = "wiz-result",
                    p(class = "small fst-italic mb-2",
                      "Se quiere evaluar el efecto de la tala selectiva sobre la
                       densidad de salamandras. Se seleccionan 8 fragmentos de bosque
                       con tala y 8 sin tala (réplicas). Dentro de cada fragmento se
                       colocan 5 parcelas (submuestras)."),
                    p(class = "small mb-0",
                      strong("n = 8"), " (fragmentos), no 40. Las parcelas son
                       submuestras, no réplicas independientes.")
                  )
                )
              )
            )
          )
        )
      ),

      # ── Tab 3: Control ─────────────────────────────────
      bslib::nav_panel(
        title = tagList(bsicons::bs_icon("sliders"), " Control"),
        bslib::card_body(
          bslib::layout_columns(
            col_widths = c(12),

            bslib::card(
              bslib::card_header(bsicons::bs_icon("sliders"), " ¿Qué es el control experimental?"),
              bslib::card_body(
                p("En experimentación, los ", strong("controles"), " son
                  observaciones paralelas usadas para verificar los efectos de
                  los tratamientos experimentales. Son unidades que no reciben
                  el tratamiento de interés pero se manejan de manera idéntica
                  en todo lo demás."),
                p("El control permite separar el efecto del tratamiento del efecto
                  de otros factores que podrían influir en la variable respuesta.
                  Sin un grupo control, no es posible atribuir causalmente los
                  cambios observados al tratamiento."),
                div(
                  class = "wiz-result mb-3",
                  p(class = "small mb-2",
                    strong("Control también puede entenderse como bloqueo o
                    estratificación: "), "agrupar las unidades experimentales
                    en bloques homogéneos para reducir la variación no controlada
                    y aumentar la precisión.")
                )
              )
            ),

            bslib::layout_columns(
              col_widths = c(6, 6),

              bslib::card(
                bslib::card_header(bsicons::bs_icon("check-circle"), " ¿Para qué sirve?"),
                bslib::card_body(
                  tags$ul(
                    class = "small",
                    tags$li("Elimina el efecto de factores ajenos al tratamiento."),
                    tags$li("Permite establecer una línea base para comparar."),
                    tags$li("Aumenta la validez interna del estudio."),
                    tags$li("Es indispensable para inferencia causal.")
                  )
                )
              ),

              bslib::card(
                bslib::card_header(bsicons::bs_icon("journal-text"), " Ejemplo"),
                bslib::card_body(
                  div(
                    class = "wiz-result",
                    p(class = "small fst-italic mb-2",
                      "Efecto de la exclusión de venados sobre la regeneración del
                       bosque: parcelas con cercado (tratamiento) vs. parcelas sin
                       cercado (control). Ambos grupos se miden de manera idéntica
                       y al mismo tiempo."),
                    p(class = "small mb-0",
                      "Sin el grupo control, no podríamos saber si los cambios en
                       regeneración se deben al cercado o a la lluvia, la estación,
                       u otro factor.")
                  )
                )
              )
            )
          )
        )
      ),

      # ── Tab 4: Pseudoreplicación ───────────────────────
      bslib::nav_panel(
        title = tagList(bsicons::bs_icon("exclamation-triangle"), " Pseudoreplicación"),
        bslib::card_body(
          bslib::layout_columns(
            col_widths = c(12),

            bslib::card(
              bslib::card_header(bsicons::bs_icon("exclamation-triangle"), " ¿Qué es la pseudoreplicación?"),
              bslib::card_body(
                div(
                  class = "wiz-result mb-3",
                  p(class = "small mb-0",
                    tags$em(
                      "\"La pseudoreplicación es el uso de la estadística inferencial
                       para evaluar el efecto de tratamientos con datos de experimentos
                       donde los tratamientos no son replicados, o las réplicas no son
                       estadísticamente independientes.\""),
                    br(),
                    span(class = "text-muted", "— Hurlbert, S.H. (1984). Ecological Monographs, 54, 187–211.")
                  )
                ),
                p("La pseudoreplicación ocurre cuando se tratan submuestras —
                  observaciones no independientes tomadas dentro de la misma unidad
                  experimental — como si fueran réplicas verdaderas. Esto infla
                  artificialmente el tamaño de muestra y viola el supuesto de
                  independencia de la mayoría de los análisis estadísticos.")
              )
            ),

            # Tres tipos
            bslib::layout_columns(
              col_widths = c(4, 4, 4),

              bslib::card(
                bslib::card_header(
                  class = "bg-light",
                  bsicons::bs_icon("geo-alt"), " Simple"
                ),
                bslib::card_body(
                  p(class = "small",
                    "Ocurre cuando se toman múltiples submuestras de una sola
                     unidad experimental y se analizan como réplicas independientes.
                     Es el tipo más común en ecología."),
                  hr(class = "my-2"),
                  div(
                    class = "wiz-result",
                    p(class = "small fst-italic mb-1", strong("Ejemplo:")),
                    p(class = "small mb-0",
                      "Se quiere comparar la abundancia de mariposas en bosque vs.
                       potrero. Se selecciona un fragmento de bosque y un potrero,
                       y en cada sitio se colocan 10 trampas. Si se analizan las
                       20 trampas como réplicas independientes, hay pseudoreplicación
                       simple: las trampas son submuestras del mismo sitio. La réplica
                       es el fragmento o el potrero. ",
                      strong("n = 1 por tratamiento"), ", no 10.")
                  )
                )
              ),

              bslib::card(
                bslib::card_header(
                  class = "bg-light",
                  bsicons::bs_icon("archive"), " De sacrificio"
                ),
                bslib::card_body(
                  p(class = "small",
                    "Existen réplicas verdaderas pero los datos se colapsan antes
                     del análisis, o las submuestras dentro de cada réplica se
                     tratan como independientes. La información sobre la varianza
                     entre réplicas se pierde o se confunde."),
                  hr(class = "my-2"),
                  div(
                    class = "wiz-result",
                    p(class = "small fst-italic mb-1", strong("Ejemplo:")),
                    p(class = "small mb-0",
                      "Mismo estudio de mariposas, pero ahora se seleccionan 4
                       fragmentos de bosque y 4 potreros, con 10 trampas en cada
                       uno. Si se analizan las 80 trampas como réplicas en lugar
                       de los 8 sitios, se comete pseudoreplicación de sacrificio:
                       las réplicas verdaderas existen, pero el análisis se hace
                       al nivel equivocado. ",
                      strong("n = 4 por tratamiento"), ", no 40.")
                  )
                )
              ),

              bslib::card(
                bslib::card_header(
                  class = "bg-light",
                  bsicons::bs_icon("clock-history"), " Temporal"
                ),
                bslib::card_body(
                  p(class = "small",
                    "Se toman medidas repetidas en el tiempo sobre las mismas
                     unidades y se tratan como observaciones independientes.
                     Las medidas sucesivas de una misma unidad están correlacionadas
                     (autocorrelación), lo que invalida los análisis estándar."),
                  hr(class = "my-2"),
                  div(
                    class = "wiz-result",
                    p(class = "small fst-italic mb-1", strong("Ejemplo:")),
                    p(class = "small mb-0",
                      "Se monitorea la calidad del agua en un mismo punto de un
                       río durante 12 meses y se tratan las mediciones mensuales
                       como independientes. Para evitarlo, se debe usar análisis
                       de series de tiempo o modelos de medidas repetidas.")
                  )
                )
              )
            ),

            bslib::card(
              bslib::card_header(bsicons::bs_icon("lightbulb"), " ¿Cómo evitarla?"),
              bslib::card_body(
                bslib::layout_columns(
                  col_widths = c(6, 6),
                  div(
                    tags$ol(
                      class = "small mb-0",
                      tags$li(
                        strong("Identificar la unidad experimental correcta: "),
                        "la réplica es la unidad a la que se asigna el tratamiento,
                         no la submuestra dentro de ella."
                      ),
                      tags$li(
                        strong("Usar el nivel correcto de análisis: "),
                        "si hay submuestras, promediarlas por réplica antes del
                         análisis, o usar modelos mixtos que las incorporen
                         correctamente."
                      ),
                      tags$li(
                        strong("Para medidas repetidas: "),
                        "usar análisis de medidas repetidas, modelos mixtos
                         longitudinales o series de tiempo según el caso."
                      )
                    )
                  ),
                  div(
                    class = "wiz-result",
                    p(class = "small mb-1",
                      bsicons::bs_icon("exclamation-circle"), " ",
                      strong("Regla práctica:")),
                    p(class = "small mb-0",
                      "Siempre preguntarse: ¿a qué nivel se asignó el tratamiento?
                       Ese es el nivel de la réplica verdadera, y ese es el
                       denominador correcto del análisis estadístico.")
                  )
                )
              )
            )

          )
        )
      )
    )
  )
}

# ── Server ────────────────────────────────────────────────
mod_fundamentos_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Sin lógica reactiva por ahora
  })
}
