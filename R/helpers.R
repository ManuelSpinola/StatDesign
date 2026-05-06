# ============================================================
# helpers.R — Funciones y objetos compartidos entre módulos
# StatDesign — Diseño de estudios y muestreo
# Paleta: Tableau Color Blind (coherente con StatFlow)
# ============================================================

library(shiny)
library(bslib)
library(tidyverse)
library(DT)

# ── Paleta de colores (idéntica a StatFlow) ────────────────
colores <- list(
  fondo       = "#F4F7FB",
  primario    = "#1170AA",
  acento      = "#FC7D0B",
  secundario  = "#5FA2CE",
  texto       = "#57606C",
  exito       = "#5FA2CE",
  advertencia = "#F1CE63",
  peligro     = "#C85200",
  borde       = "#C8D9EC",

  tableau = c(
    "#1170AA", "#FC7D0B", "#A3ACB9", "#57606C",
    "#C85200", "#7BC8ED", "#5FA2CE", "#F1CE63",
    "#9F8B75", "#B85A0D"
  )
)

# ── Tema visual (idéntico a StatFlow) ─────────────────────
tema_app <- bs_theme(
  version      = 5,
  bg           = colores$fondo,
  fg           = colores$texto,
  primary      = colores$primario,
  secondary    = colores$secundario,
  success      = colores$exito,
  danger       = colores$peligro,
  warning      = colores$advertencia,
  base_font    = font_google("Nunito"),
  heading_font = font_google("Nunito", wght = 700),
  bootswatch   = NULL
) |>
  bs_add_rules("
  .navbar { background-color: #1170AA !important; }
  .navbar-nav .nav-link { color: #ffffff !important; }
  .navbar .navbar-brand { color: #ffffff !important; }
  .nav-link.active { border-bottom: 2px solid #FC7D0B; }
  .btn-primary { background-color: #FC7D0B; border-color: #FC7D0B; color: #ffffff; }
  .btn-primary:hover { background-color: #d4680a; border-color: #d4680a; }
  .navbar-brand { display: flex !important; align-items: center !important;
                  padding-top: 0 !important; padding-bottom: 0 !important; }

  /* Tabs internos (navset_card_tab) */
  .tab-content .nav-link, .navset-card-tab .nav-link { color: #57606C !important; }
  .tab-content .nav-link.active, .navset-card-tab .nav-link.active { color: #1170AA !important; }

  /* Tarjetas de tipo de estudio */
  .card-tipo { border-left: 4px solid; border-radius: 8px; padding: 1rem; margin-bottom: 0.75rem; }
  .card-descriptivo  { border-color: #5FA2CE; background-color: #f0f7fc; }
  .card-observacional{ border-color: #FC7D0B; background-color: #fff8f2; }
  .card-experimental { border-color: #1170AA; background-color: #f0f4fa; }

  /* Tarjetas de diseño de muestreo */
  .card-muestreo { border: 1px solid #C8D9EC; border-radius: 8px;
                   padding: 0.9rem 1rem; margin-bottom: 0.6rem;
                   background-color: #ffffff; }
  .badge-prob     { background-color: #1170AA; color: #fff; }
  .badge-noprob   { background-color: #FC7D0B; color: #fff; }
  .badge-espacial { background-color: #5FA2CE; color: #fff; }

  /* Asistente */
  .wiz-opt { border: 1px solid #C8D9EC; border-radius: 8px; padding: 0.75rem 1rem;
             margin-bottom: 0.5rem; cursor: pointer; background: #ffffff;
             transition: border-color 0.15s; }
  .wiz-opt:hover { border-color: #1170AA; }
  .wiz-result { border-left: 4px solid #FC7D0B; background: #fff8f2;
                border-radius: 8px; padding: 1rem 1.25rem; }
")

# ── Escala de color para gráficos ─────────────────────────
scale_fill_tableau_cb <- function(...) {
  scale_fill_manual(values = colores$tableau, ...)
}
scale_color_tableau_cb <- function(...) {
  scale_color_manual(values = colores$tableau, ...)
}

# ── Contenido: tipos de estudio ───────────────────────────
tipos_estudio <- list(

  descriptivo = list(
    nombre     = "Descriptivo",
    icono      = "📋",
    color_css  = "descriptivo",
    definicion = "Caracteriza un fenómeno tal como se presenta en la naturaleza, sin manipular variables ni establecer relaciones causales. Responde: ¿Qué está ocurriendo y cómo se distribuye?",
    caracteristicas = c(
      "Sin manipulación de variables",
      "Observación en contexto natural",
      "Base para estudios posteriores",
      "Puede ser cuantitativo o cualitativo"
    ),
    limitaciones = c(
      "No permite inferir causalidad",
      "No controla variables externas",
      "Sesgo del observador posible",
      "Baja validez interna"
    ),
    diseños = c(
      "Inventario biológico" = "Registro sistemático de especies, abundancias o cobertura en un área definida.",
      "Monitoreo ambiental"  = "Seguimiento temporal de variables como calidad del agua, temperatura o biodiversidad.",
      "Encuesta / censo"     = "Recolección estructurada sobre uso de recursos naturales o percepción ambiental.",
      "Estudio de caso"      = "Descripción detallada de un ecosistema, especie o evento ambiental específico."
    ),
    ejemplo      = "Inventario de especies de aves en un humedal costero: se registran especies, abundancia relativa y gremios tróficos durante la época seca y lluviosa, sin manipular ninguna variable del sistema.",
    cuando       = "Cuando el objetivo es caracterizar un sistema, una población o un fenómeno antes de formular hipótesis causales. Ideal como punto de partida de cualquier línea de investigación.",
    estadisticas = "Estadísticas descriptivas, frecuencias, proporciones, índices de diversidad (Shannon, Simpson)."
  ),

  observacional = list(
    nombre     = "Observacional / Correlacional",
    icono      = "🔗",
    color_css  = "observacional",
    definicion = "Analiza relaciones o asociaciones entre variables sin manipularlas. El investigador mide lo que ocurre de forma natural. Responde: ¿Existe relación entre X e Y, y en qué dirección?",
    caracteristicas = c(
      "Sin intervención ni manipulación",
      "Mide grado y dirección de la asociación",
      "Puede ser prospectivo o retrospectivo",
      "Usa correlación, regresión, SEM"
    ),
    limitaciones = c(
      "Correlación no implica causalidad",
      "Variables de confusión no controladas",
      "Dirección causal incierta",
      "Sesgo de selección posible"
    ),
    diseños = c(
      "Estudio de gradiente" = "Compara sitios a lo largo de un gradiente natural: altitudinal, de perturbación o de contaminación.",
      "Correlacional puro"   = "Calcula coeficiente de correlación entre variables continuas medidas simultáneamente.",
      "Regresión múltiple"   = "Modela la relación entre una variable respuesta y varias predictoras observadas en campo.",
      "Seguimiento temporal" = "Monitoreo de poblaciones o ecosistemas a lo largo del tiempo para detectar tendencias."
    ),
    ejemplo      = "Relación entre porcentaje de cobertura boscosa en cuencas hidrográficas y concentración de sedimentos en el agua: se miden ambas variables en 30 cuencas de Costa Rica sin intervenir en ninguna.",
    cuando       = "Cuando no es ético o factible experimentar, o cuando se quieren explorar patrones naturales y relaciones entre variables antes de diseñar un experimento.",
    estadisticas = "Correlación de Pearson o Spearman, regresión lineal/múltiple, modelos lineales generalizados (GLM)."
  ),

  experimental = list(
    nombre     = "Experimental",
    icono      = "🧪",
    color_css  = "experimental",
    definicion = "El investigador manipula deliberadamente una o más variables para observar su efecto. Son los únicos que permiten establecer relaciones causales con rigor. Responden: ¿X produce Y bajo condiciones controladas?",
    caracteristicas = c(
      "Manipulación de la variable independiente",
      "Grupo control y grupo tratamiento",
      "Aleatorización (en diseños puros)",
      "Permite inferencia causal"
    ),
    limitaciones = c(
      "Validez externa limitada en laboratorio",
      "Mayor costo y complejidad logística",
      "Restricciones éticas o legales",
      "Escala espacial reducida en campo"
    ),
    diseños = c(
      "Experimento de campo"        = "Manipulación en condiciones naturales: parcelas de tratamiento y control en el ecosistema real.",
      "Experimento de laboratorio"  = "Mayor control de variables, menor validez externa. Útil para mecanismos específicos.",
      "Cuasiexperimental"           = "Hay intervención pero sin aleatorización completa. Común en restauración y manejo.",
      "BACI (antes-después / control-impacto)" = "Compara sitios impactados y de referencia antes y después de una perturbación o intervención."
    ),
    ejemplo      = "Efecto de tres tratamientos de revegetación (pastos nativos, arbustos pioneros y combinación) sobre la tasa de erosión del suelo en laderas degradadas: parcelas asignadas aleatoriamente, con medición de pérdida de suelo a 6 y 12 meses.",
    cuando       = "Cuando se necesita establecer causalidad con rigor y es posible manipular la variable de interés bajo condiciones controladas o semicontroladas.",
    estadisticas = "Modelos mixtos (LMM, GLMM), ANOVA, ANCOVA, pruebas t, modelos de efectos fijos y aleatorios."
  )
)

# ── Contenido: diseños de muestreo ────────────────────────
diseños_muestreo <- list(

  probabilisticos = list(
    titulo = "Probabilísticos — permiten inferencia estadística",
    badge  = "prob",
    items  = list(
      list(
        nombre     = "Aleatorio simple",
        desc       = "Cada unidad tiene igual probabilidad de ser seleccionada.",
        cuando     = "Población homogénea y con marco muestral definido.",
        ventaja    = "Sencillo, libre de sesgo de selección.",
        limitacion = "Ineficiente si hay mucha heterogeneidad espacial.",
        ejemplo    = "Seleccionar al azar 20 parcelas de 1 ha en un bosque secundario para estimar biomasa."
      ),
      list(
        nombre     = "Aleatorio estratificado",
        desc       = "La población se divide en estratos homogéneos; se muestrea al azar dentro de cada estrato.",
        cuando     = "Cuando hay subgrupos o zonas con características distintas.",
        ventaja    = "Mayor precisión con menor tamaño de muestra total.",
        limitacion = "Requiere conocer los estratos de antemano.",
        ejemplo    = "Muestrear calidad del agua en una cuenca dividida en zonas alta, media y baja."
      ),
      list(
        nombre     = "Sistemático",
        desc       = "Las unidades se seleccionan a intervalos regulares a partir de un punto de inicio aleatorio.",
        cuando     = "Cuando el área o la lista es extensa y homogénea.",
        ventaja    = "Fácil de implementar en campo, buena cobertura espacial.",
        limitacion = "Riesgo de coincidir con patrones periódicos del ambiente.",
        ejemplo    = "Colocar trampas cada 50 metros a lo largo de un transecto para monitorear mamíferos."
      ),
      list(
        nombre     = "Por conglomerados",
        desc       = "Se seleccionan grupos al azar y se muestrea todo o parte del grupo.",
        cuando     = "Área muy extensa, unidades naturalmente agrupadas.",
        ventaja    = "Reduce costos logísticos de desplazamiento.",
        limitacion = "Mayor error de muestreo si los conglomerados son heterogéneos.",
        ejemplo    = "Seleccionar 5 subcuencas al azar y muestrear todos los puntos de agua dentro de cada una."
      )
    )
  ),

  no_probabilisticos = list(
    titulo = "No probabilísticos — exploratorios o con restricciones de acceso",
    badge  = "noprob",
    items  = list(
      list(
        nombre     = "Por conveniencia",
        desc       = "Se seleccionan unidades de fácil acceso o disponibles al momento del estudio.",
        cuando     = "Estudios exploratorios, recursos limitados, zonas de difícil acceso.",
        ventaja    = "Rápido y económico.",
        limitacion = "No permite generalizar, alto riesgo de sesgo.",
        ejemplo    = "Muestrear suelos únicamente en zonas accesibles por caminos en un área montañosa."
      ),
      list(
        nombre     = "Intencional o por juicio",
        desc       = "El investigador selecciona unidades basándose en su conocimiento experto del sistema.",
        cuando     = "Cuando se buscan sitios representativos o casos extremos específicos.",
        ventaja    = "Permite enfocar recursos en sitios de mayor interés científico.",
        limitacion = "Introduce sesgo del investigador, no es fácilmente replicable.",
        ejemplo    = "Seleccionar sitios de referencia (poco perturbados) para comparar con sitios degradados en restauración."
      )
    )
  ),

  espaciales = list(
    titulo = "Diseños espaciales — propios de ecología y ciencias ambientales",
    badge  = "espacial",
    items  = list(
      list(
        nombre     = "Transectos",
        desc       = "Líneas de muestreo a lo largo de las cuales se registran organismos o variables.",
        cuando     = "Vegetación, aves, mamíferos, cobertura de suelo.",
        ventaja    = "Cubre gradientes espaciales, eficiente en terreno.",
        limitacion = "Puede sesgar si los transectos siguen rutas preferibles (senderos).",
        ejemplo    = "Transectos de 500 m para estimar densidad de palmas en un bosque tropical húmedo."
      ),
      list(
        nombre     = "Parcelas",
        desc       = "Áreas delimitadas de tamaño fijo donde se registran todos los individuos u objetos de interés.",
        cuando     = "Vegetación, suelos, cobertura, macroinvertebrados bentónicos.",
        ventaja    = "Alta precisión dentro del área muestreada.",
        limitacion = "El tamaño de parcela debe ajustarse al organismo estudiado.",
        ejemplo    = "Parcelas de 20×20 m para caracterizar estructura y composición de un bosque secundario."
      ),
      list(
        nombre     = "Muestreo en puntos",
        desc       = "Se registran organismos o variables desde un punto fijo en un radio o tiempo determinado.",
        cuando     = "Muy usado en aves, anfibios y vegetación arbórea.",
        ventaja    = "Eficiente para especies detectables a distancia (visual o auditiva).",
        limitacion = "Detectabilidad variable entre especies y observadores.",
        ejemplo    = "Puntos de conteo de 10 minutos para estimar riqueza de aves en fragmentos de bosque ripario."
      ),
      list(
        nombre     = "Muestreo adaptativo",
        desc       = "La intensidad de muestreo aumenta en zonas donde se detectan altas densidades del objeto de estudio.",
        cuando     = "Especies raras, agregadas o de distribución desconocida.",
        ventaja    = "Mayor probabilidad de detectar y cuantificar especies poco comunes.",
        limitacion = "Análisis estadístico más complejo.",
        ejemplo    = "Muestreo de orquídeas en bosque nuboso: al detectar un individuo, se intensifica el muestreo en celdas vecinas."
      )
    )
  )
)
