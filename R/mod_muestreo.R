# ============================================================
# mod_muestreo.R — Módulo: Diseños de muestreo
# Probabilísticos · No probabilísticos · Espaciales
# + Calculadora de tamaño de muestra
# + Esquema visual interactivo
# ============================================================

# ── Función: esquema espacial ──────────────────────────────
esquema_ggplot <- function(tipo, colores) {
  set.seed(123)

  base <- ggplot() +
    coord_fixed(xlim = c(0, 10), ylim = c(0, 10)) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid      = element_blank(),
      axis.text       = element_blank(),
      axis.title      = element_blank(),
      axis.ticks      = element_blank(),
      panel.border    = element_rect(color = colores$borde, fill = NA, linewidth = 0.8),
      plot.background = element_rect(fill = "#FFFFFF", color = NA),
      legend.position = "none",
      plot.title      = element_text(size = 10, color = colores$texto)
    )

  if (tipo == "alea_simple") {
    pts <- data.frame(x = runif(20, 0.5, 9.5), y = runif(20, 0.5, 9.5))
    base +
      geom_point(data = pts, aes(x, y),
                 color = colores$primario, size = 3.5, alpha = 0.85) +
      labs(title = "Aleatorio simple — 20 unidades seleccionadas al azar")

  } else if (tipo == "alea_estrat") {
    pts <- data.frame(
      x       = c(runif(7, 0.3, 3.3), runif(7, 3.7, 6.7), runif(7, 7.0, 9.7)),
      y       = runif(21, 0.5, 9.5),
      estrato = rep(c("Estrato A", "Estrato B", "Estrato C"), each = 7)
    )
    base +
      annotate("rect", xmin = 0,   xmax = 3.5, ymin = 0, ymax = 10,
               fill = colores$primario,   alpha = 0.08) +
      annotate("rect", xmin = 3.5, xmax = 6.8, ymin = 0, ymax = 10,
               fill = colores$acento,     alpha = 0.08) +
      annotate("rect", xmin = 6.8, xmax = 10,  ymin = 0, ymax = 10,
               fill = colores$secundario, alpha = 0.08) +
      geom_vline(xintercept = c(3.5, 6.8), linetype = "dashed",
                 color = colores$borde, linewidth = 0.7) +
      geom_point(data = pts, aes(x, y, color = estrato), size = 3.5, alpha = 0.9) +
      scale_color_manual(values = c(colores$primario, colores$acento, colores$secundario)) +
      annotate("text", x = c(1.75, 5.15, 8.4), y = 9.5,
               label = c("A", "B", "C"), size = 4, fontface = "bold",
               color = colores$texto) +
      labs(title = "Aleatorio estratificado — 3 estratos, 7 unidades c/u")

  } else if (tipo == "sistematico") {
    xs  <- seq(1.2, 9.2, by = 2)
    ys  <- seq(1.2, 9.2, by = 2)
    pts <- expand.grid(x = xs, y = ys)
    base +
      geom_hline(yintercept = ys, linetype = "dotted",
                 color = colores$borde, linewidth = 0.4) +
      geom_vline(xintercept = xs, linetype = "dotted",
                 color = colores$borde, linewidth = 0.4) +
      geom_point(data = pts, aes(x, y),
                 color = colores$primario, size = 3.5, shape = 15, alpha = 0.85) +
      labs(title = "Sistemático — grilla regular con inicio aleatorio")

  } else if (tipo == "conglomerados") {
    centros <- data.frame(cx = c(2, 6, 8), cy = c(7, 3, 7.5))
    pts <- do.call(rbind, lapply(1:3, function(i) {
      data.frame(
        x    = centros$cx[i] + rnorm(6, 0, 0.6),
        y    = centros$cy[i] + rnorm(6, 0, 0.6),
        cong = paste("Conglomerado", i)
      )
    }))
    no_sel <- data.frame(
      x = c(4.5, 4.5, 4.8, 1.5, 1.8, 1.2),
      y = c(1.5, 1.8, 1.2, 3.5, 3.2, 3.8)
    )
    base +
      geom_point(data = no_sel, aes(x, y),
                 color = "#A3ACB9", size = 2.5, alpha = 0.5) +
      geom_point(data = pts, aes(x, y, color = cong), size = 3.5, alpha = 0.9) +
      scale_color_manual(values = c(colores$primario, colores$acento, colores$secundario)) +
      annotate("text", x = centros$cx, y = centros$cy + 1.2,
               label = paste("C", 1:3), size = 3.5,
               color = colores$texto, fontface = "bold") +
      labs(title = "Por conglomerados — grupos seleccionados vs. no seleccionados (gris)")

  } else if (tipo == "conveniencia") {
    pts_acc <- data.frame(x = runif(15, 0.5, 4), y = runif(15, 0.5, 4))
    pts_no  <- data.frame(x = runif(10, 5, 9.5), y = runif(10, 5, 9.5))
    base +
      annotate("rect", xmin = 0, xmax = 4.2, ymin = 0, ymax = 4.2,
               fill = colores$primario, alpha = 0.07) +
      annotate("text", x = 2,   y = 4.6, label = "Zona accesible",
               size = 3.5, color = colores$primario) +
      annotate("text", x = 7.5, y = 9.5, label = "Zona inaccesible",
               size = 3.5, color = "#A3ACB9") +
      geom_point(data = pts_no,  aes(x, y), color = "#A3ACB9",      size = 2.5, alpha = 0.5) +
      geom_point(data = pts_acc, aes(x, y), color = colores$acento, size = 3.5, alpha = 0.85) +
      labs(title = "Por conveniencia — unidades de fácil acceso")

  } else if (tipo == "intencional") {
    ref <- data.frame(x = c(1.5, 3, 5, 8, 9), y = c(8, 6, 9, 7, 4))
    deg <- data.frame(x = c(2, 4, 7, 9, 6),   y = c(2, 3, 2, 1, 4))
    base +
      geom_point(data = ref, aes(x, y),
                 color = colores$primario, size = 4, shape = 17, alpha = 0.9) +
      geom_point(data = deg, aes(x, y),
                 color = colores$peligro,  size = 4, shape = 16, alpha = 0.9) +
      annotate("text", x = 9.5, y = 9.5,
               label = "\u25b2 Referencia", size = 3, color = colores$primario, hjust = 1) +
      annotate("text", x = 9.5, y = 8.8,
               label = "\u25cf Degradado",  size = 3, color = colores$peligro,  hjust = 1) +
      labs(title = "Intencional — sitios seleccionados por criterio experto")

  } else if (tipo == "transectos") {
    trans <- data.frame(x1 = c(1,1,1), x2 = c(9,9,9), y = c(2,5,8))
    pts   <- do.call(rbind, lapply(c(2,5,8), function(yt) {
      data.frame(x = seq(1.5, 8.5, by = 1.5), y = yt + rnorm(6, 0, 0.15))
    }))
    base +
      geom_segment(data = trans, aes(x = x1, xend = x2, y = y, yend = y),
                   color = colores$secundario, linewidth = 1) +
      geom_point(data = pts, aes(x, y),
                 color = colores$primario, size = 3, alpha = 0.85) +
      labs(title = "Transectos — 3 líneas con puntos de registro")

  } else if (tipo == "parcelas") {
    centros <- data.frame(cx = c(1.5, 4, 7, 2.5, 6, 8.5),
                          cy = c(1.5, 2, 1.5, 6,  7, 6.5))
    r <- 0.9
    base +
      geom_rect(data = centros,
                aes(xmin = cx - r, xmax = cx + r, ymin = cy - r, ymax = cy + r),
                fill = colores$primario, alpha = 0.15,
                color = colores$primario, linewidth = 0.7) +
      geom_point(data = centros, aes(cx, cy),
                 color = colores$primario, size = 2, shape = 3) +
      labs(title = "Parcelas — áreas delimitadas distribuidas en el área de estudio")

  } else if (tipo == "puntos") {
    pts <- data.frame(x = c(2, 5, 8, 3, 7, 5), y = c(2, 2, 2, 7, 7, 5))
    p   <- base +
      geom_point(data = pts, aes(x, y),
                 color = colores$primario, size = 4, shape = 16)
    for (i in seq_len(nrow(pts))) {
      p <- p + annotate("path",
                        x = pts$x[i] + 1.5 * cos(seq(0, 2*pi, length.out = 60)),
                        y = pts$y[i] + 1.5 * sin(seq(0, 2*pi, length.out = 60)),
                        color = colores$secundario, alpha = 0.4, linewidth = 0.6)
    }
    p + labs(title = "Muestreo en puntos — puntos fijos con radio de detección")

  } else if (tipo == "adaptativo") {
    xs   <- seq(0.5, 9.5, by = 2)
    ys   <- seq(0.5, 9.5, by = 2)
    grid <- expand.grid(x = xs, y = ys)
    det  <- grid[c(3, 8, 13), ]
    int  <- data.frame(
      x = c(det$x[1] + c(-2,0,2,0), det$x[2] + c(-2,0,2,0)),
      y = c(det$y[1] + c(0,2,0,-2), det$y[2] + c(0,2,0,-2))
    )
    base +
      geom_point(data = grid, aes(x, y), color = "#A3ACB9",          size = 2.5, alpha = 0.5) +
      geom_point(data = int,  aes(x, y), color = colores$secundario, size = 3.5, alpha = 0.8) +
      geom_point(data = det,  aes(x, y), color = colores$peligro,    size = 5,   shape = 8) +
      annotate("text", x = 9.5, y = 9.5,
               label = "\u2731 Detecci\u00f3n inicial", size = 3,
               color = colores$peligro, hjust = 1) +
      annotate("text", x = 9.5, y = 8.8,
               label = "\u25cf Intensificaci\u00f3n", size = 3,
               color = colores$secundario, hjust = 1) +
      labs(title = "Muestreo adaptativo — intensificación en zonas de detección")
  }
}

# ── UI ────────────────────────────────────────────────────
mod_muestreo_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    title = "Diseños de muestreo",

    navset_card_tab(

      # ── Pestaña 0: Conceptos básicos ───────────────────
      nav_panel(
        title = tagList(bs_icon("book", class = "me-1"), "Conceptos básicos"),

        div(
          class = "p-3",

          p(
            "El muestreo es el proceso de seleccionar un subconjunto de unidades
             de una población para estimar sus parámetros. Una muestra bien diseñada
             garantiza representatividad y permite hacer inferencias válidas sin
             necesidad de medir a todos los individuos.",
            class = "text-muted mb-4"
          ),

          # ── Jerarquía del muestreo ──────────────────────
          h5("Jerarquía del muestreo",
             style = paste0("color:", colores$primario,
                            "; border-bottom: 2px solid ", colores$borde,
                            "; padding-bottom: 0.4rem; margin-bottom: 1rem;")),

          card(
            card_body(
              tags$svg(
                xmlns   = "http://www.w3.org/2000/svg",
                viewBox = "0 0 680 130",
                style   = "width: 100%; display: block;",
                role    = "img",

                tags$defs(
                  tags$marker(
                    id="arr2", viewBox="0 0 10 10", refX="8", refY="5",
                    markerWidth="6", markerHeight="6", orient="auto-start-reverse",
                    tags$path(d="M2 1L8 5L2 9", fill="none", stroke="context-stroke",
                              `stroke-width`="1.5", `stroke-linecap`="round",
                              `stroke-linejoin`="round")
                  )
                ),

                # Box 1: Elemento (gris)
                tags$rect(x="10", y="30", width="110", height="60", rx="8",
                          fill="#F1EFE8", stroke="#5F5E5A", `stroke-width`="0.5"),
                tags$text(x="65", y="52", `text-anchor`="middle",
                          `dominant-baseline`="central", `font-size`="13",
                          `font-weight`="500", fill="#2C2C2A", "Elemento"),
                tags$text(x="65", y="72", `text-anchor`="middle",
                          `dominant-baseline`="central", `font-size`="9",
                          fill="#5F5E5A", "árbol, individuo,"),
                tags$text(x="65", y="86", `text-anchor`="middle",
                          `dominant-baseline`="central", `font-size`="9",
                          fill="#5F5E5A", "observación"),

                # Flecha 1→2
                tags$line(x1="122", y1="60", x2="142", y2="60",
                          stroke="#B4B2A9", `stroke-width`="1.5",
                          `marker-end`="url(#arr2)"),

                # Box 2: Unidad de muestreo (azul)
                tags$rect(x="145", y="30", width="120", height="60", rx="8",
                          fill="#E6F1FB", stroke="#185FA5", `stroke-width`="0.5"),
                tags$text(x="205", y="48", `text-anchor`="middle",
                          `dominant-baseline`="central", `font-size`="13",
                          `font-weight`="500", fill="#0C447C", "Unidad de"),
                tags$text(x="205", y="64", `text-anchor`="middle",
                          `dominant-baseline`="central", `font-size`="13",
                          `font-weight`="500", fill="#0C447C", "muestreo"),
                tags$text(x="205", y="82", `text-anchor`="middle",
                          `dominant-baseline`="central", `font-size`="9",
                          fill="#185FA5", "parcela, trampa"),

                # Flecha 2→3
                tags$line(x1="267", y1="60", x2="287", y2="60",
                          stroke="#B4B2A9", `stroke-width`="1.5",
                          `marker-end`="url(#arr2)"),

                # Box 3: Marco de muestreo (azul)
                tags$rect(x="290", y="30", width="120", height="60", rx="8",
                          fill="#E6F1FB", stroke="#185FA5", `stroke-width`="0.5"),
                tags$text(x="350", y="48", `text-anchor`="middle",
                          `dominant-baseline`="central", `font-size`="13",
                          `font-weight`="500", fill="#0C447C", "Marco de"),
                tags$text(x="350", y="64", `text-anchor`="middle",
                          `dominant-baseline`="central", `font-size`="13",
                          `font-weight`="500", fill="#0C447C", "muestreo"),
                tags$text(x="350", y="82", `text-anchor`="middle",
                          `dominant-baseline`="central", `font-size`="9",
                          fill="#185FA5", "lista de unidades"),

                # Flecha 3→4
                tags$line(x1="412", y1="60", x2="432", y2="60",
                          stroke="#B4B2A9", `stroke-width`="1.5",
                          `marker-end`="url(#arr2)"),

                # Box 4: Población muestreada (teal)
                tags$rect(x="435", y="20", width="110", height="38", rx="8",
                          fill="#E1F5EE", stroke="#0F6E56", `stroke-width`="0.5"),
                tags$text(x="490", y="33", `text-anchor`="middle",
                          `dominant-baseline`="central", `font-size`="13",
                          `font-weight`="500", fill="#085041", "Población"),
                tags$text(x="490", y="50", `text-anchor`="middle",
                          `dominant-baseline`="central", `font-size`="13",
                          `font-weight`="500", fill="#085041", "muestreada"),
                tags$rect(x="435", y="66", width="110", height="26", rx="8",
                          fill="#E1F5EE", stroke="#0F6E56", `stroke-width`="0.5"),
                tags$text(x="490", y="79", `text-anchor`="middle",
                          `dominant-baseline`="central", `font-size`="9",
                          fill="#0F6E56", "dentro del marco"),

                # Flecha 4→5 (punteada)
                tags$line(x1="547", y1="60", x2="567", y2="60",
                          stroke="#B4B2A9", `stroke-width`="1.5",
                          `stroke-dasharray`="4 3",
                          `marker-end`="url(#arr2)"),

                # Box 5: Población objetivo (ámbar, punteado)
                tags$rect(x="570", y="15", width="100", height="90", rx="8",
                          fill="#FAEEDA", stroke="#BA7517", `stroke-width`="1",
                          `stroke-dasharray`="5 3"),
                tags$text(x="620", y="42", `text-anchor`="middle",
                          `dominant-baseline`="central", `font-size`="13",
                          `font-weight`="500", fill="#633806", "Población"),
                tags$text(x="620", y="58", `text-anchor`="middle",
                          `dominant-baseline`="central", `font-size`="13",
                          `font-weight`="500", fill="#633806", "objetivo"),
                tags$text(x="620", y="75", `text-anchor`="middle",
                          `dominant-baseline`="central", `font-size`="9",
                          fill="#BA7517", "a la que se"),
                tags$text(x="620", y="89", `text-anchor`="middle",
                          `dominant-baseline`="central", `font-size`="9",
                          fill="#BA7517", "quiere generalizar")
              ),
              p(class = "text-muted small mt-2 mb-0",
                "La flecha punteada indica que la población muestreada idealmente
                 coincide con la población objetivo, pero en la práctica pueden diferir. ",
                strong("En la mayoría de diseños simples, el elemento y la unidad de
                 muestreo son el mismo objeto"), " — la distinción importa principalmente
                 en muestreo por conglomerados, donde se selecciona un grupo (unidad)
                 pero se mide cada individuo dentro de él (elemento).")
            )
          ),

          br(),

          # ── Población objetivo vs. muestreada ──────────
          h5("Población objetivo vs. población muestreada",
             style = paste0("color:", colores$primario,
                            "; border-bottom: 2px solid ", colores$borde,
                            "; padding-bottom: 0.4rem; margin-bottom: 1rem;")),

          card(
            card_body(
              tags$svg(
                xmlns   = "http://www.w3.org/2000/svg",
                viewBox = "0 0 680 390",
                style   = "width: 100%; display: block;",
                role    = "img",

                tags$defs(
                  tags$pattern(id="grid-a", x="0", y="0", width="22", height="22",
                               patternUnits="userSpaceOnUse",
                               tags$rect(width="22", height="22",
                                         fill="#E1F5EE", stroke="#0F6E56", `stroke-width`="0.5")),
                  tags$pattern(id="grid-b", x="0", y="0", width="22", height="22",
                               patternUnits="userSpaceOnUse",
                               tags$rect(width="22", height="22",
                                         fill="#E1F5EE", stroke="#0F6E56", `stroke-width`="0.5"))
                ),

                # ── Situación A: Sin sesgo ──
                tags$text(x="160", y="22", `text-anchor`="middle",
                          `font-size`="13", `font-weight`="500", fill="#0C447C",
                          "Sin sesgo de cobertura"),
                tags$text(x="160", y="36", `text-anchor`="middle",
                          `font-size`="11", fill="#185FA5", "el grillado cubre toda el área"),
                tags$rect(x="30", y="50", width="260", height="230", rx="8",
                          fill="#E6F1FB", stroke="#185FA5",
                          `stroke-width`="1", `stroke-dasharray`="6 3"),
                tags$rect(x="50", y="62", width="220", height="205", rx="4",
                          fill="url(#grid-a)"),
                tags$rect(x="50", y="62", width="220", height="205", rx="4",
                          fill="none", stroke="#0F6E56", `stroke-width`="1.5"),
                # Etiqueta parcela A
                tags$circle(cx="100", cy="230", r="2", fill="#5F5E5A"),
                tags$line(x1="100", y1="232", x2="120", y2="295",
                          stroke="#5F5E5A", `stroke-width`="0.5", `stroke-dasharray`="3 2"),
                tags$text(x="122", y="293", `font-size`="11", fill="#444441",
                          "parcela (unidad de muestreo)"),
                # Árboles dentro
                tags$circle(cx="80",  cy="100", r="5", fill="#085041"),
                tags$circle(cx="118", cy="90",  r="5", fill="#085041"),
                tags$circle(cx="162", cy="105", r="5", fill="#085041"),
                tags$circle(cx="208", cy="88",  r="5", fill="#085041"),
                tags$circle(cx="248", cy="100", r="5", fill="#085041"),
                tags$circle(cx="70",  cy="145", r="5", fill="#085041"),
                tags$circle(cx="108", cy="138", r="5", fill="#085041"),
                tags$circle(cx="150", cy="150", r="5", fill="#085041"),
                tags$circle(cx="195", cy="135", r="5", fill="#085041"),
                tags$circle(cx="238", cy="148", r="5", fill="#085041"),
                tags$circle(cx="260", cy="138", r="5", fill="#085041"),
                tags$circle(cx="85",  cy="190", r="5", fill="#085041"),
                tags$circle(cx="128", cy="182", r="5", fill="#085041"),
                tags$circle(cx="170", cy="197", r="5", fill="#085041"),
                tags$circle(cx="215", cy="188", r="5", fill="#085041"),
                tags$circle(cx="252", cy="195", r="5", fill="#085041"),
                tags$circle(cx="75",  cy="238", r="5", fill="#085041"),
                tags$circle(cx="118", cy="230", r="5", fill="#085041"),
                tags$circle(cx="160", cy="244", r="5", fill="#085041"),
                tags$circle(cx="203", cy="234", r="5", fill="#085041"),
                tags$circle(cx="245", cy="240", r="5", fill="#085041"),
                # Leyenda A
                tags$rect(x="30", y="316", width="22", height="10", rx="2",
                          fill="none", stroke="#185FA5",
                          `stroke-width`="1", `stroke-dasharray`="4 2"),
                tags$text(x="58", y="325", `font-size`="11", fill="#185FA5",
                          "población objetivo"),
                tags$rect(x="30", y="332", width="22", height="10", rx="2",
                          fill="#E1F5EE", stroke="#0F6E56", `stroke-width`="1"),
                tags$text(x="58", y="341", `font-size`="11", fill="#085041",
                          "marco (grillado de parcelas)"),
                tags$circle(cx="41", cy="355", r="5", fill="#085041"),
                tags$text(x="52", y="359", `font-size`="11", fill="#085041",
                          "árbol en el marco"),

                # Separador
                tags$line(x1="340", y1="12", x2="340", y2="378",
                          stroke="#D3D1C7", `stroke-width`="1", `stroke-dasharray`="4 4"),

                # ── Situación B: Con sesgo ──
                tags$text(x="510", y="22", `text-anchor`="middle",
                          `font-size`="13", `font-weight`="500", fill="#791F1F",
                          "Con sesgo de cobertura"),
                tags$text(x="510", y="36", `text-anchor`="middle",
                          `font-size`="11", fill="#A32D2D",
                          "el grillado cubre solo parte del área"),
                tags$rect(x="355", y="50", width="310", height="230", rx="8",
                          fill="#E6F1FB", stroke="#185FA5",
                          `stroke-width`="1", `stroke-dasharray`="6 3"),
                tags$rect(x="460", y="80", width="185", height="170", rx="4",
                          fill="url(#grid-b)"),
                tags$rect(x="460", y="80", width="185", height="170", rx="4",
                          fill="none", stroke="#0F6E56", `stroke-width`="1.5"),
                # Etiqueta parcela B
                tags$circle(cx="530", cy="210", r="2", fill="#5F5E5A"),
                tags$line(x1="530", y1="212", x2="530", y2="268",
                          stroke="#5F5E5A", `stroke-width`="0.5", `stroke-dasharray`="3 2"),
                tags$text(x="530", y="280", `text-anchor`="middle",
                          `font-size`="11", fill="#444441",
                          "parcela (unidad de muestreo)"),
                # Árboles dentro
                tags$circle(cx="482", cy="105", r="5", fill="#085041"),
                tags$circle(cx="520", cy="95",  r="5", fill="#085041"),
                tags$circle(cx="562", cy="110", r="5", fill="#085041"),
                tags$circle(cx="602", cy="98",  r="5", fill="#085041"),
                tags$circle(cx="632", cy="107", r="5", fill="#085041"),
                tags$circle(cx="475", cy="148", r="5", fill="#085041"),
                tags$circle(cx="515", cy="140", r="5", fill="#085041"),
                tags$circle(cx="558", cy="155", r="5", fill="#085041"),
                tags$circle(cx="598", cy="144", r="5", fill="#085041"),
                tags$circle(cx="628", cy="150", r="5", fill="#085041"),
                tags$circle(cx="480", cy="195", r="5", fill="#085041"),
                tags$circle(cx="522", cy="188", r="5", fill="#085041"),
                tags$circle(cx="565", cy="200", r="5", fill="#085041"),
                tags$circle(cx="605", cy="192", r="5", fill="#085041"),
                tags$circle(cx="635", cy="198", r="5", fill="#085041"),
                tags$circle(cx="488", cy="238", r="5", fill="#085041"),
                tags$circle(cx="530", cy="232", r="5", fill="#085041"),
                tags$circle(cx="572", cy="244", r="5", fill="#085041"),
                tags$circle(cx="612", cy="236", r="5", fill="#085041"),
                # Árboles fuera
                tags$circle(cx="372", cy="80",  r="5", fill="#E24B4A"),
                tags$circle(cx="400", cy="72",  r="5", fill="#E24B4A"),
                tags$circle(cx="430", cy="82",  r="5", fill="#E24B4A"),
                tags$circle(cx="370", cy="120", r="5", fill="#E24B4A"),
                tags$circle(cx="405", cy="112", r="5", fill="#E24B4A"),
                tags$circle(cx="438", cy="125", r="5", fill="#E24B4A"),
                tags$circle(cx="375", cy="162", r="5", fill="#E24B4A"),
                tags$circle(cx="410", cy="155", r="5", fill="#E24B4A"),
                tags$circle(cx="440", cy="168", r="5", fill="#E24B4A"),
                tags$circle(cx="378", cy="205", r="5", fill="#E24B4A"),
                tags$circle(cx="412", cy="198", r="5", fill="#E24B4A"),
                tags$circle(cx="442", cy="210", r="5", fill="#E24B4A"),
                tags$circle(cx="382", cy="248", r="5", fill="#E24B4A"),
                tags$circle(cx="415", cy="240", r="5", fill="#E24B4A"),
                tags$circle(cx="444", cy="252", r="5", fill="#E24B4A"),
                # Etiqueta zona excluida
                tags$rect(x="352", y="292", width="100", height="34", rx="4",
                          fill="#FCEBEB", stroke="#A32D2D", `stroke-width`="0.5"),
                tags$text(x="402", y="305", `text-anchor`="middle",
                          `font-size`="11", fill="#791F1F", "fuera del marco"),
                tags$text(x="402", y="319", `text-anchor`="middle",
                          `font-size`="11", fill="#791F1F", "probabilidad = 0"),
                # Leyenda B
                tags$circle(cx="368", cy="340", r="5", fill="#E24B4A"),
                tags$text(x="380", y="344", `font-size`="11", fill="#791F1F",
                          "árbol fuera del marco"),
                tags$circle(cx="368", cy="358", r="5", fill="#E24B4A"),
                tags$text(x="380", y="362", `font-size`="11", fill="#791F1F",
                          "no puede ser muestreado → sesgo")
              ),
              p(class = "text-muted small mt-2 mb-0",
                "Cuando el marco de muestreo no cubre toda la población objetivo,
                 los individuos fuera del marco nunca pueden ser seleccionados.
                 Esto introduce un sesgo sistemático que no puede corregirse
                 aumentando el tamaño de muestra.")
            )
          ),

          br(),

          # ── Términos clave ──────────────────────────────
          h5("Términos clave",
             style = paste0("color:", colores$primario,
                            "; border-bottom: 2px solid ", colores$borde,
                            "; padding-bottom: 0.4rem; margin-bottom: 1rem;")),

          layout_columns(
            col_widths = c(4, 4, 4, 4, 4),
            gap = "8px",

            card(
              class = "h-100",
              card_header(bs_icon("bullseye"), " Población objetivo"),
              card_body(
                p(class = "small mb-0",
                  "La población sobre la cual se quieren hacer inferencias.
                   Debe estar claramente definida antes de diseñar el muestreo.")
              )
            ),
            card(
              class = "h-100",
              card_header(bs_icon("grid-3x3"), " Marco de muestreo"),
              card_body(
                p(class = "small mb-0",
                  "Lista o mapa de todas las unidades de muestreo disponibles.
                   Define la población muestreada — la que queda dentro del marco.")
              )
            ),
            card(
              class = "h-100",
              card_header(bs_icon("square"), " Unidad de muestreo"),
              card_body(
                p(class = "small mb-0",
                  "La unidad que se selecciona del marco: una parcela, un transecto,
                   un individuo. En diseños simples ", strong("coincide con el elemento"),
                  "; solo difieren en muestreo por conglomerados.")
              )
            ),
            card(
              class = "h-100",
              card_header(bs_icon("dot"), " Elemento"),
              card_body(
                p(class = "small mb-0",
                  "El objeto sobre el que se toma la medición: un árbol, un individuo,
                   una observación. Frecuentemente es la misma entidad que la unidad
                   de muestreo.")
              )
            ),
            card(
              class = "h-100",
              card_header(bs_icon("collection"), " Muestra"),
              card_body(
                p(class = "small mb-0",
                  "El subconjunto de unidades seleccionadas del marco de muestreo.
                   Debe ser representativa de la población objetivo para que las
                   inferencias sean válidas.")
              )
            )
          ),

          br(),

          # ── Fuentes de variación ────────────────────────
          h5("Fuentes de variación",
             style = paste0("color:", colores$primario,
                            "; border-bottom: 2px solid ", colores$borde,
                            "; padding-bottom: 0.4rem; margin-bottom: 1rem;")),

          tags$svg(
            xmlns   = "http://www.w3.org/2000/svg",
            viewBox = "0 0 680 160",
            style   = "width: 100%; display: block; margin-bottom: 0.5rem;",
            role    = "img",

            # Panel 1: Variación temporal (azul)
            tags$rect(x="20", y="10", width="190", height="140", rx="10",
                      fill="#E6F1FB", stroke="#185FA5", `stroke-width`="0.5"),
            tags$text(x="115", y="35", `text-anchor`="middle",
                      `dominant-baseline`="central", `font-size`="13",
                      `font-weight`="500", fill="#0C447C", "Variación temporal"),
            tags$path(d="M40 115 C60 85, 80 125, 100 90 C120 55, 140 110, 160 78 C175 55, 185 100, 200 82",
                      fill="none", stroke="#185FA5", `stroke-width`="1.5"),
            tags$line(x1="35", y1="120", x2="205", y2="120",
                      stroke="#185FA5", `stroke-width`="0.5", opacity="0.4"),
            tags$text(x="42", y="134", `text-anchor`="middle",
                      `dominant-baseline`="central", `font-size`="11",
                      fill="#185FA5", opacity="0.7", "t₁"),
            tags$text(x="198", y="134", `text-anchor`="middle",
                      `dominant-baseline`="central", `font-size`="11",
                      fill="#185FA5", opacity="0.7", "t₂"),

            # Panel 2: Variación espacial (teal)
            tags$rect(x="245", y="10", width="190", height="140", rx="10",
                      fill="#E1F5EE", stroke="#0F6E56", `stroke-width`="0.5"),
            tags$text(x="340", y="35", `text-anchor`="middle",
                      `dominant-baseline`="central", `font-size`="13",
                      `font-weight`="500", fill="#085041", "Variación espacial"),
            tags$circle(cx="275", cy="80", r="5", fill="#0F6E56", opacity="0.7"),
            tags$circle(cx="285", cy="65", r="5", fill="#0F6E56", opacity="0.7"),
            tags$circle(cx="278", cy="95", r="5", fill="#0F6E56", opacity="0.7"),
            tags$circle(cx="293", cy="78", r="5", fill="#0F6E56", opacity="0.7"),
            tags$circle(cx="288", cy="55", r="3", fill="#0F6E56", opacity="0.4"),
            tags$circle(cx="340", cy="110", r="5", fill="#0F6E56", opacity="0.7"),
            tags$circle(cx="352", cy="100", r="5", fill="#0F6E56", opacity="0.7"),
            tags$circle(cx="345", cy="125", r="5", fill="#0F6E56", opacity="0.7"),
            tags$circle(cx="395", cy="70",  r="3", fill="#0F6E56", opacity="0.3"),
            tags$circle(cx="405", cy="85",  r="3", fill="#0F6E56", opacity="0.3"),
            tags$circle(cx="390", cy="90",  r="3", fill="#0F6E56", opacity="0.3"),

            # Panel 3: Variación de muestreo (ámbar)
            tags$rect(x="470", y="10", width="190", height="140", rx="10",
                      fill="#FAEEDA", stroke="#BA7517", `stroke-width`="0.5"),
            tags$text(x="565", y="35", `text-anchor`="middle",
                      `dominant-baseline`="central", `font-size`="13",
                      `font-weight`="500", fill="#633806", "Variabilidad de muestreo"),
            tags$rect(x="485", y="55", width="65", height="65", rx="4",
                      fill="none", stroke="#BA7517", `stroke-width`="1",
                      `stroke-dasharray`="3 2", opacity="0.6"),
            tags$circle(cx="500", cy="80",  r="4", fill="#854F0B", opacity="0.7"),
            tags$circle(cx="515", cy="68",  r="4", fill="#854F0B", opacity="0.7"),
            tags$circle(cx="530", cy="88",  r="4", fill="#854F0B", opacity="0.7"),
            tags$circle(cx="508", cy="102", r="4", fill="#854F0B", opacity="0.7"),
            tags$rect(x="570", y="55", width="65", height="65", rx="4",
                      fill="none", stroke="#BA7517", `stroke-width`="1",
                      `stroke-dasharray`="3 2", opacity="0.6"),
            tags$circle(cx="585", cy="75",  r="4", fill="#854F0B", opacity="0.7"),
            tags$circle(cx="600", cy="95",  r="4", fill="#854F0B", opacity="0.7"),
            tags$circle(cx="618", cy="80",  r="4", fill="#854F0B", opacity="0.7"),
            tags$circle(cx="594", cy="110", r="4", fill="#854F0B", opacity="0.7"),
            tags$text(x="520", y="130", `text-anchor`="middle",
                      `dominant-baseline`="central", `font-size`="11",
                      fill="#854F0B", "muestra 1"),
            tags$text(x="603", y="130", `text-anchor`="middle",
                      `dominant-baseline`="central", `font-size`="11",
                      fill="#854F0B", "muestra 2")
          ),

          layout_columns(
            col_widths = c(4, 4, 4),
            gap = "8px",
            p(class = "small text-muted mb-0",
              tags$strong("Temporal:"), " variabilidad asociada al período de tiempo
               del muestreo. Las poblaciones cambian estacionalmente y entre años."),
            p(class = "small text-muted mb-0",
              tags$strong("Espacial:"), " variabilidad asociada a la distribución
               parchosa de individuos. Las especies rara vez se distribuyen homogéneamente."),
            p(class = "small text-muted mb-0",
              tags$strong("De muestreo:"), " variabilidad debida al proceso de
               seleccionar unidades al azar. Distintas muestras dan distintos resultados.")
          )
        )
      ),

      # ── Pestaña 1: Diseños ──────────────────────────────
      nav_panel(
        title = tagList(bs_icon("grid-3x3-gap", class = "me-1"), "Diseños de muestreo"),

        div(
          class = "p-3",

          p(
            "El diseño de muestreo determina cómo se seleccionan las unidades de
             observación. Una muestra bien diseñada garantiza representatividad y
             permite hacer inferencias válidas sobre la población o el ecosistema
             de interés.",
            class = "text-muted mb-4"
          ),

          h5("Probabilísticos — permiten inferencia estadística",
             style = paste0("color:", colores$primario, "; border-bottom: 2px solid ",
                            colores$borde, "; padding-bottom: 0.4rem; margin-bottom: 1rem;")),
          uiOutput(ns("cards_prob")),

          br(),

          h5("No probabilísticos — exploratorios o con restricciones de acceso",
             style = paste0("color:", colores$acento, "; border-bottom: 2px solid ",
                            colores$borde, "; padding-bottom: 0.4rem; margin-bottom: 1rem;")),
          uiOutput(ns("cards_noprob")),

          br(),

          h5("Diseños espaciales — propios de ecología y ciencias ambientales",
             style = paste0("color:", colores$secundario, "; border-bottom: 2px solid ",
                            colores$borde, "; padding-bottom: 0.4rem; margin-bottom: 1rem;")),
          uiOutput(ns("cards_espacial"))
        )
      ),

      # ── Pestaña 2: Esquema visual ────────────────────────
      nav_panel(
        title = tagList(bs_icon("image", class = "me-1"), "Esquema visual"),

        div(
          class = "p-3",

          layout_sidebar(
            fillable = FALSE,

            sidebar = sidebar(
              width = 240,
              title = "Tipo de muestreo",

              selectInput(
                ns("esquema_sel"),
                label = NULL,
                choices = list(
                  "Probabilísticos" = c(
                    "Aleatorio simple"        = "alea_simple",
                    "Aleatorio estratificado" = "alea_estrat",
                    "Sistemático"             = "sistematico",
                    "Por conglomerados"       = "conglomerados"
                  ),
                  "No probabilísticos" = c(
                    "Por conveniencia" = "conveniencia",
                    "Intencional"      = "intencional"
                  ),
                  "Espaciales" = c(
                    "Transectos"          = "transectos",
                    "Parcelas"            = "parcelas",
                    "Muestreo en puntos"  = "puntos",
                    "Muestreo adaptativo" = "adaptativo"
                  )
                )
              ),

              hr(),
              div(
                class = "small text-muted",
                "El esquema muestra una representación espacial simplificada del
                 diseño sobre un área ficticia de 10 × 10 unidades."
              )
            ),

            div(
              card(
                card_header("Representación espacial"),
                plotOutput(ns("esquema_plot"), height = "320px")
              ),

              br(),

              uiOutput(ns("esquema_detalle"))
            )
          )
        )
      ),

      # ── Pestaña 3: Calculadora ──────────────────────────
      nav_panel(
        title = tagList(bs_icon("calculator", class = "me-1"), "Calculadora de tamaño de muestra"),

        div(
          class = "p-3",

          layout_sidebar(
            fillable = FALSE,

            sidebar = sidebar(
              width = 280,
              title = "Parámetros",

              # ── Selector tipo wiz-opt ─────────────────────
              uiOutput(ns("selector_tipo")),

              uiOutput(ns("sidebar_params")),

              hr(),
              div(
                class = "small text-muted",
                strong("Fórmula usada:"), br(),
                uiOutput(ns("formula_texto"))
              )
            ),

            uiOutput(ns("panel_resultados"))
          )
        )
      ),

      # ── Pestaña 4: Ejercicio MAS ────────────────────────
      nav_panel(
        title = tagList(bs_icon("play-circle", class = "me-1"), "Ejercicio"),

        div(
          class = "p-3",

          p(
            "Ejercicio de muestreo aleatorio simple (MAS) sobre una población simulada
             de árboles con valores de DAP (diámetro a la altura del pecho, cm).
             Ajustá los parámetros, tomá una muestra y observá las métricas resultantes.",
            class = "text-muted mb-4"
          ),

          layout_columns(
            col_widths = c(3, 9),

            # ── Sidebar de controles ──────────────────────
            card(
              card_header(
                tagList(bs_icon("sliders", class = "me-1"), "Parámetros del ejercicio")
              ),
              card_body(

                numericInput(
                  ns("ej_N"),
                  label = "Tamaño de la población (N)",
                  value = 200, min = 50, max = 2000, step = 50
                ),

                numericInput(
                  ns("ej_n"),
                  label = "Tamaño de muestra (n)",
                  value = 30, min = 5, max = 500, step = 1
                ),

                hr(),

                numericInput(
                  ns("ej_media_pob"),
                  label = "Media DAP población (cm)",
                  value = 25, min = 5, max = 100, step = 1
                ),

                numericInput(
                  ns("ej_sd_pob"),
                  label = "Desvío estándar población (cm)",
                  value = 8, min = 1, max = 40, step = 1
                ),

                numericInput(
                  ns("ej_seed"),
                  label = "Semilla aleatoria",
                  value = 42, min = 1, max = 9999, step = 1
                ),

                hr(),

                actionButton(
                  ns("ej_muestrear"),
                  label  = "Tomar muestra",
                  class  = "btn-primary w-100",
                  icon   = icon("shuffle")
                ),

                br(), br(),

                actionButton(
                  ns("ej_resetear"),
                  label = "Reiniciar",
                  class = "btn-outline-secondary w-100",
                  icon  = icon("rotate-left")
                ),

                br(), br(),

                div(
                  class = "small text-muted",
                  bs_icon("info-circle"), " ",
                  "El DAP se simula con distribución normal truncada (DAP mínimo: 1 cm).
                   Cada clic en «Tomar muestra» genera una selección aleatoria distinta."
                )
              )
            ),

            # ── Panel de resultados ───────────────────────
            div(

              # Métricas
              uiOutput(ns("ej_metricas")),

              br(),

              # Gráfico
              card(
                card_header(
                  tagList(bs_icon("bar-chart", class = "me-1"),
                          "Distribución de DAP — población y muestra")
                ),
                plotOutput(ns("ej_plot"), height = "320px")
              ),

              br(),

              # Tabla
              card(
                card_header(
                  tagList(bs_icon("table", class = "me-1"),
                          "Unidades seleccionadas en la muestra")
                ),
                card_body(
                  uiOutput(ns("ej_tabla_wrap"))
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
mod_muestreo_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Renderizar cards de diseños ──────────────────────
    render_cards <- function(grupo) {
      items <- diseños_muestreo[[grupo]]$items
      badge <- diseños_muestreo[[grupo]]$badge

      renderUI({
        layout_columns(
          col_widths = rep(6, length(items)),
          !!!lapply(items, function(d) {
            card(
              class = "card-muestreo",
              card_header(
                div(
                  span(class = paste("badge badge-", badge, " me-2", sep = ""), badge_label(badge)),
                  strong(d$nombre)
                )
              ),
              p(d$desc, class = "text-muted small"),
              tags$dl(
                class = "row small mb-0",
                tags$dt(class = "col-sm-4", "¿Cuándo usar?"),
                tags$dd(class = "col-sm-8", d$cuando),
                tags$dt(class = "col-sm-4", "Ventaja"),
                tags$dd(class = "col-sm-8", d$ventaja),
                tags$dt(class = "col-sm-4", "Limitación"),
                tags$dd(class = "col-sm-8", d$limitacion)
              ),
              hr(class = "my-2"),
              p(class = "fst-italic small text-muted mb-0",
                tags$strong("Ejemplo: "), d$ejemplo)
            )
          })
        )
      })
    }

    output$cards_prob     <- render_cards("probabilisticos")
    output$cards_noprob   <- render_cards("no_probabilisticos")
    output$cards_espacial <- render_cards("espaciales")

    # ── Selector de tipo (botones wiz-opt) ───────────────
    calc_tipo <- reactiveVal("prop")

    observeEvent(input$btn_prop,        { calc_tipo("prop") })
    observeEvent(input$btn_media,       { calc_tipo("media") })
    observeEvent(input$btn_comp_medias, { calc_tipo("comp_medias") })
    observeEvent(input$btn_comp_prop,   { calc_tipo("comp_prop") })

    output$selector_tipo <- renderUI({
      sel <- calc_tipo()

      btn <- function(id, label, valor) {
        es_sel <- sel == valor
        actionButton(
          ns(id), label,
          class = paste("wiz-opt w-100 text-start mb-1",
                        if (es_sel) "border border-primary" else ""),
          style = if (es_sel)
            paste0("border-color:", colores$primario, " !important;",
                   "background:", colores$fondo, ";")
          else ""
        )
      }

      div(
        # Grupo estimación
        div(
          class = "small fw-semibold mb-1 mt-1",
          style = paste0("color:", colores$acento),
          "\U0001f4ca Estimación"
        ),
        btn("btn_prop",  "Proporción", "prop"),
        btn("btn_media", "Media continua", "media"),

        # Grupo comparación
        div(
          class = "small fw-semibold mb-1 mt-2",
          style = paste0("color:", colores$acento),
          "\u2696\ufe0f Comparación de grupos"
        ),
        btn("btn_comp_prop",   "Dos proporciones", "comp_prop"),
        btn("btn_comp_medias", "Dos medias",       "comp_medias")
      )
    })

    # ── Sidebar dinámico de parámetros ───────────────────
    output$sidebar_params <- renderUI({
      tipo <- calc_tipo()

      if (is.null(tipo) || tipo == "prop") {
        tagList(
          numericInput(ns("calc_p"), "Proporción esperada (p)",
                       value = 0.5, min = 0.01, max = 0.99, step = 0.01),
          selectInput(ns("calc_z"), "Nivel de confianza",
                      choices = c("95%" = 1.96, "99%" = 2.576, "90%" = 1.645), selected = 1.96),
          numericInput(ns("calc_e"), "Error máximo aceptable (e)",
                       value = 0.05, min = 0.01, max = 0.5, step = 0.01),
          numericInput(ns("calc_N"), "Tamaño de población (N) — 0 = infinita",
                       value = 0, min = 0, step = 1)
        )
      } else if (tipo == "media") {
        tagList(
          numericInput(ns("calc_cv"), "Coeficiente de variación esperado (%)",
                       value = 30, min = 1, max = 200, step = 1),
          selectInput(ns("calc_z"), "Nivel de confianza",
                      choices = c("95%" = 1.96, "99%" = 2.576, "90%" = 1.645), selected = 1.96),
          numericInput(ns("calc_e"), "Error máximo aceptable (e)",
                       value = 0.05, min = 0.01, max = 0.5, step = 0.01),
          numericInput(ns("calc_N"), "Tamaño de población (N) — 0 = infinita",
                       value = 0, min = 0, step = 1)
        )
      } else if (tipo == "comp_medias") {
        tagList(
          numericInput(ns("comp_delta"), "Diferencia mínima detectable (\u03b4)",
                       value = 5, min = 0.01, step = 0.1),
          numericInput(ns("comp_sigma"), "Desviación estándar esperada (\u03c3)",
                       value = 10, min = 0.01, step = 0.1),
          selectInput(ns("comp_alpha"), "Nivel de significancia (\u03b1)",
                      choices = c("\u03b1 = 0.01" = "0.01", "\u03b1 = 0.05" = "0.05",
                                  "\u03b1 = 0.10" = "0.10"), selected = "0.05"),
          selectInput(ns("comp_pot"), "Potencia (1 \u2212 \u03b2)",
                      choices = c("70%" = "0.70", "80%" = "0.80",
                                  "90%" = "0.90", "95%" = "0.95"), selected = "0.80")
        )
      } else if (tipo == "comp_prop") {
        tagList(
          numericInput(ns("comp_p1"), "Proporción grupo 1 (p\u2081)",
                       value = 0.30, min = 0.01, max = 0.99, step = 0.01),
          numericInput(ns("comp_p2"), "Proporción grupo 2 (p\u2082)",
                       value = 0.50, min = 0.01, max = 0.99, step = 0.01),
          selectInput(ns("comp_alpha2"), "Nivel de significancia (\u03b1)",
                      choices = c("\u03b1 = 0.01" = "0.01", "\u03b1 = 0.05" = "0.05",
                                  "\u03b1 = 0.10" = "0.10"), selected = "0.05"),
          selectInput(ns("comp_pot2"), "Potencia (1 \u2212 \u03b2)",
                      choices = c("70%" = "0.70", "80%" = "0.80",
                                  "90%" = "0.90", "95%" = "0.95"), selected = "0.80")
        )
      }
    })

    # ── Calculadora: estimación ───────────────────────────
    es_estimacion <- reactive({
      calc_tipo() %in% c("prop", "media")
    })

    es_comparacion <- reactive({
      calc_tipo() %in% c("comp_medias", "comp_prop")
    })

    calc <- reactive({
      req(es_estimacion())
      z  <- as.numeric(input$calc_z)
      e  <- input$calc_e
      N  <- input$calc_N

      n0 <- if (calc_tipo() == "prop") {
        p <- input$calc_p
        ceiling((z^2 * p * (1 - p)) / e^2)
      } else {
        cv <- input$calc_cv / 100
        ceiling((z * cv / e)^2)
      }

      nc   <- if (N > 0 && N > n0) ceiling(n0 / (1 + n0 / N)) else NA
      frac <- if (N > 0) {
        n_usar <- ifelse(is.na(nc), n0, nc)
        paste0(round(n_usar / N * 100, 1), "%")
      } else "—"

      list(
        n0   = n0, nc = nc, frac = frac, N = N, z = z, e = e,
        tipo = calc_tipo(),
        p    = if (calc_tipo() == "prop")  input$calc_p  else NULL,
        cv   = if (calc_tipo() == "media") input$calc_cv else NULL
      )
    })

    # ── Calculadora: comparación ──────────────────────────
    calc_comp <- reactive({
      req(es_comparacion())

      if (calc_tipo() == "comp_medias") {
        delta <- input$comp_delta
        sigma <- input$comp_sigma
        alpha <- as.numeric(input$comp_alpha)
        pot   <- as.numeric(input$comp_pot)
        req(delta > 0, sigma > 0)

        z_a <- qnorm(1 - alpha / 2)
        z_b <- qnorm(pot)
        n1  <- ceiling(2 * (z_a + z_b)^2 * sigma^2 / delta^2)

        # Curva de potencia
        n_seq <- seq(2, max(n1 * 3, 30))
        pot_v <- pnorm(sqrt(n_seq / 2) * abs(delta) / sigma - z_a) +
          pnorm(-sqrt(n_seq / 2) * abs(delta) / sigma - z_a)

        list(tipo = "comp_medias", n1 = n1, n2 = n1, total = n1 * 2,
             alpha = alpha, pot = pot, z_a = z_a, z_b = z_b,
             delta = delta, sigma = sigma,
             curv  = data.frame(n = n_seq, potencia = pot_v))

      } else {
        p1    <- input$comp_p1
        p2    <- input$comp_p2
        alpha <- as.numeric(input$comp_alpha2)
        pot   <- as.numeric(input$comp_pot2)
        req(p1 > 0, p1 < 1, p2 > 0, p2 < 1, p1 != p2)

        z_a   <- qnorm(1 - alpha / 2)
        z_b   <- qnorm(pot)
        p_bar <- (p1 + p2) / 2
        num   <- (z_a * sqrt(2 * p_bar * (1 - p_bar)) +
                    z_b  * sqrt(p1 * (1 - p1) + p2 * (1 - p2)))^2
        n1    <- ceiling(num / (p1 - p2)^2)

        n_seq <- seq(2, max(n1 * 3, 30))
        ncp   <- abs(p1 - p2) / sqrt(2 * p_bar * (1 - p_bar) / n_seq)
        pot_v <- pnorm(ncp - z_a) + pnorm(-ncp - z_a)

        list(tipo = "comp_prop", n1 = n1, n2 = n1, total = n1 * 2,
             alpha = alpha, pot = pot, z_a = z_a, z_b = z_b,
             p1 = p1, p2 = p2, p_bar = p_bar,
             curv = data.frame(n = n_seq, potencia = pot_v))
      }
    })

    # ── Panel de resultados (switcher) ────────────────────
    output$panel_resultados <- renderUI({
      tipo <- calc_tipo()

      if (tipo %in% c("prop", "media")) {
        # Panel de estimación (igual que antes)
        div(
          layout_columns(
            col_widths = c(4, 4, 4),
            value_box(title = "Muestra base (n\u2080)", value = textOutput(ns("res_n0")),
                      showcase = bsicons::bs_icon("people-fill"), theme = "primary"),
            value_box(title = "Con corrección finita (n)", value = textOutput(ns("res_nc")),
                      showcase = bsicons::bs_icon("funnel-fill"), theme = "info"),
            value_box(title = "Fracción de muestreo", value = textOutput(ns("res_frac")),
                      showcase = bsicons::bs_icon("percent"), theme = "secondary")
          ),
          br(),
          card(card_header("Interpretación"),  uiOutput(ns("interpretacion"))),
          card(card_header("Guía pedagógica"), uiOutput(ns("pedagogico"))),
          card(card_header("Fórmula"),         uiOutput(ns("formula_detalle"))),
          card(
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tagList(bs_icon("code-slash"), " Código R reproducible"),
              downloadButton(ns("descargar_script"), label = "Descargar .R",
                             icon = bs_icon("download"), class = "btn-sm btn-outline-primary")
            ),
            p("Este script reproduce el cálculo con los parámetros actuales.",
              class = "text-muted small px-3 pt-2 mb-1"),
            verbatimTextOutput(ns("codigo_r"))
          )
        )

      } else {
        # Panel de comparación — inline directo
        r <- calc_comp()

        params_txt <- if (r$tipo == "comp_medias") {
          sprintf("\u03b4 = %.2f  ·  \u03c3 = %.2f  ·  \u03b1 = %.2f  ·  Potencia = %.0f%%",
                  r$delta, r$sigma, r$alpha, r$pot * 100)
        } else {
          sprintf("p\u2081 = %.2f  ·  p\u2082 = %.2f  ·  \u03b1 = %.2f  ·  Potencia = %.0f%%",
                  r$p1, r$p2, r$alpha, r$pot * 100)
        }

        formula_ui <- if (r$tipo == "comp_medias") {
          withMathJax(
            p("$$n = \\frac{2\\,(z_{\\alpha/2}+z_\\beta)^2\\,\\sigma^2}{\\delta^2}$$"),
            p(class = "small text-muted",
              "Donde \u03b4 = diferencia m\u00ednima a detectar, \u03c3 = desviaci\u00f3n est\u00e1ndar.")
          )
        } else {
          withMathJax(
            p("$$n = \\frac{\\left(z_{\\alpha/2}\\sqrt{2\\bar{p}(1-\\bar{p})} +
                  z_\\beta\\sqrt{p_1(1-p_1)+p_2(1-p_2)}\\right)^2}{(p_1-p_2)^2}$$"),
            p(class = "small text-muted",
              "Donde p\u0305 = (p\u2081 + p\u2082)/2.")
          )
        }

        div(
          layout_columns(
            col_widths = c(4, 4, 4),
            value_box(title = "n por grupo", value = format(r$n1, big.mark = ","),
                      showcase = bsicons::bs_icon("people-fill"), theme = "primary"),
            value_box(title = "Total (ambos grupos)", value = format(r$total, big.mark = ","),
                      showcase = bsicons::bs_icon("people"), theme = "info"),
            value_box(title = "Potencia objetivo",
                      value = paste0(round(r$pot * 100), "%"),
                      showcase = bsicons::bs_icon("lightning-charge-fill"), theme = "secondary")
          ),
          p(class = "text-muted small text-center mt-1 mb-3", params_txt),
          card(
            card_header("Curva de potencia"),
            p(class = "text-muted small px-3 pt-2",
              "Muestra c\u00f3mo crece la potencia al aumentar el n por grupo."),
            plotOutput(ns("plot_potencia"), height = "260px")
          ),
          card(card_header("F\u00f3rmula"), div(class = "px-3 py-2", formula_ui)),
          card(
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tagList(bs_icon("code-slash"), " C\u00f3digo R reproducible"),
              downloadButton(ns("descargar_comp"), label = "Descargar .R",
                             icon = bs_icon("download"), class = "btn-sm btn-outline-primary")
            ),
            p("Script reproducible con los par\u00e1metros actuales.",
              class = "text-muted small px-3 pt-2 mb-1"),
            verbatimTextOutput(ns("codigo_comp"))
          )
        )
      }
    })

    # ── Resultados: estimación ────────────────────────────
    output$res_n0   <- renderText({ req(es_estimacion()); format(calc()$n0, big.mark = ",") })
    output$res_nc   <- renderText({
      req(es_estimacion())
      if (is.na(calc()$nc)) "N/A" else format(calc()$nc, big.mark = ",")
    })
    output$res_frac <- renderText({ calc()$frac })

    # ── Resultados: comparación — gráfico y código ────────

    output$plot_potencia <- renderPlot({
      r <- calc_comp()
      curv  <- r$curv
      n_obj <- r$n1
      pot_t <- r$pot

      par(mar = c(4, 4, 1, 1), bg = "white", family = "sans")
      plot(curv$n, curv$potencia,
           type = "l", lwd = 2.5, col = colores$primario,
           xlab = "n por grupo", ylab = "Potencia (1 \u2212 \u03b2)",
           ylim = c(0, 1), las = 1,
           panel.first = {
             abline(h = seq(0, 1, 0.2), col = "grey92")
             abline(h = pot_t,  col = colores$acento,  lty = 2, lwd = 1.5)
             abline(v = n_obj,  col = colores$acento,  lty = 2, lwd = 1.5)
           })
      pot_en_n <- approx(curv$n, curv$potencia, xout = n_obj)$y
      points(n_obj, pot_en_n, pch = 21,
             bg = colores$acento, col = "white", cex = 1.8, lwd = 2)
      text(n_obj, pot_en_n,
           labels = sprintf("  n=%d\n  %.0f%%", n_obj, pot_en_n * 100),
           pos = 4, cex = 0.85, col = colores$primario)
    }, res = 110)

    codigo_comp_generado <- reactive({
      r <- calc_comp()
      encabezado <- encabezado_script("StatDesign", "Comparaci\u00f3n de grupos — tama\u00f1o de muestra")

      if (r$tipo == "comp_medias") {
        paste0(encabezado,
               "# Par\u00e1metros\n",
               "delta <- ", r$delta, "   # Diferencia m\u00ednima detectable\n",
               "sigma <- ", r$sigma, "   # Desviaci\u00f3n est\u00e1ndar esperada\n",
               "alpha <- ", r$alpha, "   # Nivel de significancia\n",
               "pot   <- ", r$pot,   "   # Potencia deseada\n\n",
               "# C\u00e1lculo\n",
               "z_alpha <- qnorm(1 - alpha / 2)\n",
               "z_beta  <- qnorm(pot)\n",
               "n <- ceiling(2 * (z_alpha + z_beta)^2 * sigma^2 / delta^2)\n",
               "n   # \u2192 ", r$n1, " por grupo (total: ", r$total, ")\n"
        )
      } else {
        paste0(encabezado,
               "# Par\u00e1metros\n",
               "p1    <- ", r$p1,    "   # Proporci\u00f3n grupo 1\n",
               "p2    <- ", r$p2,    "   # Proporci\u00f3n grupo 2\n",
               "alpha <- ", r$alpha, "   # Nivel de significancia\n",
               "pot   <- ", r$pot,   "   # Potencia deseada\n\n",
               "# C\u00e1lculo\n",
               "z_alpha <- qnorm(1 - alpha / 2)\n",
               "z_beta  <- qnorm(pot)\n",
               "p_bar   <- (p1 + p2) / 2\n",
               "num <- (z_alpha * sqrt(2 * p_bar * (1 - p_bar)) +\n",
               "        z_beta  * sqrt(p1 * (1 - p1) + p2 * (1 - p2)))^2\n",
               "n   <- ceiling(num / (p1 - p2)^2)\n",
               "n   # \u2192 ", r$n1, " por grupo (total: ", r$total, ")\n"
        )
      }
    })

    output$codigo_comp <- renderText({ codigo_comp_generado() })

    output$descargar_comp <- downloadHandler(
      filename = function() paste0("comparacion_", format(Sys.Date(), "%Y%m%d"), ".R"),
      content  = function(file) writeLines(codigo_comp_generado(), file)
    )

    output$formula_texto <- renderUI({
      switch(calc_tipo(),
             prop       = "n\u2080 = z\u00b2 \u00b7 p(1-p) / e\u00b2",
             media      = "n\u2080 = (z \u00b7 CV / e)\u00b2",
             comp_medias = "n = 2(z\u03b1+z\u03b2)\u00b2\u03c3\u00b2 / \u03b4\u00b2",
             comp_prop   = "n basado en p\u2081, p\u2082, \u03b1, \u03b2",
             NULL
      )
    })

    output$formula_detalle <- renderUI({
      req(es_estimacion())
      r <- calc()
      if (calc_tipo() == "prop") {
        withMathJax(
          p("$$n_0 = \\frac{z^2 \\cdot p(1-p)}{e^2}$$"),
          p(class = "small text-muted",
            "Donde z = valor crítico normal, p = proporción esperada, e = error máximo aceptable.")
        )
      } else {
        withMathJax(
          p("$$n_0 = \\left(\\frac{z \\cdot CV}{e}\\right)^2$$"),
          p(class = "small text-muted",
            "Donde CV = coeficiente de variación (decimal), e = error máximo aceptable.")
        )
      }
    })

    # ── Código R reproducible (estimación) ───────────────
    codigo_generado <- reactive({
      req(es_estimacion())
      r <- calc()

      encabezado <- encabezado_script("StatDesign", "Calculadora de tama\u00f1o de muestra")

      nivel_conf <- ifelse(abs(r$z - 1.96)  < 0.01, "95%",
                           ifelse(abs(r$z - 2.576) < 0.01, "99%", "90%"))

      cuerpo <- if (r$tipo == "prop") {
        paste0(
          "# Par\u00e1metros\n",
          "z <- ", r$z, "   # Nivel de confianza ", nivel_conf, "\n",
          "p <- ", r$p, "   # Proporci\u00f3n esperada\n",
          "e <- ", r$e, "   # Error m\u00e1ximo aceptable\n",
          if (r$N > 0) paste0("N <- ", r$N, "   # Tama\u00f1o de la poblaci\u00f3n\n") else "",
          "\n",
          "# Muestra base (f\u00f3rmula para proporciones)\n",
          "n0 <- ceiling((z^2 * p * (1 - p)) / e^2)\n",
          "n0  # \u2192 ", r$n0, "\n"
        )
      } else {
        paste0(
          "# Par\u00e1metros\n",
          "z  <- ", r$z, "   # Nivel de confianza ", nivel_conf, "\n",
          "cv <- ", round(r$cv / 100, 3), "   # Coeficiente de variaci\u00f3n (decimal)\n",
          "e  <- ", r$e, "   # Error m\u00e1ximo aceptable\n",
          if (r$N > 0) paste0("N  <- ", r$N, "   # Tama\u00f1o de la poblaci\u00f3n\n") else "",
          "\n",
          "# Muestra base (f\u00f3rmula para medias con CV)\n",
          "n0 <- ceiling((z * cv / e)^2)\n",
          "n0  # \u2192 ", r$n0, "\n"
        )
      }

      finita <- if (r$N > 0 && !is.na(r$nc)) {
        paste0(
          "\n",
          "# Correcci\u00f3n por poblaci\u00f3n finita\n",
          "nc <- ceiling(n0 / (1 + n0 / N))\n",
          "nc  # \u2192 ", r$nc, "\n",
          "\n",
          "# Fracci\u00f3n de muestreo\n",
          "round(nc / N * 100, 1)  # \u2192 ", r$frac, "\n"
        )
      } else if (r$N > 0 && r$N <= r$n0) {
        paste0(
          "\n",
          "# Nota: la muestra calculada (", r$n0, ") es >= la poblaci\u00f3n (", r$N, ").\n",
          "# Consider\u00e1 censar toda la poblaci\u00f3n en lugar de muestrear.\n"
        )
      } else {
        paste0(
          "\n",
          "# Para aplicar correcci\u00f3n por poblaci\u00f3n finita,\n",
          "# defin\u00ed N (tama\u00f1o de tu poblaci\u00f3n):\n",
          "# N  <- 500\n",
          "# nc <- ceiling(n0 / (1 + n0 / N))\n"
        )
      }

      paste0(encabezado, cuerpo, finita)
    })

    output$codigo_r <- renderText({
      codigo_generado()
    })

    output$descargar_script <- downloadHandler(
      filename = function() {
        paste0("muestra_", format(Sys.Date(), "%Y%m%d"), ".R")
      },
      content = function(file) {
        writeLines(codigo_generado(), file)
      }
    )

    output$interpretacion <- renderUI({
      req(es_estimacion())
      r <- calc()
      if (r$N > 0 && !is.na(r$nc) && r$nc < r$n0) {
        div(
          class = "wiz-result",
          p(sprintf(
            "Con una población finita de %s unidades, la corrección reduce
             la muestra de %s a %s unidades (fracción de muestreo: %s).",
            format(r$N,  big.mark = ","),
            format(r$n0, big.mark = ","),
            format(r$nc, big.mark = ","),
            r$frac
          ))
        )
      } else if (r$N > 0 && r$N <= r$n0) {
        div(
          class = "alert alert-warning",
          sprintf(
            "La muestra calculada (%s) es mayor o igual a la población (%s).
             Considera censar toda la población.",
            format(r$n0, big.mark = ","),
            format(r$N,  big.mark = ",")
          )
        )
      } else {
        p("Muestra calculada para población infinita o muy grande.
           Si conoces el tamaño de tu población, ingrésalo para aplicar
           la corrección por población finita.",
          class = "text-muted")
      }
    })

    # ── Card pedagógico ───────────────────────────────────
    output$pedagogico <- renderUI({
      req(es_estimacion())
      r <- calc()

      nivel_conf <- ifelse(abs(r$z - 1.96)   < 0.01, "95",
                           ifelse(abs(r$z - 2.576)  < 0.01, "99", "90"))

      # — Explicación del error máximo aceptable —
      txt_e <- if (r$tipo == "prop") {
        sprintf(
          "El error máximo aceptable (e = %.2f) es el margen de error a cada lado
           de tu estimación. Significa que la proporción estimada estará a ±%d puntos
           porcentuales del valor real, con un %s%% de confianza. Por ejemplo, si
           estimas que el 40%% de los sitios presenta cierta condición, el intervalo
           sería aproximadamente [%d%%, %d%%].",
          r$e,
          round(r$e * 100),
          nivel_conf,
          round((0.4 - r$e) * 100),
          round((0.4 + r$e) * 100)
        )
      } else {
        req(r$cv)
        sprintf(
          "El error máximo aceptable (e = %.2f) es un error relativo: la media
           estimada no diferirá del valor real en más de un %d%%. Con un CV de %d%%,
           esto implica que la variabilidad del sistema requiere al menos %s unidades
           para alcanzar esa precisión con un %s%% de confianza.",
          r$e,
          round(r$e * 100),
          round(r$cv),
          format(r$n0, big.mark = ","),
          nivel_conf
        )
      }

      # — Explicación de la fracción de muestreo —
      txt_frac <- if (r$N > 0) {
        n_usar   <- ifelse(is.na(r$nc), r$n0, r$nc)
        frac_num <- n_usar / r$N * 100
        if (frac_num >= 20) {
          sprintf(
            "La fracción de muestreo es alta (%s): estás cubriendo más del 20%% de
             la población. Esto activa la corrección por población finita, que reduce
             la muestra necesaria. Con poblaciones tan pequeñas relativas a la muestra,
             conviene evaluar si es factible censar a toda la población.",
            r$frac
          )
        } else if (frac_num >= 5) {
          sprintf(
            "La fracción de muestreo (%s) es moderada. La corrección por población
             finita tiene un efecto apreciable y reduce la muestra de %s a %s unidades
             sin perder precisión estadística. Esto puede representar un ahorro
             importante de recursos en campo.",
            r$frac,
            format(r$n0, big.mark = ","),
            format(r$nc, big.mark = ",")
          )
        } else {
          sprintf(
            "La fracción de muestreo es muy pequeña (%s): la población es mucho mayor
             que la muestra necesaria. En este caso la corrección por población finita
             tiene un efecto mínimo y la muestra base (n\u2080 = %s) es prácticamente
             la definitiva.",
            r$frac,
            format(r$n0, big.mark = ",")
          )
        }
      } else {
        "Ingresa el tamaño de tu población (N) para ver el efecto de la corrección
         por población finita y la fracción de muestreo correspondiente."
      }

      # — Recomendación según precisión —
      txt_rec <- if (r$tipo == "prop") {
        if (r$e <= 0.03) {
          "Con e \u2264 0.03 tienes alta precisión, adecuada para estudios donde
           pequeñas diferencias son ecológicamente relevantes (p.ej. prevalencia
           de enfermedades, cobertura de especies raras)."
        } else if (r$e <= 0.05) {
          "Con e = 0.05 (\u00b15%) tienes la precisión estándar recomendada para
           la mayoría de estudios en ciencias ambientales y recursos naturales."
        } else {
          "Con e > 0.05 la precisión es baja. Es aceptable en estudios exploratorios
           o con recursos limitados, pero los resultados deben interpretarse con cautela."
        }
      } else {
        if (r$e <= 0.10) {
          "Con e \u2264 0.10 (10%) tienes buena precisión para estimar medias continuas.
           Es el estándar en inventarios forestales y estudios de biomasa."
        } else {
          "Con e > 0.10 la estimación de la media tiene baja precisión. Considera
           reducir el error o aumentar el esfuerzo de muestreo si la variable es
           crítica para la toma de decisiones."
        }
      }

      div(
        p(txt_e,    class = "small mb-3"),
        hr(class = "my-2"),
        p(txt_frac, class = "small mb-3"),
        hr(class = "my-2"),
        div(
          class = "wiz-result",
          p(strong("\U0001f4a1 Recomendación: "), txt_rec, class = "small mb-0")
        )
      )
    })

    # ── Ejercicio MAS ─────────────────────────────────────

    poblacion_rv <- reactiveVal(NULL)
    muestra_rv   <- reactiveVal(NULL)
    n_muestreo   <- reactiveVal(0)   # contador de muestreos para variar seed

    # Generar población cuando cambian parámetros
    observeEvent(
      list(input$ej_N, input$ej_media_pob, input$ej_sd_pob, input$ej_seed),
      {
        N     <- input$ej_N
        media <- input$ej_media_pob
        sd    <- input$ej_sd_pob
        seed  <- input$ej_seed
        req(N > 0, media > 0, sd > 0)
        set.seed(seed)
        dap <- round(rnorm(N, mean = media, sd = sd), 1)
        dap <- pmax(dap, 1)
        poblacion_rv(data.frame(id = seq_len(N), dap = dap, seleccionado = FALSE))
        muestra_rv(NULL)
        n_muestreo(0)
      },
      ignoreNULL = FALSE
    )

    # Tomar muestra
    observeEvent(input$ej_muestrear, {
      pob <- poblacion_rv()
      req(pob)
      n   <- min(input$ej_n, nrow(pob))
      req(n >= 2)
      cnt <- n_muestreo() + 1
      set.seed(input$ej_seed * 100 + cnt)
      idx <- sample(nrow(pob), n)
      pob$seleccionado        <- FALSE
      pob$seleccionado[idx]   <- TRUE
      poblacion_rv(pob)
      muestra_rv(pob[idx, ])
      n_muestreo(cnt)
    })

    # Reiniciar
    observeEvent(input$ej_resetear, {
      N     <- input$ej_N
      media <- input$ej_media_pob
      sd    <- input$ej_sd_pob
      seed  <- input$ej_seed
      req(N > 0)
      set.seed(seed)
      dap <- round(rnorm(N, mean = media, sd = sd), 1)
      dap <- pmax(dap, 1)
      poblacion_rv(data.frame(id = seq_len(N), dap = dap, seleccionado = FALSE))
      muestra_rv(NULL)
      n_muestreo(0)
    })

    # Métricas
    output$ej_metricas <- renderUI({
      m <- muestra_rv()

      if (is.null(m)) {
        return(
          div(
            class = "alert alert-info d-flex align-items-center gap-2",
            bs_icon("info-circle-fill"),
            span("Configurá los parámetros y presioná ",
                 strong("Tomar muestra"), " para ver los resultados.")
          )
        )
      }

      n     <- nrow(m)
      N     <- input$ej_N
      x_bar <- mean(m$dap)
      s     <- sd(m$dap)
      ee    <- s / sqrt(n)
      # Error estándar con corrección por población finita
      fpc   <- sqrt((N - n) / (N - 1))
      ee_c  <- ee * fpc
      ic_lo <- x_bar - qt(0.975, df = n - 1) * ee
      ic_hi <- x_bar + qt(0.975, df = n - 1) * ee

      frac_pct <- n / N * 100
      dif_pct  <- (1 - fpc) * 100

      fpc_info <- if (frac_pct < 5) {
        list(
          clase = "alert-success",
          icono = bs_icon("check-circle-fill"),
          texto = sprintf(
            "Fracción de muestreo: %.1f%% (n/N = %d/%d). La corrección por población finita tiene efecto mínimo (FPC = %.4f, reducción del EE: %.1f%%). Con fracciones menores al 5%%, el EE sin corrección (%.3f cm) y con corrección (%.3f cm) son prácticamente iguales.",
            frac_pct, n, N, fpc, dif_pct, ee, ee_c
          )
        )
      } else if (frac_pct < 20) {
        list(
          clase = "alert-warning",
          icono = bs_icon("exclamation-triangle-fill"),
          texto = sprintf(
            "Fracción de muestreo: %.1f%% (n/N = %d/%d). La corrección por población finita tiene efecto apreciable (FPC = %.4f, reducción del EE: %.1f%%). El EE sin corrección sería %.3f cm; con la corrección es %.3f cm. Se recomienda aplicarla cuando la fracción supera el 5%%.",
            frac_pct, n, N, fpc, dif_pct, ee, ee_c
          )
        )
      } else {
        list(
          clase = "alert-danger",
          icono = bs_icon("exclamation-octagon-fill"),
          texto = sprintf(
            "Fracción de muestreo alta: %.1f%% (n/N = %d/%d). La corrección reduce sustancialmente el EE (FPC = %.4f, reducción: %.1f%%). EE sin corrección: %.3f cm → con corrección: %.3f cm. Con fracciones tan altas, conviene evaluar si es factible censar toda la población.",
            frac_pct, n, N, fpc, dif_pct, ee, ee_c
          )
        )
      }

      tagList(
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          value_box(
            title    = "Media muestral (\u0078\u0305)",
            value    = paste0(round(x_bar, 2), " cm"),
            showcase = bs_icon("rulers"),
            theme    = "primary"
          ),
          value_box(
            title    = "Desvío estándar (s)",
            value    = paste0(round(s, 2), " cm"),
            showcase = bs_icon("distribute-vertical"),
            theme    = value_box_theme(bg = colores$secundario)
          ),
          value_box(
            title    = "Error estándar (EE)",
            value    = paste0(round(ee_c, 3), " cm"),
            showcase = bs_icon("bullseye"),
            theme    = value_box_theme(bg = colores$acento)
          ),
          value_box(
            title    = "IC 95%",
            value    = paste0("[", round(ic_lo, 1), " \u2013 ", round(ic_hi, 1), "]"),
            showcase = bs_icon("arrows-expand"),
            theme    = value_box_theme(bg = "#5FA2CE")
          )
        ),
        div(
          class = paste("d-flex align-items-start gap-2 mt-3 small alert", fpc_info$clase),
          fpc_info$icono,
          p(fpc_info$texto, class = "mb-0")
        )
      )
    })

    # Gráfico
    output$ej_plot <- renderPlot({
      pob <- poblacion_rv()
      m   <- muestra_rv()
      req(pob)

      p <- ggplot(pob, aes(x = dap)) +
        geom_histogram(
          fill  = colores$borde,
          color = "white",
          bins  = 30,
          alpha = 0.9
        ) +
        labs(x = "DAP (cm)", y = "Frecuencia") +
        theme_minimal(base_size = 13) +
        theme(
          panel.grid.minor  = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.background   = element_rect(fill = "#FFFFFF", color = NA)
        )

      if (!is.null(m)) {
        p <- p +
          geom_histogram(
            data  = m, aes(x = dap),
            fill  = colores$primario,
            color = "white",
            bins  = 30,
            alpha = 0.85
          ) +
          geom_vline(
            xintercept = mean(m$dap),
            color      = colores$acento,
            linewidth  = 1.2,
            linetype   = "dashed"
          ) +
          annotate(
            "label",
            x     = mean(m$dap),
            y     = Inf,
            label = paste0("\u0078\u0305 = ", round(mean(m$dap), 1), " cm"),
            vjust = 1.5, hjust = -0.1,
            color      = colores$acento,
            fill       = "white",
            label.size = 0.3,
            size       = 4
          )

        # Leyenda manual
        p <- p +
          annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
                   fill = NA, color = NA) +
          labs(
            subtitle = paste0(
              "Población (gris, N = ", nrow(pob), ")   |   ",
              "Muestra (azul, n = ", nrow(m), ")"
            )
          ) +
          theme(plot.subtitle = element_text(color = colores$texto, size = 11))
      }

      p
    }, res = 110)

    # Tabla
    output$ej_tabla_wrap <- renderUI({
      m <- muestra_rv()
      if (is.null(m)) {
        return(p("Aún no se tomó ninguna muestra.", class = "text-muted small"))
      }
      tbl <- m[order(m$id), c("id", "dap")]
      colnames(tbl) <- c("ID árbol", "DAP (cm)")
      DT::dataTableOutput(ns("ej_tabla"))
    })

    output$ej_tabla <- DT::renderDataTable({
      m <- muestra_rv()
      req(m)
      tbl <- m[order(m$id), c("id", "dap")]
      colnames(tbl) <- c("ID árbol", "DAP (cm)")
      DT::datatable(
        tbl,
        options  = list(pageLength = 10, dom = "tp", scrollX = TRUE),
        rownames = FALSE,
        class    = "compact stripe"
      )
    })

    # ── Esquema visual ────────────────────────────────────
    output$esquema_plot <- renderPlot({
      esquema_ggplot(input$esquema_sel, colores)
    }, res = 110)

    output$esquema_detalle <- renderUI({
      claves <- c("alea_simple", "alea_estrat", "sistematico", "conglomerados",
                  "conveniencia", "intencional",
                  "transectos", "parcelas", "puntos", "adaptativo")
      todos  <- unlist(lapply(diseños_muestreo, function(g) g$items), recursive = FALSE)
      item   <- todos[[which(claves == input$esquema_sel)]]

      div(
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header("Descripción"),
            p(item$desc, class = "text-muted small"),
            tags$dl(
              class = "row small mb-0",
              tags$dt(class = "col-sm-4", "¿Cuándo usar?"),
              tags$dd(class = "col-sm-8", item$cuando),
              tags$dt(class = "col-sm-4", "Ventaja"),
              tags$dd(class = "col-sm-8", item$ventaja),
              tags$dt(class = "col-sm-4", "Limitación"),
              tags$dd(class = "col-sm-8", item$limitacion)
            )
          ),
          card(
            card_header("Ejemplo"),
            div(
              class = "wiz-result",
              p(item$ejemplo, class = "mb-0 fst-italic small")
            )
          )
        )
      )
    })

  })
}

# ── Helper interno ────────────────────────────────────────
badge_label <- function(badge) {
  switch(badge,
         prob     = "Probabilístico",
         noprob   = "No probabilístico",
         espacial = "Espacial"
  )
}
