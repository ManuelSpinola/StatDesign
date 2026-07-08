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
    celdas <- construir_grilla(8, 8)
    sel    <- sample(64, 16)
    pts <- do.call(rbind, lapply(sel, function(qid) {
      cx <- celdas$x0[celdas$qid == qid]
      cy <- celdas$y0[celdas$qid == qid]
      data.frame(x = cx + runif(2, 0.15, 1.1), y = cy + runif(2, 0.15, 1.1))
    }))
    p <- agregar_celdas_grilla(base, celdas, sel, colores)
    p +
      geom_point(data = pts, aes(x, y),
                 color = colores$primario, size = 3, alpha = 0.9) +
      labs(title = "Aleatorio simple — 16 de 64 cuadrantes seleccionados al azar")

  } else if (tipo == "alea_estrat") {
    celdas <- construir_grilla(8, 8)
    zonas <- list(
      list(nombre = "A", cols = 1:3, color = colores$primario),
      list(nombre = "B", cols = 4:5, color = colores$acento),
      list(nombre = "C", cols = 6:8, color = colores$secundario)
    )
    sel_por_zona <- lapply(zonas, function(z) {
      ids <- celdas$qid[celdas$qx %in% z$cols]
      sample(ids, 2)
    })
    sel <- unlist(sel_por_zona)

    p <- base
    for (z in zonas) {
      xmin <- (min(z$cols) - 1) * 1.25
      xmax <- max(z$cols) * 1.25
      p <- p +
        annotate("rect", xmin = xmin, xmax = xmax, ymin = 0, ymax = 10,
                 fill = z$color, alpha = 0.07) +
        annotate("text", x = (xmin + xmax) / 2, y = 9.7, label = z$nombre,
                 size = 4, fontface = "bold", color = z$color)
    }
    p <- agregar_celdas_grilla(p, celdas, sel, colores)

    pts <- do.call(rbind, lapply(sel_por_zona, function(ids) {
      do.call(rbind, lapply(ids, function(qid) {
        cx <- celdas$x0[celdas$qid == qid]
        cy <- celdas$y0[celdas$qid == qid]
        data.frame(x = cx + runif(2, 0.15, 1.1), y = cy + runif(2, 0.15, 1.1))
      }))
    }))

    p +
      geom_point(data = pts, aes(x, y),
                 color = colores$primario, size = 3, alpha = 0.9) +
      labs(title = "Aleatorio estratificado — 2 cuadrantes seleccionados por estrato")

  } else if (tipo == "sistematico") {
    celdas <- construir_grilla(8, 8)
    sel <- celdas$qid[celdas$qx %in% c(2, 4, 6, 8) & celdas$qy %in% c(1, 3, 5, 7)]
    pts <- do.call(rbind, lapply(sel, function(qid) {
      cx <- celdas$x0[celdas$qid == qid]
      cy <- celdas$y0[celdas$qid == qid]
      data.frame(x = cx + runif(2, 0.15, 1.1), y = cy + runif(2, 0.15, 1.1))
    }))
    p <- agregar_celdas_grilla(base, celdas, sel, colores)
    p +
      geom_point(data = pts, aes(x, y),
                 color = colores$primario, size = 3, alpha = 0.9) +
      labs(title = "Sistemático — cuadrantes en patrón regular con inicio aleatorio")

  } else if (tipo == "conglomerados") {
    celdas <- construir_grilla(4, 4)
    sel <- sample(16, 4)
    pts <- do.call(rbind, lapply(seq_along(sel), function(i) {
      qid <- sel[i]
      cx  <- celdas$x0[celdas$qid == qid]
      cy  <- celdas$y0[celdas$qid == qid]
      n   <- sample(5:8, 1)
      data.frame(x = cx + runif(n, 0.2, 2.3), y = cy + runif(n, 0.2, 2.3),
                 cong = paste("Conglomerado", i))
    }))
    p <- agregar_celdas_grilla(base, celdas, sel, colores)
    p +
      geom_point(data = pts, aes(x, y, color = cong), size = 3, alpha = 0.9) +
      scale_color_tableau_cb() +
      labs(title = "Por conglomerados — 4 de 16 bloques seleccionados, todo medido dentro")

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
      data.frame(x = seq(1.5, 8.5, length.out = 6), y = yt + rnorm(6, 0, 0.15))
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
# ── Helpers: ejercicios interactivos — muestreo por cuadrantes ──
# La grilla es la unidad de muestreo real (no solo un dibujo de fondo): el
# área de estudio se divide en cuadrantes fijos; los árboles se dispersan de
# forma natural dentro de ella (no en filas), y lo que se selecciona en cada
# diseño son cuadrantes completos — se mide cada árbol dentro de ellos.
# `ancho`/`alto` permiten áreas rectangulares (no solo cuadradas) para que
# cada diseño pueda usar el espacio horizontal disponible sin deformar el
# tamaño de los cuadrantes (que siempre se calculan como celdas cuadradas).

generar_poblacion_natural <- function(N, media, sd, seed, xr = c(0, 10), yr = c(0, 10)) {
  set.seed(seed)
  data.frame(
    id = seq_len(N),
    dap = pmax(round(rnorm(N, mean = media, sd = sd), 1), 1),
    x = runif(N, xr[1] + 0.1, xr[2] - 0.1),
    y = runif(N, yr[1] + 0.1, yr[2] - 0.1),
    color_base = colores$secundario,
    seleccionado = FALSE,
    stringsAsFactors = FALSE
  )
}

construir_grilla <- function(ncol, nrow, ancho = 10, alto = 10) {
  cw <- ancho / ncol
  ch <- alto / nrow
  celdas <- expand.grid(qx = seq_len(ncol), qy = seq_len(nrow))
  celdas$qid <- (celdas$qy - 1) * ncol + celdas$qx
  celdas$x0  <- (celdas$qx - 1) * cw
  celdas$x1  <- celdas$qx * cw
  celdas$y0  <- (celdas$qy - 1) * ch
  celdas$y1  <- celdas$qy * ch
  celdas
}

asignar_cuadrante <- function(x, y, ncol, nrow, ancho = 10, alto = 10) {
  cw <- ancho / ncol
  ch <- alto / nrow
  qx <- pmin(ncol, pmax(1, ceiling(x / cw)))
  qy <- pmin(nrow, pmax(1, ceiling(y / ch)))
  (qy - 1) * ncol + qx
}

agregar_celdas_grilla <- function(p, celdas, sel_ids, colores) {
  p <- p + geom_rect(data = celdas, aes(xmin = x0, xmax = x1, ymin = y0, ymax = y1),
                      fill = NA, color = colores$borde, linewidth = 0.3)
  if (!is.null(sel_ids) && length(sel_ids) > 0) {
    sel_celdas <- celdas[celdas$qid %in% sel_ids, ]
    p <- p + geom_rect(data = sel_celdas,
                        aes(xmin = x0, xmax = x1, ymin = y0, ymax = y1),
                        fill = colores$acento, alpha = 0.12,
                        color = colores$acento, linewidth = 0.9)
  }
  p
}

agregar_puntos_grilla <- function(p, pob, colores, xlim = c(0, 10), ylim = c(0, 10)) {
  pob$plot_color <- ifelse(pob$seleccionado, colores$acento, pob$color_base)
  pob$plot_alpha <- ifelse(pob$seleccionado, 1, 0.75)
  pob$plot_size  <- ifelse(pob$seleccionado, 4, 2.8)

  p <- p + geom_point(data = pob, aes(x = x, y = y),
                       color = pob$plot_color, alpha = pob$plot_alpha, size = pob$plot_size)

  p +
    coord_fixed(xlim = xlim, ylim = ylim, clip = "off") +
    theme_light(base_size = 12) +
    theme(
      panel.grid      = element_blank(),
      axis.text       = element_blank(),
      axis.title      = element_blank(),
      axis.ticks      = element_blank(),
      panel.border    = element_blank(),
      plot.background = element_rect(fill = "#FFFFFF", color = NA),
      legend.position = "none"
    )
}

# ── UI ────────────────────────────────────────────────────
mod_muestreo_ui <- function(id) {
  ns <- NS(id)

  bslib::nav_panel(
    title = "Diseños de muestreo",

    bslib::navset_card_tab(

      # ── Pestaña 0: Conceptos básicos ───────────────────
      bslib::nav_panel(
        title = tagList(bsicons::bs_icon("book", class = "me-1"), "Conceptos básicos"),

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

          bslib::card(
            bslib::card_body(
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

          bslib::card(
            bslib::card_body(
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

          bslib::layout_columns(
            col_widths = c(4, 4, 4, 4, 4),
            gap = "8px",

            bslib::card(
              class = "h-100",
              bslib::card_header(bsicons::bs_icon("bullseye"), " Población objetivo"),
              bslib::card_body(
                p(class = "small mb-0",
                  "La población sobre la cual se quieren hacer inferencias.
                   Debe estar claramente definida antes de diseñar el muestreo.")
              )
            ),
            bslib::card(
              class = "h-100",
              bslib::card_header(bsicons::bs_icon("grid-3x3"), " Marco de muestreo"),
              bslib::card_body(
                p(class = "small mb-0",
                  "Lista o mapa de todas las unidades de muestreo disponibles.
                   Define la población muestreada — la que queda dentro del marco.")
              )
            ),
            bslib::card(
              class = "h-100",
              bslib::card_header(bsicons::bs_icon("square"), " Unidad de muestreo"),
              bslib::card_body(
                p(class = "small mb-0",
                  "La unidad que se selecciona del marco: una parcela, un transecto,
                   un individuo. En diseños simples ", strong("coincide con el elemento"),
                  "; solo difieren en muestreo por conglomerados.")
              )
            ),
            bslib::card(
              class = "h-100",
              bslib::card_header(bsicons::bs_icon("dot"), " Elemento"),
              bslib::card_body(
                p(class = "small mb-0",
                  "El objeto sobre el que se toma la medición: un árbol, un individuo,
                   una observación. Frecuentemente es la misma entidad que la unidad
                   de muestreo.")
              )
            ),
            bslib::card(
              class = "h-100",
              bslib::card_header(bsicons::bs_icon("collection"), " Muestra"),
              bslib::card_body(
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

          bslib::layout_columns(
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
      bslib::nav_panel(
        title = tagList(bsicons::bs_icon("grid-3x3-gap", class = "me-1"), "Diseños de muestreo"),

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
      bslib::nav_panel(
        title = tagList(bsicons::bs_icon("image", class = "me-1"), "Esquema visual"),

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
              bslib::card(
                bslib::card_header("Representación espacial"),
                plotOutput(ns("esquema_plot"), height = "320px")
              ),

              br(),

              uiOutput(ns("esquema_detalle"))
            )
          )
        )
      ),

      # ── Pestaña 3: Calculadora ──────────────────────────
      bslib::nav_panel(
        title = tagList(bsicons::bs_icon("calculator", class = "me-1"), "Calculadora de tamaño de muestra"),

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

bslib::nav_panel(
  title = tagList(bsicons::bs_icon("play-circle", class = "me-1"), "Ejercicio"),

  div(
    class = "p-3",

    p(
      "Ejercicios interactivos sobre una población simulada de árboles con
       valores de DAP (diámetro a la altura del pecho, cm), dispersos de forma
       natural en un área de estudio. En los 4 diseños, la unidad de muestreo
       es el cuadrante: el área se divide en una grilla fija y lo que se
       selecciona son cuadrantes completos — se mide cada árbol dentro de
       ellos, igual que en un inventario forestal real.",
      class = "text-muted mb-4"
    ),

    bslib::navset_card_tab(
      id = ns("ejercicio_tipo"),

      # ── Sub-pestaña: Aleatorio simple (MAS) ──
      bslib::nav_panel(
        title    = "Aleatorio simple",
        value    = "mas",
        fillable = FALSE,
        div(
          class = "pt-3",

          p(class = "text-muted small mb-3",
            "Muestreo aleatorio simple de cuadrantes: cada uno de los 64
             cuadrantes de la grilla tiene igual probabilidad de ser
             seleccionado, sin importar su posición."),

          bslib::layout_columns(
            col_widths = c(3, 9),

            bslib::card(
              fill = FALSE, full_screen = FALSE,
              bslib::card_header(
                tagList(bsicons::bs_icon("sliders", class = "me-1"), "Parámetros del ejercicio")
              ),
              bslib::card_body(
                numericInput(ns("ej_N"), "Árboles en la población (N)",
                             value = 130, min = 60, max = 250, step = 10),
                numericInput(ns("ej_nq"), "Cuadrantes a muestrear (de 64)",
                             value = 15, min = 1, max = 64, step = 1),
                hr(),
                numericInput(ns("ej_media_pob"), "Media DAP población (cm)",
                             value = 25, min = 5, max = 100, step = 1),
                numericInput(ns("ej_sd_pob"), "Desvío estándar población (cm)",
                             value = 8, min = 1, max = 40, step = 1),
                numericInput(ns("ej_seed"), "Semilla aleatoria",
                             value = 42, min = 1, max = 9999, step = 1),
                hr(),
                actionButton(ns("ej_muestrear"), label = "Tomar muestra",
                             class = "btn-primary w-100", icon = icon("shuffle")),
                br(), br(),
                actionButton(ns("ej_resetear"), label = "Reiniciar",
                             class = "btn-outline-secondary w-100", icon = icon("rotate-left")),
                br(), br(),
                div(class = "small text-muted", bsicons::bs_icon("info-circle"), " ",
                    "El área se divide en una grilla de 8 × 8 = 64 cuadrantes.
                     Cada clic en «Tomar muestra» sortea cuadrantes distintos.")
              )
            ),

            div(
              uiOutput(ns("ej_metricas")),
              br(),
              bslib::card(
                fill = FALSE, full_screen = FALSE,
                bslib::card_header(
                  tagList(bsicons::bs_icon("map", class = "me-1"), "Cuadrantes y árboles")
                ),
                plotOutput(ns("ej_plot"), height = "400px"),
                div(class = "px-3 pb-2 small text-muted",
                    "Cuadrante resaltado = seleccionado en la muestra · todos los árboles dentro se miden.")
              ),
              br(),
              bslib::card(
                fill = FALSE, full_screen = FALSE,
                bslib::card_header(
                  tagList(bsicons::bs_icon("table", class = "me-1"),
                          "Árboles medidos en la muestra")
                ),
                bslib::card_body(uiOutput(ns("ej_tabla_wrap")))
              )
            )
          )
        )
      ),

      # ── Sub-pestaña: Estratificado ──
      bslib::nav_panel(
        title    = "Estratificado",
        value    = "estrat",
        fillable = FALSE,
        div(
          class = "pt-3",

          p(class = "text-muted small mb-3",
            "La grilla se divide en 3 estratos (bosque primario, bosque
             secundario y pastizal), cada uno con su propia cantidad de
             cuadrantes disponibles. Definí cuántos cuadrantes muestrear en
             cada estrato y observá cómo se combina la estimación."),

          bslib::layout_columns(
            col_widths = c(3, 9),

            bslib::card(
              fill = FALSE, full_screen = FALSE,
              bslib::card_header(
                tagList(bsicons::bs_icon("sliders", class = "me-1"), "Parámetros")
              ),
              bslib::card_body(
                numericInput(ns("ej_estrat_seed"), "Semilla aleatoria",
                             value = 42, min = 1, max = 9999, step = 1),
                hr(),
                div(class = "small fw-semibold mb-2",
                    style = paste0("color:", colores$primario),
                    "Cuadrantes a muestrear por estrato"),
                numericInput(ns("ej_estrat_nA"), "Bosque primario — 40 cuadrantes disponibles",
                             value = 8, min = 1, max = 40, step = 1),
                numericInput(ns("ej_estrat_nB"), "Bosque secundario — 32 cuadrantes disponibles",
                             value = 6, min = 1, max = 32, step = 1),
                numericInput(ns("ej_estrat_nC"), "Pastizal — 40 cuadrantes disponibles",
                             value = 8, min = 1, max = 40, step = 1),
                actionButton(ns("ej_estrat_prop"), "Asignación proporcional",
                             class = "btn-outline-secondary w-100 btn-sm",
                             icon = icon("balance-scale")),
                hr(),
                actionButton(ns("ej_estrat_muestrear"), "Tomar muestra",
                             class = "btn-primary w-100", icon = icon("shuffle")),
                br(), br(),
                actionButton(ns("ej_estrat_resetear"), "Reiniciar",
                             class = "btn-outline-secondary w-100", icon = icon("rotate-left"))
              )
            ),

            div(
              uiOutput(ns("ej_estrat_metricas")),
              br(),
              bslib::card(
                fill = FALSE, full_screen = FALSE,
                bslib::card_header(
                  tagList(bsicons::bs_icon("map", class = "me-1"),
                          "Cuadrantes por estrato")
                ),
                plotOutput(ns("ej_estrat_plot"), height = "400px"),
                div(class = "px-3 pb-2 small text-muted",
                    "Cuadrante resaltado = seleccionado en la muestra · todos los árboles dentro se miden.")
              ),
              br(),
              bslib::card(
                fill = FALSE, full_screen = FALSE,
                bslib::card_header(
                  tagList(bsicons::bs_icon("table", class = "me-1"), "Árboles medidos")
                ),
                bslib::card_body(uiOutput(ns("ej_estrat_tabla_wrap")))
              )
            )
          )
        )
      ),

      # ── Sub-pestaña: Sistemático ──
      bslib::nav_panel(
        title    = "Sistemático",
        value    = "sist",
        fillable = FALSE,
        div(
          class = "pt-3",

          p(class = "text-muted small mb-3",
            "El diseño sistemático elige cuadrantes a intervalos regulares
             sobre la misma grilla de 8 × 8, con un punto de inicio aleatorio
             — así se ubican los transectos en inventarios forestales reales.
             Activá el patrón periódico para ver qué pasa cuando el espaciado
             de la grilla coincide con un ciclo real de la población."),

          bslib::layout_columns(
            col_widths = c(3, 9),

            bslib::card(
              fill = FALSE, full_screen = FALSE,
              bslib::card_header(
                tagList(bsicons::bs_icon("sliders", class = "me-1"), "Parámetros")
              ),
              bslib::card_body(
                numericInput(ns("ej_sist_N"), "Árboles en la población (N)",
                             value = 130, min = 60, max = 250, step = 10),
                numericInput(ns("ej_sist_nq"), "Cuadrantes deseados (aprox.)",
                             value = 12, min = 4, max = 32, step = 1),
                hr(),
                numericInput(ns("ej_sist_media_pob"), "Media DAP población (cm)",
                             value = 25, min = 5, max = 100, step = 1),
                numericInput(ns("ej_sist_sd_pob"), "Desvío estándar población (cm)",
                             value = 8, min = 1, max = 40, step = 1),
                numericInput(ns("ej_sist_seed"), "Semilla aleatoria",
                             value = 42, min = 1, max = 9999, step = 1),
                hr(),
                checkboxInput(ns("ej_sist_periodico"),
                              "Simular patrón periódico en la población",
                              value = FALSE),
                uiOutput(ns("ej_sist_periodo_ui")),
                hr(),
                actionButton(ns("ej_sist_muestrear"), "Tomar muestra",
                             class = "btn-primary w-100", icon = icon("shuffle")),
                br(), br(),
                actionButton(ns("ej_sist_resetear"), "Reiniciar",
                             class = "btn-outline-secondary w-100", icon = icon("rotate-left"))
              )
            ),

            div(
              uiOutput(ns("ej_sist_metricas")),
              br(),
              bslib::card(
                fill = FALSE, full_screen = FALSE,
                bslib::card_header(
                  tagList(bsicons::bs_icon("map", class = "me-1"),
                          "Cuadrantes en patrón regular")
                ),
                plotOutput(ns("ej_sist_plot"), height = "400px"),
                div(class = "px-3 pb-2 small text-muted",
                    "Cuadrante resaltado = seleccionado en la muestra · todos los árboles dentro se miden.")
              ),
              br(),
              bslib::card(
                fill = FALSE, full_screen = FALSE,
                bslib::card_header(
                  tagList(bsicons::bs_icon("table", class = "me-1"), "Árboles medidos")
                ),
                bslib::card_body(uiOutput(ns("ej_sist_tabla_wrap")))
              )
            )
          )
        )
      ),

      # ── Sub-pestaña: Por conglomerados ──
      bslib::nav_panel(
        title    = "Por conglomerados",
        value    = "congl",
        fillable = FALSE,
        div(
          class = "pt-3",

          p(class = "text-muted small mb-3",
            "La población está dividida en una grilla más gruesa de 4 × 4 = 16
             conglomerados. Al muestrear se sortean conglomerados completos y
             se mide cada árbol dentro de ellos — no todos tendrán la misma
             cantidad de árboles, así que compará los dos estimadores."),

          bslib::layout_columns(
            col_widths = c(3, 9),

            bslib::card(
              fill = FALSE, full_screen = FALSE,
              bslib::card_header(
                tagList(bsicons::bs_icon("sliders", class = "me-1"), "Parámetros")
              ),
              bslib::card_body(
                numericInput(ns("ej_congl_N"), "Árboles en la población (N)",
                             value = 100, min = 40, max = 200, step = 10),
                numericInput(ns("ej_congl_m"), "Conglomerados a muestrear (de 16)",
                             value = 4, min = 1, max = 8, step = 1),
                hr(),
                numericInput(ns("ej_congl_media_pob"), "Media DAP población (cm)",
                             value = 25, min = 5, max = 100, step = 1),
                numericInput(ns("ej_congl_sd_pob"), "Desvío estándar población (cm)",
                             value = 8, min = 1, max = 40, step = 1),
                numericInput(ns("ej_congl_seed"), "Semilla aleatoria",
                             value = 42, min = 1, max = 9999, step = 1),
                hr(),
                actionButton(ns("ej_congl_muestrear"), "Tomar muestra",
                             class = "btn-primary w-100", icon = icon("shuffle")),
                br(), br(),
                actionButton(ns("ej_congl_resetear"), "Reiniciar",
                             class = "btn-outline-secondary w-100", icon = icon("rotate-left"))
              )
            ),

            div(
              uiOutput(ns("ej_congl_metricas")),
              br(),
              bslib::card(
                fill = FALSE, full_screen = FALSE,
                bslib::card_header(
                  tagList(bsicons::bs_icon("map", class = "me-1"),
                          "Conglomerados en el área de estudio")
                ),
                plotOutput(ns("ej_congl_plot"), height = "400px"),
                div(class = "px-3 pb-2 small text-muted",
                    "Conglomerado resaltado = seleccionado en la muestra · todos los árboles dentro se miden.")
              ),
              br(),
              bslib::layout_columns(
                col_widths = c(6, 6),
                bslib::card(
                  fill = FALSE, full_screen = FALSE,
                  bslib::card_header(
                    tagList(bsicons::bs_icon("diagram-3-fill", class = "me-1"), "Detalle por conglomerado")
                  ),
                  bslib::card_body(uiOutput(ns("ej_congl_detalle_wrap")))
                ),
                bslib::card(
                  fill = FALSE, full_screen = FALSE,
                  bslib::card_header(
                    tagList(bsicons::bs_icon("calculator", class = "me-1"), "Fórmulas de los estimadores")
                  ),
                  bslib::card_body(
                    withMathJax(
                      p(class = "small mb-2", "Media de medias:"),
                      p("$$\\bar{y} = \\frac{1}{m}\\sum_{i=1}^{m} \\bar{y}_i$$"),
                      p(class = "small mb-2 mt-2", "Razón combinada:"),
                      p("$$\\bar{y}_r = \\frac{\\sum_{i=1}^{m} \\sum_j y_{ij}}{\\sum_{i=1}^{m} n_i}$$"),
                      p(class = "small text-muted mb-0",
                        "Donde m = conglomerados muestreados, \\(\\bar{y}_i\\) = media del
                         conglomerado i, y \\(n_i\\) = árboles medidos en el conglomerado i.")
                    )
                  )
                )
              ),
              br(),
              bslib::card(
                fill = FALSE, full_screen = FALSE,
                bslib::card_header(
                  tagList(bsicons::bs_icon("table", class = "me-1"), "Árboles medidos")
                ),
                bslib::card_body(uiOutput(ns("ej_congl_tabla_wrap")))
              )
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
mod_muestreo_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Renderizar cards de diseños ──────────────────────
    render_cards <- function(grupo) {
      items <- diseños_muestreo[[grupo]]$items
      badge <- diseños_muestreo[[grupo]]$badge

      renderUI({
        bslib::layout_columns(
          col_widths = rep(6, length(items)),
          !!!lapply(items, function(d) {
            bslib::card(
              class = "card-muestreo",
              bslib::card_header(
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
                        if (es_sel) "" else "btn-outline-secondary"),
          style = if (es_sel)
            paste0("background:", colores$primario, " !important;",
                   "border-color:", colores$primario, " !important;",
                   "color: #ffffff !important;",
                   "font-weight: 600;")
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
          bslib::layout_columns(
            col_widths = c(4, 4, 4),
            value_box(title = "Muestra base (n\u2080)", value = textOutput(ns("res_n0")),
                      showcase = bsicons::bs_icon("people-fill"), theme = "primary"),
            value_box(title = "Con corrección finita (n)", value = textOutput(ns("res_nc")),
                      showcase = bsicons::bs_icon("funnel-fill"), theme = "info"),
            value_box(title = "Fracción de muestreo", value = textOutput(ns("res_frac")),
                      showcase = bsicons::bs_icon("percent"), theme = "secondary")
          ),
          br(),
          bslib::card(bslib::card_header("Interpretación"),  uiOutput(ns("interpretacion"))),
          bslib::card(bslib::card_header("Guía pedagógica"), uiOutput(ns("pedagogico"))),
          bslib::card(bslib::card_header("Fórmula"),         uiOutput(ns("formula_detalle"))),
          bslib::card(
            bslib::card_header(
              class = "d-flex justify-content-between align-items-center",
              tagList(bsicons::bs_icon("code-slash"), " Código R reproducible"),
              downloadButton(ns("descargar_script"), label = "Descargar .R",
                             icon = bsicons::bs_icon("download"), class = "btn-sm btn-outline-primary")
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
          bslib::layout_columns(
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
          bslib::card(
            bslib::card_header("Curva de potencia"),
            p(class = "text-muted small px-3 pt-2",
              "Muestra c\u00f3mo crece la potencia al aumentar el n por grupo."),
            plotOutput(ns("plot_potencia"), height = "260px")
          ),
          bslib::card(bslib::card_header("F\u00f3rmula"), div(class = "px-3 py-2", formula_ui)),
          bslib::card(
            bslib::card_header(
              class = "d-flex justify-content-between align-items-center",
              tagList(bsicons::bs_icon("code-slash"), " C\u00f3digo R reproducible"),
              downloadButton(ns("descargar_comp"), label = "Descargar .R",
                             icon = bsicons::bs_icon("download"), class = "btn-sm btn-outline-primary")
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

# ── Ejercicio: constantes de grilla ──────────────────

EJ_NCOL <- 8L; EJ_NROW <- 8L            # grilla fina: MAS, Sistemático
EJ_CONGL_NCOL <- 4L; EJ_CONGL_NROW <- 4L # grilla gruesa: Conglomerados

# Estratificado usa una grilla más ancha que alta (14 x 8, celdas cuadradas de
# igual tamaño que las demás: 17.5/14 = 10/8 = 1.25) para que los 3 estratos
# tengan espacio suficiente para distinguirse en pantalla.
EJ_ESTRAT_NCOL  <- 14L; EJ_ESTRAT_NROW <- 8L
EJ_ESTRAT_ANCHO <- 17.5; EJ_ESTRAT_ALTO <- 10

# ── Ejercicio: Aleatorio simple (MAS) ────────────────

poblacion_rv     <- reactiveVal(NULL)
muestra_rv       <- reactiveVal(NULL)
cuadrantes_rv    <- reactiveVal(NULL)   # ids de cuadrantes seleccionados
n_muestreo       <- reactiveVal(0)

observeEvent(
  list(input$ej_N, input$ej_media_pob, input$ej_sd_pob, input$ej_seed),
  {
    N <- input$ej_N; media <- input$ej_media_pob; sd <- input$ej_sd_pob; seed <- input$ej_seed
    req(N > 0, media > 0, sd > 0)
    pob <- generar_poblacion_natural(N, media, sd, seed)
    pob$qid <- asignar_cuadrante(pob$x, pob$y, EJ_NCOL, EJ_NROW)
    poblacion_rv(pob)
    muestra_rv(NULL)
    cuadrantes_rv(NULL)
    n_muestreo(0)
  },
  ignoreNULL = FALSE
)

observeEvent(input$ej_muestrear, {
  pob <- poblacion_rv()
  req(pob)
  total_q <- EJ_NCOL * EJ_NROW
  nq <- min(input$ej_nq, total_q)
  req(nq >= 1)
  cnt <- n_muestreo() + 1
  set.seed(input$ej_seed * 100 + cnt)
  q_sel <- sample(total_q, nq)
  pob$seleccionado <- pob$qid %in% q_sel
  poblacion_rv(pob)
  muestra_rv(pob[pob$seleccionado, ])
  cuadrantes_rv(q_sel)
  n_muestreo(cnt)
})

observeEvent(input$ej_resetear, {
  N <- input$ej_N; media <- input$ej_media_pob; sd <- input$ej_sd_pob; seed <- input$ej_seed
  req(N > 0)
  pob <- generar_poblacion_natural(N, media, sd, seed)
  pob$qid <- asignar_cuadrante(pob$x, pob$y, EJ_NCOL, EJ_NROW)
  poblacion_rv(pob)
  muestra_rv(NULL)
  cuadrantes_rv(NULL)
  n_muestreo(0)
})

output$ej_metricas <- renderUI({
  m <- muestra_rv(); q_sel <- cuadrantes_rv()

  if (is.null(m)) {
    return(
      div(class = "alert alert-info d-flex align-items-center gap-2",
          bsicons::bs_icon("info-circle-fill"),
          span("Configurá los parámetros y presioná ",
               strong("Tomar muestra"), " para ver los resultados."))
    )
  }

  Q <- EJ_NCOL * EJ_NROW
  nq    <- length(q_sel)
  n_arb <- nrow(m)
  x_bar <- if (n_arb > 0) mean(m$dap) else NA
  s     <- if (n_arb > 1) sd(m$dap) else 0
  ee    <- if (n_arb > 0) s / sqrt(n_arb) else NA
  fpc   <- sqrt((Q - nq) / (Q - 1))
  ee_c  <- ee * fpc

  tagList(
    bslib::layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box(title = "Cuadrantes muestreados", value = paste0(nq, " de ", Q),
                showcase = bsicons::bs_icon("grid-3x3-gap-fill"), theme = "primary"),
      value_box(title = "Árboles medidos", value = n_arb,
                showcase = bsicons::bs_icon("people-fill"),
                theme = value_box_theme(bg = colores$secundario)),
      value_box(title = "Media muestral", value = if (n_arb > 0) paste0(round(x_bar, 2), " cm") else "—",
                showcase = bsicons::bs_icon("rulers"),
                theme = value_box_theme(bg = colores$acento)),
      value_box(title = "Error estándar (aprox.)", value = if (n_arb > 1) paste0(round(ee_c, 3), " cm") else "—",
                showcase = bsicons::bs_icon("bullseye"), theme = "secondary")
    ),
    div(
      class = "d-flex align-items-start gap-2 mt-3 small alert alert-light border",
      bsicons::bs_icon("lightbulb-fill"),
      p(sprintf(
        "Se sortearon %d de %d cuadrantes y se midió cada árbol dentro de
         ellos (%d en total; algunos cuadrantes pueden no tener árboles, es
         parte de la variabilidad natural). El error estándar trata a los
         árboles medidos como una muestra simple — es una aproximación:
         el diseño real es un muestreo por conglomerados de cuadrantes, cuya
         varianza exacta depende de cómo se reparten los árboles entre
         cuadrantes.",
        nq, Q, n_arb
      ), class = "mb-0")
    )
  )
})

output$ej_plot <- renderPlot({
  pob <- poblacion_rv(); req(pob)
  q_sel <- cuadrantes_rv()
  celdas <- construir_grilla(EJ_NCOL, EJ_NROW)

  p <- ggplot()
  p <- agregar_celdas_grilla(p, celdas, q_sel, colores)
  agregar_puntos_grilla(p, pob, colores)
}, res = 110)

output$ej_tabla_wrap <- renderUI({
  m <- muestra_rv()
  if (is.null(m)) return(p("Aún no se tomó ninguna muestra.", class = "text-muted small"))
  DT::dataTableOutput(ns("ej_tabla"))
})

output$ej_tabla <- DT::renderDataTable({
  m <- muestra_rv()
  req(m)
  tbl <- m[order(m$qid, m$id), c("qid", "id", "dap")]
  colnames(tbl) <- c("Cuadrante", "ID árbol", "DAP (cm)")
  DT::datatable(tbl, options = list(pageLength = 10, dom = "tp", scrollX = TRUE),
                rownames = FALSE, class = "compact stripe")
})


# ── Ejercicio: Estratificado ─────────────────────────

estratos_def <- data.frame(
  estrato = c("Bosque primario", "Bosque secundario", "Pastizal"),
  media   = c(35, 22, 10),
  sd      = c(7, 5, 3),
  color   = c(colores$primario, colores$secundario, colores$texto),
  qx0     = c(1, 6, 10),
  qx1     = c(5, 9, 14),
  Nq      = c(40, 32, 40),
  N       = c(53, 35, 42),
  stringsAsFactors = FALSE
)

poblacion_estrat_rv <- reactiveVal(NULL)
muestra_estrat_rv   <- reactiveVal(NULL)
cuadrantes_estrat_rv <- reactiveVal(NULL)
n_muestreo_estrat   <- reactiveVal(0)

generar_poblacion_estrat <- function(seed) {
  set.seed(seed)
  cw <- EJ_ESTRAT_ANCHO / EJ_ESTRAT_NCOL
  filas <- lapply(seq_len(nrow(estratos_def)), function(i) {
    e  <- estratos_def[i, ]
    xr <- c((e$qx0 - 1) * cw, e$qx1 * cw)
    sub <- generar_poblacion_natural(e$N, e$media, e$sd, seed + i * 17, xr = xr, yr = c(0, EJ_ESTRAT_ALTO))
    sub$estrato    <- e$estrato
    sub$color_base <- e$color
    sub$qid        <- asignar_cuadrante(sub$x, sub$y, EJ_ESTRAT_NCOL, EJ_ESTRAT_NROW,
                                         ancho = EJ_ESTRAT_ANCHO, alto = EJ_ESTRAT_ALTO)
    sub
  })
  do.call(rbind, filas)
}

observeEvent(input$ej_estrat_seed, {
  req(input$ej_estrat_seed)
  poblacion_estrat_rv(generar_poblacion_estrat(input$ej_estrat_seed))
  muestra_estrat_rv(NULL)
  cuadrantes_estrat_rv(NULL)
  n_muestreo_estrat(0)
}, ignoreNULL = FALSE)

observeEvent(input$ej_estrat_prop, {
  total_n <- sum(input$ej_estrat_nA, input$ej_estrat_nB, input$ej_estrat_nC, na.rm = TRUE)
  req(total_n > 0)
  nh_prop <- pmax(round(total_n * estratos_def$Nq / sum(estratos_def$Nq)), 1)
  updateNumericInput(session, "ej_estrat_nA", value = nh_prop[1])
  updateNumericInput(session, "ej_estrat_nB", value = nh_prop[2])
  updateNumericInput(session, "ej_estrat_nC", value = nh_prop[3])
})

observeEvent(input$ej_estrat_muestrear, {
  pob <- poblacion_estrat_rv()
  req(pob)
  nh <- c(input$ej_estrat_nA, input$ej_estrat_nB, input$ej_estrat_nC)
  req(all(nh >= 1))
  cnt <- n_muestreo_estrat() + 1
  set.seed(input$ej_estrat_seed * 100 + cnt)

  q_sel_total <- c()
  for (i in seq_len(nrow(estratos_def))) {
    e <- estratos_def[i, ]
    quads_estrato <- (e$qx0):(e$qx1)
    quads_estrato <- unlist(lapply(quads_estrato, function(qx) qx + (0:(EJ_ESTRAT_NROW - 1)) * EJ_ESTRAT_NCOL))
    n_i <- min(nh[i], length(quads_estrato))
    q_sel_total <- c(q_sel_total, sample(quads_estrato, n_i))
  }

  pob$seleccionado <- pob$qid %in% q_sel_total
  poblacion_estrat_rv(pob)
  muestra_estrat_rv(pob[pob$seleccionado, ])
  cuadrantes_estrat_rv(q_sel_total)
  n_muestreo_estrat(cnt)
})

observeEvent(input$ej_estrat_resetear, {
  req(input$ej_estrat_seed)
  poblacion_estrat_rv(generar_poblacion_estrat(input$ej_estrat_seed))
  muestra_estrat_rv(NULL)
  cuadrantes_estrat_rv(NULL)
  n_muestreo_estrat(0)
})

output$ej_estrat_metricas <- renderUI({
  m <- muestra_estrat_rv()

  if (is.null(m)) {
    return(
      div(class = "alert alert-info d-flex align-items-center gap-2",
          bsicons::bs_icon("info-circle-fill"),
          span("Configurá el número de cuadrantes por estrato y presioná ",
               strong("Tomar muestra"), "."))
    )
  }

  Q <- sum(estratos_def$Nq)
  resumen <- do.call(rbind, lapply(seq_len(nrow(estratos_def)), function(i) {
    nombre <- estratos_def$estrato[i]
    sub    <- m[m$estrato == nombre, ]
    nh_i   <- c(input$ej_estrat_nA, input$ej_estrat_nB, input$ej_estrat_nC)[i]
    data.frame(
      estrato = nombre,
      Nq      = estratos_def$Nq[i],
      nq      = nh_i,
      media_h = if (nrow(sub) > 0) mean(sub$dap) else NA,
      var_h   = if (nrow(sub) > 1) var(sub$dap) else 0
    )
  }))

  resumen$Wh <- resumen$Nq / Q
  y_st <- sum(resumen$Wh * resumen$media_h, na.rm = TRUE)

  var_st <- sum(
    (resumen$Wh^2) * (resumen$var_h / pmax(resumen$nq, 1)) * (1 - resumen$nq / resumen$Nq),
    na.rm = TRUE
  )
  ee_st    <- sqrt(var_st)
  nq_total <- sum(resumen$nq)
  n_arb    <- nrow(m)

  tagList(
    bslib::layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box(title = "Cuadrantes muestreados", value = paste0(nq_total, " de ", Q),
                showcase = bsicons::bs_icon("grid-3x3-gap-fill"), theme = "primary"),
      value_box(title = "Árboles medidos", value = n_arb,
                showcase = bsicons::bs_icon("people-fill"),
                theme = value_box_theme(bg = colores$secundario)),
      value_box(title = "Estimación combinada", value = paste0(round(y_st, 2), " cm"),
                showcase = bsicons::bs_icon("rulers"),
                theme = value_box_theme(bg = colores$acento)),
      value_box(title = "Error estándar (aprox.)", value = paste0(round(ee_st, 3), " cm"),
                showcase = bsicons::bs_icon("bullseye"), theme = "secondary")
    ),
    div(
      class = "d-flex align-items-start gap-2 mt-3 small alert alert-light border",
      bsicons::bs_icon("lightbulb-fill"),
      p(sprintf(
        "La estimación combinada pondera la media de cada estrato por su peso
         en la grilla (Wh = cuadrantes del estrato / cuadrantes totales = %s
         — cuanto más grande el estrato, más pesa su media). Probá
         «Asignación proporcional» y compará el error estándar contra una
         asignación igualitaria para ver el efecto.",
        paste(round(resumen$Wh, 2), collapse = " / ")
      ), class = "mb-0")
    )
  )
})

output$ej_estrat_plot <- renderPlot({
  pob <- poblacion_estrat_rv()
  req(pob)
  q_sel <- cuadrantes_estrat_rv()
  celdas <- construir_grilla(EJ_ESTRAT_NCOL, EJ_ESTRAT_NROW,
                              ancho = EJ_ESTRAT_ANCHO, alto = EJ_ESTRAT_ALTO)
  cw <- EJ_ESTRAT_ANCHO / EJ_ESTRAT_NCOL

  p <- ggplot()
  for (i in seq_len(nrow(estratos_def))) {
    e <- estratos_def[i, ]
    p <- p +
      annotate("rect", xmin = (e$qx0 - 1) * cw, xmax = e$qx1 * cw, ymin = 0, ymax = EJ_ESTRAT_ALTO,
               fill = e$color, alpha = 0.06) +
      annotate("text", x = ((e$qx0 - 1) * cw + e$qx1 * cw) / 2, y = EJ_ESTRAT_ALTO - 0.15, label = e$estrato,
               size = 3.5, color = e$color, fontface = "bold")
  }

  p <- agregar_celdas_grilla(p, celdas, q_sel, colores)
  agregar_puntos_grilla(p, pob, colores,
                         xlim = c(0, EJ_ESTRAT_ANCHO), ylim = c(0, EJ_ESTRAT_ALTO))
}, res = 110)

output$ej_estrat_tabla_wrap <- renderUI({
  m <- muestra_estrat_rv()
  if (is.null(m)) return(p("Aún no se tomó ninguna muestra.", class = "text-muted small"))
  DT::dataTableOutput(ns("ej_estrat_tabla"))
})

output$ej_estrat_tabla <- DT::renderDataTable({
  m <- muestra_estrat_rv()
  req(m)
  tbl <- m[order(m$estrato, m$qid, m$id), c("qid", "id", "estrato", "dap")]
  colnames(tbl) <- c("Cuadrante", "ID árbol", "Estrato", "DAP (cm)")
  DT::datatable(tbl, options = list(pageLength = 10, dom = "tp", scrollX = TRUE),
                rownames = FALSE, class = "compact stripe")
})


# ── Ejercicio: Sistemático (cuadrantes en patrón regular) ──

poblacion_sist_rv  <- reactiveVal(NULL)
muestra_sist_rv    <- reactiveVal(NULL)
cuadrantes_sist_rv <- reactiveVal(NULL)
n_muestreo_sist    <- reactiveVal(0)
espaciado_sist_rv  <- reactiveVal(NULL)
inicio_sist_rv     <- reactiveVal(NULL)

output$ej_sist_periodo_ui <- renderUI({
  if (isTRUE(input$ej_sist_periodico)) {
    numericInput(ns("ej_sist_periodo"),
                 "Período del patrón (en unidades del área, 0–10)",
                 value = 2.5, min = 0.5, max = 5, step = 0.5)
  }
})

generar_poblacion_sist <- function(N, media, sd, seed, periodico, periodo) {
  pob <- generar_poblacion_natural(N, media, sd, seed)
  if (isTRUE(periodico) && !is.null(periodo) && periodo >= 0.5) {
    set.seed(seed + 999)
    ajuste <- (sd * 0.9) * sin(2 * pi * pob$x / periodo)
    pob$dap <- pmax(round(pob$dap + ajuste, 1), 1)
  }
  pob$qid <- asignar_cuadrante(pob$x, pob$y, EJ_NCOL, EJ_NROW)
  pob
}

observeEvent(
  list(input$ej_sist_N, input$ej_sist_media_pob, input$ej_sist_sd_pob,
       input$ej_sist_seed, input$ej_sist_periodico, input$ej_sist_periodo),
  {
    req(input$ej_sist_N, input$ej_sist_media_pob, input$ej_sist_sd_pob, input$ej_sist_seed)
    poblacion_sist_rv(generar_poblacion_sist(
      input$ej_sist_N, input$ej_sist_media_pob, input$ej_sist_sd_pob,
      input$ej_sist_seed, input$ej_sist_periodico, input$ej_sist_periodo
    ))
    muestra_sist_rv(NULL)
    cuadrantes_sist_rv(NULL)
    espaciado_sist_rv(NULL)
    inicio_sist_rv(NULL)
    n_muestreo_sist(0)
  },
  ignoreNULL = FALSE
)

observeEvent(input$ej_sist_muestrear, {
  pob <- poblacion_sist_rv()
  req(pob)
  nq_deseado <- max(4, input$ej_sist_nq)
  lado <- max(2, round(sqrt(nq_deseado)))
  sx <- max(1, round(EJ_NCOL / lado))
  sy <- max(1, round(EJ_NROW / lado))
  cnt <- n_muestreo_sist() + 1
  set.seed(input$ej_sist_seed * 100 + cnt)
  qx0 <- sample(seq_len(sx), 1)
  qy0 <- sample(seq_len(sy), 1)
  qxs <- seq(qx0, EJ_NCOL, by = sx)
  qys <- seq(qy0, EJ_NROW, by = sy)
  grilla_sel <- expand.grid(qx = qxs, qy = qys)
  q_sel <- (grilla_sel$qy - 1) * EJ_NCOL + grilla_sel$qx

  pob$seleccionado <- pob$qid %in% q_sel
  poblacion_sist_rv(pob)
  muestra_sist_rv(pob[pob$seleccionado, ])
  cuadrantes_sist_rv(q_sel)
  espaciado_sist_rv(c(sx, sy))
  inicio_sist_rv(c(qx0, qy0))
  n_muestreo_sist(cnt)
})

observeEvent(input$ej_sist_resetear, {
  req(input$ej_sist_N, input$ej_sist_seed)
  poblacion_sist_rv(generar_poblacion_sist(
    input$ej_sist_N, input$ej_sist_media_pob, input$ej_sist_sd_pob,
    input$ej_sist_seed, input$ej_sist_periodico, input$ej_sist_periodo
  ))
  muestra_sist_rv(NULL)
  cuadrantes_sist_rv(NULL)
  espaciado_sist_rv(NULL)
  inicio_sist_rv(NULL)
  n_muestreo_sist(0)
})

output$ej_sist_metricas <- renderUI({
  m <- muestra_sist_rv()

  if (is.null(m)) {
    return(
      div(class = "alert alert-info d-flex align-items-center gap-2",
          bsicons::bs_icon("info-circle-fill"),
          span("Definí cuántos cuadrantes querés aproximadamente y presioná ",
               strong("Tomar muestra"), " para generar el patrón regular."))
    )
  }

  Q  <- EJ_NCOL * EJ_NROW
  esp <- espaciado_sist_rv()
  nq_sel <- length(cuadrantes_sist_rv())
  n_arb  <- nrow(m)
  x_bar  <- mean(m$dap)
  s      <- if (n_arb > 1) sd(m$dap) else 0
  ee     <- if (n_arb > 0) s / sqrt(n_arb) else NA

  riesgo <- NULL
  if (isTRUE(input$ej_sist_periodico) && !is.null(input$ej_sist_periodo)) {
    periodo <- input$ej_sist_periodo
    espaciado_fisico <- esp[1] * (10 / EJ_NCOL)
    ratio <- espaciado_fisico / periodo
    if (abs(ratio - round(ratio)) < 0.15) {
      riesgo <- div(
        class = "d-flex align-items-start gap-2 mt-3 small alert alert-danger",
        bsicons::bs_icon("exclamation-octagon-fill"),
        p(sprintf(
          "Riesgo de sesgo por aliasing: el espaciado de la grilla (%.2f
           unidades) coincide o es múltiplo del período del patrón
           poblacional (%.2f). El muestreo sistemático puede caer siempre en
           la misma fase del ciclo y producir una estimación sesgada, aunque
           n sea grande. Desactivá el patrón periódico o cambiá el número de
           cuadrantes deseados para comparar.",
          espaciado_fisico, periodo
        ), class = "mb-0")
      )
    }
  }

  tagList(
    bslib::layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box(title = "Cuadrantes muestreados", value = paste0(nq_sel, " de ", Q),
                showcase = bsicons::bs_icon("grid-3x3-gap-fill"), theme = "primary"),
      value_box(title = "Árboles medidos", value = n_arb,
                showcase = bsicons::bs_icon("people-fill"),
                theme = value_box_theme(bg = colores$secundario)),
      value_box(title = "Media muestral", value = paste0(round(x_bar, 2), " cm"),
                showcase = bsicons::bs_icon("rulers"),
                theme = value_box_theme(bg = colores$acento)),
      value_box(title = "Patrón de la grilla", value = sprintf("cada %d × %d cuadr.", esp[1], esp[2]),
                showcase = bsicons::bs_icon("distribute-horizontal"), theme = "secondary")
    ),
    if (!is.null(riesgo)) riesgo else div(
      class = "d-flex align-items-start gap-2 mt-3 small alert alert-light border",
      bsicons::bs_icon("lightbulb-fill"),
      p(sprintf(
        "Se seleccionó 1 de cada %d cuadrantes en columnas y 1 de cada %d en
         filas, con un punto de inicio aleatorio. El error estándar trata a
         los árboles medidos como una muestra simple (EE ≈ s/√n = %.3f cm)
         como referencia simplificada.",
        esp[1], esp[2], ee
      ), class = "mb-0")
    )
  )
})

output$ej_sist_plot <- renderPlot({
  pob <- poblacion_sist_rv(); req(pob)
  q_sel <- cuadrantes_sist_rv()
  celdas <- construir_grilla(EJ_NCOL, EJ_NROW)

  p <- ggplot()
  p <- agregar_celdas_grilla(p, celdas, q_sel, colores)
  agregar_puntos_grilla(p, pob, colores)
}, res = 110)

output$ej_sist_tabla_wrap <- renderUI({
  m <- muestra_sist_rv()
  if (is.null(m)) return(p("Aún no se tomó ninguna muestra.", class = "text-muted small"))
  DT::dataTableOutput(ns("ej_sist_tabla"))
})

output$ej_sist_tabla <- DT::renderDataTable({
  m <- muestra_sist_rv()
  req(m)
  tbl <- m[order(m$qid, m$id), c("qid", "id", "dap")]
  colnames(tbl) <- c("Cuadrante", "ID árbol", "DAP (cm)")
  DT::datatable(tbl, options = list(pageLength = 10, dom = "tp", scrollX = TRUE),
                rownames = FALSE, class = "compact stripe")
})


# ── Ejercicio: Por conglomerados ─────────────────────

poblacion_congl_rv  <- reactiveVal(NULL)
muestra_congl_rv    <- reactiveVal(NULL)
cuadrantes_congl_rv <- reactiveVal(NULL)
n_muestreo_congl    <- reactiveVal(0)

observeEvent(
  list(input$ej_congl_N, input$ej_congl_media_pob, input$ej_congl_sd_pob, input$ej_congl_seed),
  {
    N <- input$ej_congl_N; media <- input$ej_congl_media_pob
    sd <- input$ej_congl_sd_pob; seed <- input$ej_congl_seed
    req(N > 0, media > 0, sd > 0)
    pob <- generar_poblacion_natural(N, media, sd, seed)
    pob$qid <- asignar_cuadrante(pob$x, pob$y, EJ_CONGL_NCOL, EJ_CONGL_NROW)
    poblacion_congl_rv(pob)
    muestra_congl_rv(NULL)
    cuadrantes_congl_rv(NULL)
    n_muestreo_congl(0)
  },
  ignoreNULL = FALSE
)

observeEvent(input$ej_congl_muestrear, {
  pob <- poblacion_congl_rv()
  req(pob)
  total_q <- EJ_CONGL_NCOL * EJ_CONGL_NROW
  m <- min(input$ej_congl_m, total_q)
  req(m >= 1)
  cnt <- n_muestreo_congl() + 1
  set.seed(input$ej_congl_seed * 100 + cnt)
  q_sel <- sample(total_q, m)
  pob$seleccionado <- pob$qid %in% q_sel
  poblacion_congl_rv(pob)
  muestra_congl_rv(pob[pob$seleccionado, ])
  cuadrantes_congl_rv(q_sel)
  n_muestreo_congl(cnt)
})

observeEvent(input$ej_congl_resetear, {
  N <- input$ej_congl_N; media <- input$ej_congl_media_pob
  sd <- input$ej_congl_sd_pob; seed <- input$ej_congl_seed
  req(N > 0)
  pob <- generar_poblacion_natural(N, media, sd, seed)
  pob$qid <- asignar_cuadrante(pob$x, pob$y, EJ_CONGL_NCOL, EJ_CONGL_NROW)
  poblacion_congl_rv(pob)
  muestra_congl_rv(NULL)
  cuadrantes_congl_rv(NULL)
  n_muestreo_congl(0)
})

output$ej_congl_metricas <- renderUI({
  m <- muestra_congl_rv()

  if (is.null(m)) {
    return(
      div(class = "alert alert-info d-flex align-items-center gap-2",
          bsicons::bs_icon("info-circle-fill"),
          span("Elegí cuántos conglomerados muestrear y presioná ",
               strong("Tomar muestra"), "."))
    )
  }

  Q <- EJ_CONGL_NCOL * EJ_CONGL_NROW
  clusters_sel <- sort(unique(m$qid))
  medias_i <- sapply(clusters_sel, function(cl) mean(m$dap[m$qid == cl]))
  Ni_sel   <- sapply(clusters_sel, function(cl) sum(m$qid == cl))

  y_no_pesado <- mean(medias_i)
  y_razon     <- sum(m$dap) / sum(Ni_sel)
  diff_est    <- abs(y_no_pesado - y_razon)

  tagList(
    bslib::layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box(title = "Conglomerados muestreados",
                value = paste0(length(clusters_sel), " de ", Q),
                showcase = bsicons::bs_icon("grid-3x3-gap-fill"), theme = "primary"),
      value_box(title = "Árboles medidos", value = nrow(m),
                showcase = bsicons::bs_icon("people-fill"),
                theme = value_box_theme(bg = colores$secundario)),
      value_box(title = "Estimación (media de medias)",
                value = paste0(round(y_no_pesado, 2), " cm"),
                showcase = bsicons::bs_icon("rulers"),
                theme = value_box_theme(bg = colores$acento)),
      value_box(title = "Estimación (razón combinada)",
                value = paste0(round(y_razon, 2), " cm"),
                showcase = bsicons::bs_icon("bullseye"), theme = "secondary")
    ),
    div(
      class = "d-flex align-items-start gap-2 mt-3 small alert alert-light border",
      bsicons::bs_icon("lightbulb-fill"),
      p(sprintf(
        "La media de medias trata cada conglomerado con igual peso, sin importar
         cuántos árboles cayeron en él por azar; la razón combinada pondera por
         el número de árboles medidos, por lo que suele ser más robusta cuando
         los conglomerados quedan con tamaños muy distintos. La diferencia entre
         ambas estimaciones (%.2f cm) es una señal de cuánto está afectando esa
         diferencia de tamaño — revisá la tabla de detalle.",
        diff_est
      ), class = "mb-0")
    )
  )
})

output$ej_congl_plot <- renderPlot({
  pob <- poblacion_congl_rv(); req(pob)
  q_sel <- cuadrantes_congl_rv()
  celdas <- construir_grilla(EJ_CONGL_NCOL, EJ_CONGL_NROW)

  p <- ggplot()
  p <- agregar_celdas_grilla(p, celdas, q_sel, colores)
  agregar_puntos_grilla(p, pob, colores)
}, res = 110)

output$ej_congl_detalle_wrap <- renderUI({
  m <- muestra_congl_rv()
  if (is.null(m)) return(p("Aún no se tomó ninguna muestra.", class = "text-muted small"))
  DT::dataTableOutput(ns("ej_congl_detalle_tabla"))
})

output$ej_congl_detalle_tabla <- DT::renderDataTable({
  m <- muestra_congl_rv()
  req(m)
  detalle <- do.call(rbind, lapply(sort(unique(m$qid)), function(cl) {
    sub <- m[m$qid == cl, ]
    data.frame(
      conglomerado = cl,
      Ni      = nrow(sub),
      media   = round(mean(sub$dap), 1),
      sd      = round(if (nrow(sub) > 1) sd(sub$dap) else 0, 1)
    )
  }))
  colnames(detalle) <- c("Conglomerado", "Árboles (Ni)", "Media DAP (cm)", "SD DAP (cm)")
  DT::datatable(detalle, options = list(pageLength = 10, dom = "tp", scrollX = TRUE),
                rownames = FALSE, class = "compact stripe")
})

output$ej_congl_tabla_wrap <- renderUI({
  m <- muestra_congl_rv()
  if (is.null(m)) return(p("Aún no se tomó ninguna muestra.", class = "text-muted small"))
  DT::dataTableOutput(ns("ej_congl_tabla"))
})

output$ej_congl_tabla <- DT::renderDataTable({
  m <- muestra_congl_rv()
  req(m)
  tbl <- m[order(m$qid, m$id), c("id", "qid", "dap")]
  colnames(tbl) <- c("ID árbol", "Conglomerado", "DAP (cm)")
  DT::datatable(tbl, options = list(pageLength = 10, dom = "tp", scrollX = TRUE),
                rownames = FALSE, class = "compact stripe")
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
        bslib::layout_columns(
          col_widths = c(6, 6),
          bslib::card(
            bslib::card_header("Descripción"),
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
          bslib::card(
            bslib::card_header("Ejemplo"),
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
