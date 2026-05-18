#' Application UI
#'
#' @return A Shiny UI object.
#' @noRd
app_ui <- function() {

  golem::add_resource_path(
    "www",
    system.file("app/www", package = "StatDesign")
  )

  page_navbar(
    title = div(
      style = "display: flex; align-items: center; gap: 10px; margin-top: 4px;",
      img(src = "www/hexsticker_StatDesign.png", height = "38px"),
      span("StatDesign", style = "font-weight: 600;")
    ),
    theme  = tema_app,
    lang   = "es",
    footer = div(
      class = "text-center text-muted small py-2",
      style = paste0("border-top: 1px solid ", colores$borde, ";"),
      "Manuel Sp\u00ednola \u00b7 ICOMVIS \u00b7 Universidad Nacional \u00b7 Costa Rica"
    ),
    nav_panel(title = "Tipos de estudio",       icon = bs_icon("book"),             mod_tipos_ui("tipos")),
    nav_panel(title = "Dise\u00f1os de muestreo",    icon = bs_icon("grid"),             mod_muestreo_ui("muestreo")),
    nav_panel(title = "Fundamentos del dise\u00f1o", icon = bs_icon("journal-bookmark"), mod_fundamentos_ui("fundamentos")),
    nav_panel(title = "Asistente",              icon = bs_icon("compass"),          mod_asistente_ui("asistente")),
    nav_panel(title = "Ayuda",                  icon = bs_icon("question-circle"),  mod_ayuda_ui("ayuda")),
    nav_panel(title = "Acerca de",              icon = bs_icon("info-circle"),      mod_acerca_de_ui("acerca_de")),
    nav_spacer(),
    nav_item(tags$span(class = "text-muted small", "StatDesign v1.0"))
  )
}
