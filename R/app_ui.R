#' Application UI
#'
#' @return A Shiny UI object.
#' @noRd
app_ui <- function() {

  tagList(
    golem_add_external_resources(),

    bslib::page_navbar(
      title = div(
        style = "display: flex; align-items: center; gap: 10px; margin-top: 4px;",
        img(src = "www/hexsticker_StatDesign.png", height = "38px"),
        span("StatDesign", style = "font-weight: 600;")
      ),
      theme  = tema_app,
      lang   = "es",
      footer = div(
        class = "text-center small py-2",
        style = paste0("background:", colores$primario, "; color: white;"),
        "Manuel Sp\u00ednola \u00b7 ICOMVIS \u00b7 Universidad Nacional \u00b7 Costa Rica"
      ),
      bslib::nav_panel(title = "Tipos de estudio",       icon = bsicons::bs_icon("book"),             mod_tipos_ui("tipos")),
      bslib::nav_panel(title = "Fundamentos del dise\u00f1o", icon = bsicons::bs_icon("journal-bookmark"), mod_fundamentos_ui("fundamentos")),
      bslib::nav_panel(title = "Dise\u00f1os de muestreo",    icon = bsicons::bs_icon("grid"),             mod_muestreo_ui("muestreo")),
      bslib::nav_panel(title = "Asistente",              icon = bsicons::bs_icon("compass"),          mod_asistente_ui("asistente")),
      bslib::nav_panel(title = "Ayuda",                  icon = bsicons::bs_icon("question-circle"),  mod_ayuda_ui("ayuda")),
      bslib::nav_panel(title = "Acerca de",              icon = bsicons::bs_icon("info-circle"),      mod_acerca_de_ui("acerca_de")),
      bslib::nav_spacer(),
      bslib::nav_item(tags$span(class = "text-muted small", "StatDesign v1.0"))
    )
  )
}

#' Add external resources to the application
#'
#' Recurso de golem para registrar la carpeta www/ y el <head> de la
#' p\u00e1gina (favicon, etc.). Se llama como hermana de page_navbar() dentro
#' de un tagList(), no como argumento de page_navbar() \u2014 ese slot solo
#' acepta nav_panel()/nav_menu().
#'
#' @noRd
golem_add_external_resources <- function() {
  golem::add_resource_path(
    "www",
    system.file("app/www", package = "StatDesign")
  )

  tags$head(
    golem::favicon(ext = "png")
  )
}
