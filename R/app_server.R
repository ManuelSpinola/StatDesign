#' Application Server
#'
#' @param input,output,session Internal parameters for Shiny.
#' @noRd
app_server <- function(input, output, session) {
  mod_tipos_server("tipos")
  mod_muestreo_server("muestreo")
  mod_fundamentos_server("fundamentos")
  mod_asistente_server("asistente")
  mod_ayuda_server("ayuda")
  mod_acerca_de_server("acerca_de")
}
