#' Avvia la Shiny app di FaciesIdentificator
#' @export
FIapp <- function() {
  appDir <- system.file("shiny/faciesapp", package = "FaciesIdentificator")
  if (appDir == "") {
    stop("App directory non trovata. Verifica la struttura del pacchetto.")
  }
  shiny::runApp(appDir, display.mode = "normal")
}