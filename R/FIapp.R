#' Avvia la Shiny app di FaciesIdentificator
#' @export
FIapp <- function() {
  
  lib <- c ("vegan","shiny","shinythemes","writexl","DT","readr","readxl","FaciesIdentificator")
  
  # Install and load libraries
  for (i in lib) {
    if (i %in% installed.packages () [,"Package"] == F) {
      install.packages (i)
    }
    library (i, character.only = TRUE)
  }
  
  appDir <- system.file("shiny/faciesapp", package = "FaciesIdentificator")
  if (appDir == "") {
    stop("App directory non trovata. Verifica la struttura del pacchetto.")
  }
  shiny::runApp(appDir, display.mode = "normal")
}