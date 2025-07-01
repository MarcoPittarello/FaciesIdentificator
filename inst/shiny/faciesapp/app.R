# ---- UI ----
ui <- shiny::fluidPage(
  shiny::titlePanel(shiny::strong("Facies Identificator")),
  shinythemes::shinytheme("cosmo"),
  shiny::navbarPage(icon = shiny::icon("home"),
                    
                    shiny::tabPanel("Istruzioni",
                                    shiny::fluidPage(
                                      shiny::h2(shiny::strong("Come usare questa app")),
                                      shiny::p("1. Carica il file con la matrice vegetazionale per la quale occorre identificare le facies (.csv o .xlsx)."),
                                      shiny::p("2. Carica il file con la matrice vegetazionale con la composizione delle facies di riferimento (.csv o .xlsx)."),
                                      shiny::p("3. Imposta parametri."),
                                      shiny::p("4. Clicca 'Esegui Analisi'."),
                                      shiny::p("5. Visualizza e scarica i risultati."),
                                      shiny::h2(shiny::strong("Impostazioni matrice specie")),
                                      shiny::p("Righe dataset--> SPECIE. colonna in prima posizione!"),
                                      shiny::p("Colonne dataset --> CODICI RILIEVO"),
                                      shiny::p("Nomenclatura: Aeschimann et al. 2004")
                                    )
                    ),
                    
                    shiny::tabPanel("App",
                                    shiny::sidebarLayout(
                                      shiny::sidebarPanel(
                                        shiny::fileInput("file_tbd", "Carica file con facies DA DEFINIRE (.csv o .xlsx)", accept = c(".csv", ".xlsx")),
                                        shiny::uiOutput("sheet_select_tbd"),
                                        
                                        shiny::fileInput("file_ref", "Carica file con facies di RIFERIMENTO (.csv o .xlsx)", accept = c(".csv", ".xlsx")),
                                        shiny::uiOutput("sheet_select_ref"),
                                        
                                        shiny::checkboxInput("prime10", label = "Uso solo le prime 10 specie più abbondanti?", value = TRUE),
                                        shiny::checkboxInput("soglia", "Uso un valore soglia di similarità?", value = TRUE),
                                        shiny::numericInput("valore_soglia", "Specificare valore soglia di similarità (<=)", value = 0.5, min = 0, max = 1, step = 0.01),
                                        
                                        shiny::actionButton("run_btn", "Esegui Analisi")
                                      ),
                                      
                                      shiny::mainPanel(
                                        shiny::tabsetPanel(
                                          shiny::tabPanel("Similarità Presenza/Assenza (Jaccard)",
                                                          shiny::tabsetPanel(
                                                            shiny::tabPanel("Complessivo",
                                                                            DT::dataTableOutput("jaccard_tot"),
                                                                            shiny::downloadButton("download_jaccard_tot_csv", "Scarica CSV"),
                                                                            shiny::downloadButton("download_jaccard_tot_xlsx", "Scarica XLSX")
                                                            ),
                                                            shiny::tabPanel("Sintetico",
                                                                            DT::dataTableOutput("jaccard_synth"),
                                                                            shiny::downloadButton("download_jaccard_synth_csv", "Scarica CSV"),
                                                                            shiny::downloadButton("download_jaccard_synth_xlsx", "Scarica XLSX")
                                                            )
                                                          )
                                          ),
                                          shiny::tabPanel("Similarità per abbondanza (Bray curtis)",
                                                          shiny::tabsetPanel(
                                                            shiny::tabPanel("Complessivo",
                                                                            DT::dataTableOutput("bray_tot"),
                                                                            shiny::downloadButton("download_bray_tot_csv", "Scarica CSV"),
                                                                            shiny::downloadButton("download_bray_tot_xlsx", "Scarica XLSX")
                                                            ),
                                                            shiny::tabPanel("Sintetico",
                                                                            DT::dataTableOutput("bray_synth"),
                                                                            shiny::downloadButton("download_bray_synth_csv", "Scarica CSV"),
                                                                            shiny::downloadButton("download_bray_synth_xlsx", "Scarica XLSX")
                                                            )
                                                          )
                                          )
                                        )
                                      )
                                    )
                    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  # UI dinamica per fogli xlsx
  shiny::observeEvent(input$file_tbd, {
    shiny::req(input$file_tbd)
    ext <- tools::file_ext(input$file_tbd$name)
    if (ext == "xlsx") {
      sheets <- readxl::excel_sheets(input$file_tbd$datapath)
      output$sheet_select_tbd <- shiny::renderUI({
        shiny::selectInput("sheet_tbd", "Seleziona foglio", choices = sheets)
      })
    } else {
      output$sheet_select_tbd <- shiny::renderUI({ NULL })
    }
  })
  
  shiny::observeEvent(input$file_ref, {
    shiny::req(input$file_ref)
    ext <- tools::file_ext(input$file_ref$name)
    if (ext == "xlsx") {
      sheets <- readxl::excel_sheets(input$file_ref$datapath)
      output$sheet_select_ref <- shiny::renderUI({
        shiny::selectInput("sheet_ref", "Seleziona foglio", choices = sheets)
      })
    } else {
      output$sheet_select_ref <- shiny::renderUI({ NULL })
    }
  })
  
  # Funzione per leggere dati da file
  read_data <- function(file, sheet = NULL) {
    shiny::req(file)
    ext <- tools::file_ext(file$datapath)
    if (ext == "csv") {
      readr::read_csv(file$datapath, show_col_types = FALSE)
    } else if (ext == "xlsx") {
      readxl::read_excel(file$datapath, sheet = sheet)
    } else {
      shiny::validate("Formato file non supportato.")
    }
  }
  
  # Analisi al click
  dati <- shiny::eventReactive(input$run_btn, {
    shiny::req(input$file_tbd, input$file_ref)
    
    ext_tbd <- tools::file_ext(input$file_tbd$name)
    ext_ref <- tools::file_ext(input$file_ref$name)
    
    tbd <- read_data(input$file_tbd, sheet = if (ext_tbd == "xlsx") input$sheet_tbd else NULL)
    ref <- read_data(input$file_ref, sheet = if (ext_ref == "xlsx") input$sheet_ref else NULL)
    
    FaciesIdentificator::FaciesIdentificator(
      tbd = tbd,
      ref = ref,
      prime10tbd = input$prime10,
      soglia = input$soglia,
      valore.soglia = input$valore_soglia
    )
  })
  
  # Output tabelle con filtro
  output$jaccard_tot <- DT::renderDataTable({ shiny::req(dati()); dati()$Jaccard$complessivo }, filter = "top", options = list(pageLength = 10))
  output$jaccard_synth <- DT::renderDataTable({ shiny::req(dati()); dati()$Jaccard$sintetico }, filter = "top", options = list(pageLength = 10))
  output$bray_tot <- DT::renderDataTable({ shiny::req(dati()); dati()$Bray$complessivo }, filter = "top", options = list(pageLength = 10))
  output$bray_synth <- DT::renderDataTable({ shiny::req(dati()); dati()$Bray$sintetico }, filter = "top", options = list(pageLength = 10))
  
  # Download handler Jaccard
  output$download_jaccard_tot_csv <- shiny::downloadHandler(
    filename = function() "Jaccard_complessivo.csv",
    content = function(file) readr::write_csv(dati()$Jaccard$complessivo, file)
  )
  output$download_jaccard_tot_xlsx <- shiny::downloadHandler(
    filename = function() "Jaccard_complessivo.xlsx",
    content = function(file) writexl::write_xlsx(dati()$Jaccard$complessivo, path = file)
  )
  output$download_jaccard_synth_csv <- shiny::downloadHandler(
    filename = function() "Jaccard_sintetico.csv",
    content = function(file) readr::write_csv(dati()$Jaccard$sintetico, file)
  )
  output$download_jaccard_synth_xlsx <- shiny::downloadHandler(
    filename = function() "Jaccard_sintetico.xlsx",
    content = function(file) writexl::write_xlsx(dati()$Jaccard$sintetico, path = file)
  )
  
  # Download handler Bray
  output$download_bray_tot_csv <- shiny::downloadHandler(
    filename = function() "Bray_complessivo.csv",
    content = function(file) readr::write_csv(dati()$Bray$complessivo, file)
  )
  output$download_bray_tot_xlsx <- shiny::downloadHandler(
    filename = function() "Bray_complessivo.xlsx",
    content = function(file) writexl::write_xlsx(dati()$Bray$complessivo, path = file)
  )
  output$download_bray_synth_csv <- shiny::downloadHandler(
    filename = function() "Bray_sintetico.csv",
    content = function(file) readr::write_csv(dati()$Bray$sintetico, file)
  )
  output$download_bray_synth_xlsx <- shiny::downloadHandler(
    filename = function() "Bray_sintetico.xlsx",
    content = function(file) writexl::write_xlsx(dati()$Bray$sintetico, path = file)
  )
}

# ---- AVVIO APP ----
shiny::shinyApp(ui = ui, server = server)
