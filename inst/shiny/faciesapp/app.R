# ---- UI ----
ui <- fluidPage(
  titlePanel(strong("Facies Identificator")),
  #shinythemes::shinytheme("cosmo"),
  navbarPage(icon("home"),
                    
                    tabPanel("Istruzioni",
                                    fluidPage(
                                      h2(strong("Come usare questa app")),
                                      p("1. Carica il file con la matrice vegetazionale per la quale occorre identificare le facies (.csv o .xlsx)."),
                                      p("2. Carica il file con la matrice vegetazionale con la composizione delle facies di riferimento (.csv o .xlsx)."),
                                      p("3. Imposta parametri."),
                                      p("4. Clicca 'Esegui Analisi'."),
                                      p("5. Visualizza e scarica i risultati."),
                                      h2(strong("Impostazioni matrice specie")),
                                      p("Righe dataset--> SPECIE. colonna in prima posizione!"),
                                      p("Colonne dataset --> CODICI RILIEVO"),
                                      p("Nomenclatura: Aeschimann et al. 2004")
                                    )
                    ),
                    
                    tabPanel("App",
                                    sidebarLayout(
                                      sidebarPanel(
                                        fileInput("file_tbd", "Carica file con facies DA DEFINIRE (.csv o .xlsx)", accept = c(".csv", ".xlsx")),
                                        uiOutput("sheet_select_tbd"),
                                        
                                        fileInput("file_ref", "Carica file con facies di RIFERIMENTO (.csv o .xlsx)", accept = c(".csv", ".xlsx")),
                                        uiOutput("sheet_select_ref"),
                                        
                                        checkboxInput("prime10", label = "Uso solo le prime 10 specie più abbondanti?", value = TRUE),
                                        checkboxInput("soglia", "Uso un valore soglia di similarità?", value = TRUE),
                                        numericInput("valore_soglia", "Specificare valore soglia di similarità (<=)", value = 0.5, min = 0, max = 1, step = 0.01),
                                        
                                        actionButton("run_btn", "Esegui Analisi")
                                      ),
                                      
                                      mainPanel(
                                        tabsetPanel(
                                          tabPanel("Similarità Presenza/Assenza (Jaccard)",
                                                          tabsetPanel(
                                                            tabPanel("Complessivo",
                                                                            DT::dataTableOutput("jaccard_tot"),
                                                                            downloadButton("download_jaccard_tot_csv", "Scarica CSV"),
                                                                            downloadButton("download_jaccard_tot_xlsx", "Scarica XLSX")
                                                            ),
                                                            tabPanel("Sintetico",
                                                                            DT::dataTableOutput("jaccard_synth"),
                                                                            downloadButton("download_jaccard_synth_csv", "Scarica CSV"),
                                                                            downloadButton("download_jaccard_synth_xlsx", "Scarica XLSX")
                                                            )
                                                          )
                                          ),
                                          tabPanel("Similarità per abbondanza (Bray curtis)",
                                                          tabsetPanel(
                                                            tabPanel("Complessivo",
                                                                            DT::dataTableOutput("bray_tot"),
                                                                            downloadButton("download_bray_tot_csv", "Scarica CSV"),
                                                                            downloadButton("download_bray_tot_xlsx", "Scarica XLSX")
                                                            ),
                                                            tabPanel("Sintetico",
                                                                            DT::dataTableOutput("bray_synth"),
                                                                            downloadButton("download_bray_synth_csv", "Scarica CSV"),
                                                                            downloadButton("download_bray_synth_xlsx", "Scarica XLSX")
                                                            )
                                                          )
                                          ),
                                          tabPanel("CEP names",
                                                   DT::dataTableOutput("cep_tab"),
                                                   downloadButton("download_cep_csv", "Scarica CSV"),
                                                   downloadButton("download_cep_xlsx", "Scarica XLSX"))
                                        )
                                      )
                                    )
                    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  # UI dinamica per fogli xlsx
  observeEvent(input$file_tbd, {
    req(input$file_tbd)
    ext <- tools::file_ext(input$file_tbd$name)
    if (ext == "xlsx") {
      sheets <- readxl::excel_sheets(input$file_tbd$datapath)
      output$sheet_select_tbd <- renderUI({
        selectInput("sheet_tbd", "Seleziona foglio", choices = sheets)
      })
    } else {
      output$sheet_select_tbd <- renderUI({ NULL })
    }
  })
  
  observeEvent(input$file_ref, {
    req(input$file_ref)
    ext <- tools::file_ext(input$file_ref$name)
    if (ext == "xlsx") {
      sheets <- readxl::excel_sheets(input$file_ref$datapath)
      output$sheet_select_ref <- renderUI({
        selectInput("sheet_ref", "Seleziona foglio", choices = sheets)
      })
    } else {
      output$sheet_select_ref <- renderUI({ NULL })
    }
  })
  
  # Funzione per leggere dati da file
  read_data <- function(file, sheet = NULL) {
    req(file)
    ext <- tools::file_ext(file$datapath)
    if (ext == "csv") {
      readr::read_csv(file$datapath, show_col_types = FALSE)
    } else if (ext == "xlsx") {
      readxl::read_excel(file$datapath, sheet = sheet)
    } else {
      validate("Formato file non supportato.")
    }
  }
  
  # Analisi al click
  dati <- eventReactive(input$run_btn, {
    req(input$file_tbd, input$file_ref)
    
    ext_tbd <- tools::file_ext(input$file_tbd$name)
    ext_ref <- tools::file_ext(input$file_ref$name)
    
    tbd <- read_data(input$file_tbd, sheet = if (ext_tbd == "xlsx") input$sheet_tbd else NULL)
    ref <- read_data(input$file_ref, sheet = if (ext_ref == "xlsx") input$sheet_ref else NULL)
    
    FaciesIdentificator(
      tbd = tbd,
      ref = ref,
      prime10tbd = input$prime10,
      soglia = input$soglia,
      valore.soglia = input$valore_soglia
    )
  })
  
  # Output tabelle con filtro
  output$jaccard_tot <- DT::renderDataTable({ req(dati()); dati()$Jaccard$complessivo }, filter = "top", options = list(pageLength = 10))
  output$jaccard_synth <- DT::renderDataTable({ req(dati()); dati()$Jaccard$sintetico }, filter = "top", options = list(pageLength = 10))
  output$bray_tot <- DT::renderDataTable({ req(dati()); dati()$Bray$complessivo }, filter = "top", options = list(pageLength = 10))
  output$bray_synth <- DT::renderDataTable({ req(dati()); dati()$Bray$sintetico }, filter = "top", options = list(pageLength = 10))
  
  # Download handler Jaccard
  output$download_jaccard_tot_csv <- downloadHandler(
    filename = function() "Jaccard_complessivo.csv",
    content = function(file) readr::write_csv(dati()$Jaccard$complessivo, file)
  )
  output$download_jaccard_tot_xlsx <- downloadHandler(
    filename = function() "Jaccard_complessivo.xlsx",
    content = function(file) writexl::write_xlsx(dati()$Jaccard$complessivo, path = file)
  )
  output$download_jaccard_synth_csv <- downloadHandler(
    filename = function() "Jaccard_sintetico.csv",
    content = function(file) readr::write_csv(dati()$Jaccard$sintetico, file)
  )
  output$download_jaccard_synth_xlsx <- downloadHandler(
    filename = function() "Jaccard_sintetico.xlsx",
    content = function(file) writexl::write_xlsx(dati()$Jaccard$sintetico, path = file)
  )
  
  # Download handler Bray
  output$download_bray_tot_csv <- downloadHandler(
    filename = function() "Bray_complessivo.csv",
    content = function(file) readr::write_csv(dati()$Bray$complessivo, file)
  )
  output$download_bray_tot_xlsx <- downloadHandler(
    filename = function() "Bray_complessivo.xlsx",
    content = function(file) writexl::write_xlsx(dati()$Bray$complessivo, path = file)
  )
  output$download_bray_synth_csv <- downloadHandler(
    filename = function() "Bray_sintetico.csv",
    content = function(file) readr::write_csv(dati()$Bray$sintetico, file)
  )
  output$download_bray_synth_xlsx <- downloadHandler(
    filename = function() "Bray_sintetico.xlsx",
    content = function(file) writexl::write_xlsx(dati()$Bray$sintetico, path = file)
  )
  
  # Output CEP
  output$cep_tab <- DT::renderDataTable({
    req(dati())
    dati()$cepNames
  }, filter = "top", options = list(pageLength = 10))
  
  # Download handler CEP
  output$download_cep_csv <- downloadHandler(
    filename = function() "Classificazione_CEP.csv",
    content = function(file) readr::write_csv(dati()$cepNames, file)
  )
  output$download_cep_xlsx <- downloadHandler(
    filename = function() "Classificazione_CEP.xlsx",
    content = function(file) writexl::write_xlsx(dati()$cepNames, path = file)
  )
}

# ---- AVVIO APP ----
shinyApp(ui = ui, server = server)
