library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(stringr)

# Configurazione dell'app per shinyapps.io
options(shiny.sanitize.errors = TRUE)

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = "Tavoli da Gioco",
    titleWidth = 300
  ),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Generatore Tavoli", tabName = "generator", icon = icon("table")),
      menuItem("Informazioni", tabName = "info", icon = icon("info-circle"))
    ),
    div(
      style = "padding: 15px;",
      h4("Effetti Visivi", style = "color: white; margin-top: 20px;"),
      helpText("Personalizza la tua esperienza!", style = "color: #ddd;"),
      checkboxInput("showAnimation", "Mostra animazione", value = TRUE),
      tags$hr(style = "border-color: #555;")
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f8f9fa;
        }
        .box {
          border-radius: 10px;
          box-shadow: 0 2px 10px rgba(0, 0, 0, 0.1);
        }
        .box-header {
          border-radius: 10px 10px 0 0;
        }
        .box-body {
          padding: 20px;
        }
        .form-control {
          border-radius: 5px;
        }
        .btn {
          border-radius: 5px;
        }
        .table-box {
          margin-top: 15px;
          padding: 15px;
          border-radius: 8px;
          background-color: #fff;
          box-shadow: 0 1px 5px rgba(0,0,0,0.1);
          transition: all 0.3s ease;
        }
        .table-box.highlight {
          background-color: #f0f7ff;
          border-left: 5px solid #3c8dbc;
        }
        .table-title {
          font-weight: bold;
          margin-bottom: 10px;
          color: #2c3e50;
          border-bottom: 2px solid #3c8dbc;
          padding-bottom: 5px;
        }
        .help-text {
          color: #7f8c8d;
          font-style: italic;
          font-size: 0.9em;
          margin-top: 5px;
        }
        .error-message {
          color: #e74c3c;
          font-weight: bold;
          margin-top: 10px;
        }
        .success-message {
          color: #27ae60;
          font-weight: bold;
          margin-top: 10px;
        }
        .participants-list {
          max-height: 300px;
          overflow-y: auto;
          background-color: #f9f9f9;
          padding: 10px;
          border-radius: 5px;
          border: 1px solid #ddd;
        }
        .name-card {
          display: inline-block;
          margin: 5px;
          padding: 8px 15px;
          background-color: #3c8dbc;
          color: white;
          border-radius: 20px;
          font-weight: bold;
          box-shadow: 0 2px 5px rgba(0,0,0,0.2);
        }
        .animation-container {
          min-height: 150px;
          background-color: #f0f7ff;
          border-radius: 8px;
          padding: 20px;
          margin-bottom: 20px;
          text-align: center;
          border: 1px dashed #3c8dbc;
        }
        .participant-card {
          padding: 8px;
          margin: 5px 0;
          border-radius: 5px;
          background-color: #f1f8ff;
          border-left: 4px solid #3c8dbc;
          transition: background-color 0.3s;
        }
        .participant-card:hover {
          background-color: #e1f0ff;
        }
        .loading-spinner {
          margin: 20px auto;
          width: 40px;
          height: 40px;
          border: 4px solid rgba(0, 0, 0, 0.1);
          border-radius: 50%;
          border-top-color: #3c8dbc;
        }
      "))
    ),
    tabItems(
      tabItem(
        tabName = "generator",
        fluidRow(
          column(
            width = 6,
            box(
              width = NULL,
              title = "Inserisci Partecipanti",
              status = "primary",
              solidHeader = TRUE,
              textAreaInput(
                "names",
                "Lista dei nomi (uno per riga):",
                rows = 8,
                resize = "vertical",
                placeholder = "Mario\nGiulia\nMarco\nChiara\nLuca\nSofia"
              ),
              div(class = "help-text", "Inserisci un nome per riga. Possono essere nomi, cognomi o nickname."),
              hr(),
              textAreaInput(
                "tableSizes",
                "Dimensioni dei tavoli (numeri separati da virgole o spazi):",
                rows = 2,
                placeholder = "4,3,3"
              ),
              div(class = "help-text", "Inserisci il numero di persone per ogni tavolo. La somma deve corrispondere al numero totale di partecipanti."),
              hr(),
              div(
                class = "text-center",
                actionButton(
                  "generate",
                  "Genera Tavoli",
                  icon = icon("random"),
                  class = "btn-primary",
                  style = "padding: 10px 24px; font-size: 16px;"
                )
              ),
              div(id = "errorMsg", class = "error-message"),
              div(id = "successMsg", class = "success-message")
            )
          ),
          column(
            width = 6,
            box(
              width = NULL,
              title = "Risultati",
              status = "success",
              solidHeader = TRUE,
              div(id = "animationContainer", class = "animation-container", style = "display: none;",
                  h4("Mescolamento in corso...", class = "text-center"),
                  div(id = "shuffleArea")
              ),
              div(id = "loadingSpinner", class = "text-center", style = "display: none;",
                  div(class = "loading-spinner"),
                  p("Assegnazione dei tavoli in corso...")
              ),
              uiOutput("tablesOutput")
            )
          )
        )
      ),
      tabItem(
        tabName = "info",
        box(
          width = 12,
          title = "Informazioni sull'applicazione",
          status = "info",
          solidHeader = TRUE,
          HTML("
            <h3>Come utilizzare l'applicazione</h3>
            <p>Questa applicazione ti permette di distribuire casualmente un gruppo di persone in diversi tavoli da gioco.</p>
            <ol>
              <li><strong>Inserisci i nomi</strong> dei partecipanti, uno per riga, nel campo apposito.</li>
              <li><strong>Specifica le dimensioni dei tavoli</strong> (quante persone per ogni tavolo) separando i numeri con virgole o spazi.</li>
              <li>Assicurati che la somma delle dimensioni dei tavoli sia esattamente uguale al numero di partecipanti.</li>
              <li>Clicca su <strong>Genera Tavoli</strong> per creare una distribuzione casuale delle persone nei tavoli.</li>
            </ol>
            <h3>Esempio</h3>
            <p>Se hai 10 partecipanti e vuoi creare 3 tavoli con dimensioni 4, 3 e 3, l'applicazione distribuirà casualmente i partecipanti in questi tre tavoli.</p>
            <h3>Note</h3>
            <p>L'app utilizza un algoritmo di randomizzazione per garantire una distribuzione casuale ogni volta che viene eseguita.</p>
          ")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Nasconde i messaggi all'avvio
  observe({
    hide("errorMsg")
    hide("successMsg")
  })
  
  # Funzione per animare il processo di mescolamento in modo semplificato
  animateShuffle <- function(names_list) {
    if (!input$showAnimation) {
      show("tablesOutput")
      return()
    }
    
    # Mostra il contenitore dell'animazione
    show("animationContainer")
    hide("tablesOutput")
    
    # Genera l'HTML per i nomi in "carte"
    cards_html <- paste0(
      lapply(names_list, function(name) {
        paste0("<span class='name-card'>", name, "</span>")
      }),
      collapse = " "
    )
    
    # Aggiorna l'area di mescolamento
    html("shuffleArea", cards_html)
    
    # Usa setTimeout per mostrare il caricamento dopo l'animazione
    shinyjs::delay(2000, {
      hide("animationContainer")
      show("loadingSpinner")
      
      # Mostra i risultati dopo un'altra pausa
      shinyjs::delay(1500, {
        hide("loadingSpinner")
        show("tablesOutput")
        
        # Anima i tavoli con shinyjs
        runjs("
          $('.table-box').addClass('highlight');
          setTimeout(function() { 
            $('.table-box').removeClass('highlight'); 
          }, 1500);
        ")
      })
    })
  }
  
  # Reagisce al clic del pulsante
  observeEvent(input$generate, {
    # Reimposta i messaggi
    hide("errorMsg")
    hide("successMsg")
    hide("tablesOutput")
    hide("animationContainer")
    hide("loadingSpinner")
    
    # Ottiene e pulisce la lista dei nomi
    names_raw <- input$names
    names_list <- names_raw %>%
      str_split("\n") %>%
      unlist() %>%
      trimws() %>%
      .[. != ""]
    
    # Ottiene e pulisce le dimensioni dei tavoli
    table_sizes_raw <- input$tableSizes
    table_sizes <- table_sizes_raw %>%
      str_replace_all("[,;]", " ") %>%
      str_split("\\s+") %>%
      unlist() %>%
      trimws() %>%
      .[. != ""] %>%
      as.numeric()
    
    # Verifica che ci siano nomi
    if (length(names_list) == 0) {
      html("errorMsg", "Errore: inserisci almeno un nome.")
      show("errorMsg")
      return()
    }
    
    # Verifica che ci siano dimensioni dei tavoli
    if (length(table_sizes) == 0) {
      html("errorMsg", "Errore: inserisci almeno una dimensione per i tavoli.")
      show("errorMsg")
      return()
    }
    
    # Verifica che le dimensioni siano numeri positivi
    if (any(is.na(table_sizes)) || any(table_sizes <= 0)) {
      html("errorMsg", "Errore: le dimensioni dei tavoli devono essere numeri positivi.")
      show("errorMsg")
      return()
    }
    
    # Verifica che la somma delle dimensioni sia uguale al numero di nomi
    total_seats <- sum(table_sizes)
    if (total_seats != length(names_list)) {
      html("errorMsg", paste0(
        "Errore: la somma delle dimensioni dei tavoli (", total_seats, 
        ") non corrisponde al numero di nomi (", length(names_list), ")."
      ))
      show("errorMsg")
      return()
    }
    
    # Randomizza i nomi
    original_names <- names_list  # Per l'animazione
    set.seed(Sys.time()) # Usa il tempo come seed per garantire una distribuzione diversa ogni volta
    shuffled_names <- sample(names_list)
    
    # Crea i tavoli
    tables <- list()
    start_idx <- 1
    for (i in 1:length(table_sizes)) {
      end_idx <- start_idx + table_sizes[i] - 1
      tables[[i]] <- shuffled_names[start_idx:end_idx]
      start_idx <- end_idx + 1
    }
    
    # Mostra messaggio di successo
    html("successMsg", paste0(
      "Tavoli generati con successo! ", length(names_list), 
      " partecipanti distribuiti in ", length(tables), " tavoli."
    ))
    show("successMsg")
    
    # Aggiorna l'output con i nuovi tavoli
    output$tablesOutput <- renderUI({
      div(
        lapply(1:length(tables), function(i) {
          table_members <- tables[[i]]
          div(
            class = "table-box",
            div(class = "table-title", paste0("Tavolo ", i, " (", length(table_members), " persone)")),
            div(class = "participants-list",
                tags$ul(
                  lapply(table_members, function(name) {
                    tags$li(
                      div(class = "participant-card", name)
                    )
                  })
                )
            )
          )
        })
      )
    })
    
    # Mostra l'animazione
    animateShuffle(original_names)
  })
}

shinyApp(ui = ui, server = server)