source("def.R")

ui <- fluidPage(
  
#### Spiele auswerten ####
# Übersicht Spiele aus Gesamttabelle mit gespielten und bewerteten Spielen
  tabsetPanel(
    tabPanel("Spiele auswerten",
#          tags$img(height=100, src="IMG-20171011-WA0010.jpg"),
           # Style für die Seite definieren:
           fluidRow(tags$style(HTML("
                                    .multicol { 
                                    height: 200px;
                                    -webkit-column-count: 2; /* Chrome, Safari, Opera */ 
                                    -moz-column-count: 2;    /* Firefox */ 
                                    column-count: 2; 
                                    -moz-column-fill: auto;
                                    -column-fill: auto;
                                    }
                                    div.checkbox {margin-top: 0px;}")),
                    # Datum begrenzen oder nur bestimmte Kategorien filtern
                    column(3,dateRangeInput(inputId = "dateaus",label="Datumsauswahl zuletzt gespielt",
                                            start="2007-09-01",end=Sys.Date(),language = "de", separator = " bis "),
                           checkboxInput("katwahl","Kategorien filtern"),
                           conditionalPanel(
                             condition = "input.katwahl == true",
                             selectInput("kat", "Kategorien aussuchen:",choices=kategorien,multiple=TRUE)
                           )
                    ),
                    # Variablen wählen, die angezeigt werden sollen:
                    column(5,tags$div(align = 'left', 
                                      class = 'multicol', checkboxGroupInput("vars","Variablen anzeigen:",choices=vars,selected=vars[1:3])))),
           # Button für Auswertung, daran Tabellenoutput anknüpfen:
           fluidRow(actionButton(inputId="auswert",label="Spiele auswerten"),
                    dataTableOutput("tabagg"))
           ),
    
### Gespielte Spiele eintragen ####
# Name: wer trägt ein, Datum: wann gespielt, Spiel: aus Liste wählen, Spieler auswählen, Gewinner darauf basierend eintragen
    tabPanel("Gespielte Spiele",
      fluidRow(
       column(3,selectInput("nameg","Name",choices=spieler)),
       column(5,dateInput("date","Gespielt am:",Sys.Date(),language = "de"))
      ),
      selectInput("spielg", "Spiel",choices=dats$Spiel, selected="Pandemic Legacy: Season 1"),
      fluidRow(
       column(3,checkboxGroupInput("spieler","Spieler",choices=c(spieler,"Andere"),selected=spieler)),
       column(5,uiOutput("winnerg"))
      ),
      # Button für Eintragung, darunter Tabelle anzeigen mit allen bisher eingetragenen Spielen
      actionButton(inputId="eintragen",label="Speichern"),
      dataTableOutput("tabg")
  ),

### Spiele bewerten ####
# Name: Wer bewertet, Bewertung (1-10), Spiel aus Liste wählen
  tabPanel("Spiele bewerten",
#      tags$img(height=50,src="spiele.jpg"),
      selectInput("nameb","Name",choices=spieler),
      checkboxInput("nbew2", "Nur nicht bewertete Spiele anzeigen"),
      uiOutput("sliderb"),
      sliderInput("bewert","Bewertung (1=schlecht bis 10=gut)",value = 5, min = 1, max = 10, step=1,round=TRUE,ticks=FALSE),      
      actionButton(inputId="bewerten",label="Bewerten"),
      dataTableOutput("tab")
    ),

### Daten herunterladen ####
    tabPanel("Download",
     selectInput("dataset", "Datensatz Auswahl:", choices = c("Bewertete Spiele" = "Spieleb", "Gespielte Spiele" = "Spieleg", "Spiele Metadaten" = "bgg")),  
     downloadButton("downloadData", "Download"),
     dataTableOutput("tabout")
    ),

### Daten hochladen ####
  tabPanel("Upload",
     fileInput("neudat", "Datensatz hochladen:",accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
     checkboxInput("trenn", "Trennzeichen verwenden"),
     conditionalPanel(
       condition = "input.trenn == true",
       textInput("sepneudat", "Trennzeichen angeben:", value=",")),
     dataTableOutput("tabin"),
     selectInput("datwahl", "Welcher Datensatz:", choices = c("Bewertete Spiele" = "Spieleb", "Gespielte Spiele" = "Spieleg", "Spiele Metadaten" = "bgg")),  
     actionButton("dazu", "Daten updaten"),
     actionButton("neu", "Daten ersetzen"),
     textOutput("done")
  ),

### Eintragung bei gespielten Spielen oder Bewertung korrigieren ####
# Datensatz bestimmen, Name: wessen Eintrag
tabPanel("Korrekturen",
         selectInput("datwahlk", "Welcher Datensatz:", choices = c("Gespielte Spiele" = "Spieleg", "Bewertete Spiele" = "Spieleb")),  
#         selectInput("namek","Name",choices=spieler),
         DT::dataTableOutput('datkorr'),
         actionButton("korr", "markierte Zeilen entfernen"),
         textOutput("donek")
)
  )
)
