source("def.R")

ui <- fluidPage(
  
#### Spiele auswerten ####
# Übersicht
  tabsetPanel(
    tabPanel("Spiele auswerten",
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
                    column(3,dateRangeInput(inputId = "dateaus",label="Datumsauswahl zuletzt gespielt",
                                            start="2007-09-01",end=Sys.Date(),language = "de", separator = " bis "),
                           checkboxInput("katwahl","Kategorien filtern"),
                           conditionalPanel(
                             condition = "input.katwahl == true",
                             selectInput("kat", "Kategorien aussuchen:",choices=kategorien,multiple=TRUE)
                           )
                    ),
#                    column(1,checkboxInput("katwahl","Kategorien filtern")),  
                    column(5,tags$div(align = 'left', 
                                      class = 'multicol', checkboxGroupInput("vars","Variablen anzeigen:",choices=vars,selected=vars[1:3])))),
           fluidRow(actionButton(inputId="auswert",label="Spiele auswerten"),
                    dataTableOutput("tabagg"))
           ),
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
      actionButton(inputId="eintragen",label="Speichern"),
      dataTableOutput("tabg")
  ),
  tabPanel("Spiele bewerten",
#      tags$img(height=50,src="spiele.jpg"),
      selectInput("nameb","Name",choices=spieler),
      checkboxInput("nbew2", "Nur nicht bewertete Spiele anzeigen"),
      uiOutput("sliderb"),
      sliderInput("bewert","Bewertung (1=schlecht bis 10=gut)",value = 5, min = 1, max = 10, step=1,round=TRUE,ticks=FALSE),      
      actionButton(inputId="bewerten",label="Bewerten"),
      dataTableOutput("tab")
    ),
    tabPanel("Download",
     selectInput("dataset", "Datensatz Auswahl:", choices = c("Bewertete Spiele" = "Spieleb", "Gespielte Spiele" = "Spieleg", "Spiele Metadaten" = "bgg")),  
     downloadButton("downloadData", "Download"),
     dataTableOutput("tabout")
    ),
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
tabPanel("Korrekturen",
         selectInput("datwahlk", "Welcher Datensatz:", choices = c("Gespielte Spiele" = "Spieleg", "Bewertete Spiele" = "Spieleb")),  
         selectInput("namek","Name",choices=spieler),
#         dateRangeInput(inputId = "datek",label="Datumsauswahl wann eingetragen",
#                        start="2017-09-01",end=Sys.Date(),language = "de", separator = " bis "),
         DT::dataTableOutput('datkorr'),
         actionButton("korr", "markierte Zeilen entfernen"),
         textOutput("donek")
)
  )
)
