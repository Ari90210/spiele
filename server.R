#setwd("H:/R-Files/Shiny/Spiele/")

server <- function(input, output) {
### Datensatz für Auswertung erstellen ####
  datagg <- eventReactive(input$auswert, {
    datb <- loadData("Spieleb.csv")
    datg <- loadData("Spieleg.csv")
    datb <- unique(datb, by=c("Spiel","Name"))
    dats <- loadData("bgg.csv")
    setkey(dats, Spiel)
    setkey(datb, Spiel)
    setkey(datg, Spiel)
    datb$Bewertung <- as.integer(datb$Bewertung)
    for(i in spieler){
      datg[,Gewinner2 := ifelse(Gewinner=="alle", Spieler, Gewinner)]
      ind <- grep(i, datg$Gewinner2)
      datg[,paste("Gewinner",i,sep=""):= 0]
      datg[ind,paste("Gewinner",i,sep="")] <- 1
    }
    dataggg <- datg[, list(AnzahlGespielt=.N,
                           ZuerstGespielt = as.character(min(as.Date(Datum))),
                           ZuletztGespielt=as.character(max(as.Date(Datum))),
                           #MeistensGewonnen=max(Gewinner),
                           GewinnerAri=sum(GewinnerAri),
                           GewinnerBasti=sum(GewinnerBasti),
                           GewinnerFlo=sum(GewinnerFlo),
                           GewinnerSabrina=sum(GewinnerSabrina)),
                    by = list(Spiel)]
    dataggg[,maxGewinn := max(c(GewinnerAri,GewinnerFlo,GewinnerBasti,GewinnerSabrina)),by=Spiel]
    dataggg[,MeistGewonnen := ""]
    for(i in spieler){
      ind <- ((dataggg[,paste("Gewinner",i,sep=""),with=FALSE]==dataggg[,maxGewinn])[,1] & dataggg[,maxGewinn] > 0)
      dataggg[ind, MeistGewonnen := paste(dataggg[ind,MeistGewonnen],i,sep="")]
    }
    dataggb <- datb[, list(Bewertung=round(mean(Bewertung),1),AnzahlBewert=length(unique(Name)),BewertungAri=Bewertung[Name=="Ari"],BewertungBasti=Bewertung[Name=="Basti"],BewertungFlo=Bewertung[Name=="Flo"],BewertungSabrina=Bewertung[Name=="Sabrina"]), by = list(Spiel)]
    datagg1 <- merge(dataggg,dataggb,all=TRUE)
    datagg <- merge(dats,datagg1)
    if(input$katwahl == TRUE) {
      ind <- 0
      for(i in input$kat){
        ind <- c(ind, grep(i,datagg$Kategorien))
      }
      ind <- unique(ind)
      datagg <- datagg[ind]
    }
    datred <- datagg[between(as.Date(ZuletztGespielt), input$dateaus[1], input$dateaus[2]) | is.na(ZuletztGespielt)]
    datred[,input$vars,with=FALSE]    
  })
  datoutb <- eventReactive(input$bewerten, {
    dat <- loadData("Spieleb.csv")
    dati <- data.frame(Timestamp=as.character(Sys.time()),Spiel=input$spielb,Name=input$nameb,Bewertung=input$bewert)
    datoutb <- rbind(dati,dat)
    datoutb
  })
  datoutg <- eventReactive(input$eintragen, {
    dat <- loadData("Spieleg.csv")
    dati <- data.frame(TimestampEintrag=as.character(Sys.time()),Spiel=input$spielg,Name=input$nameg,Datum=as.character(input$date),Gewinner=paste(input$winner, collapse = ","),Spieler=paste(input$spieler,collapse=", "))
    datoutg <- rbind(dati,dat)
    datoutg
  })
  observeEvent(input$eintragen,{
    saveData(datoutg(),"Spieleg.csv")
  })
  observeEvent(input$bewerten,{
    saveData(datoutb(),"Spieleb.csv")
  })
  datneu <- eventReactive((input$neu || input$dazu), {
    dat <- fread(input$neudat$datapath, colClasses="character", sep=input$sepneudat)
    # Variablen pruefen
    datalt <- loadData(paste(input$datwahl,".csv",sep=""))
    if(all(names(dat) == names(datalt))){
      if(input$neu){
        saveData(dat, file=paste(input$datwahl,".csv",sep=""))
        out <- "Daten ersetzt"
      }
      if(input$dazu){
        datd <- rbind(dat, datalt)
        if(input$datwahl != "bgg") datd <- unique(datd, by=c("Timestamp","Name"))
        if(input$datwahl == "bgg") datd <- unique(datd, by=c("ID.BGG"))
        saveData(datd, file=paste(input$datwahl,".csv",sep=""))
        out <- "Daten dazu"
      }
    }
    else {out <- "Daten haben nicht die korrekte Form"}
    print(out)
  })
  output$tab <- renderDataTable({
    input$bewerten
    datb <- loadData("Spieleb.csv")
    unique(datb, by=c("Spiel","Name"))
    })
  output$tabg <- renderDataTable({
    input$eintragen
    loadData("Spieleg.csv")})
  output$tabagg <- renderDataTable({
    datagg()})
  output$tabout <- renderDataTable({
    loadData(paste(input$dataset,".csv",sep=""))})
  output$tabin <- renderDataTable({
    req(input$neudat)
    dat <- fread(input$neudat$datapath, colClasses="character", sep=input$sepneudat)
    dat})
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(Sys.Date(), input$dataset,".csv", sep="")
    },
    content = function(file) {
#      write.table(fread(paste(input$dataset,".csv",sep="")), file, row.names = FALSE, sep=",")
      write.table(loadData(paste(input$dataset,".csv",sep="")), file, row.names = FALSE, sep=",")
    })
  output$done <- renderText({
    req((input$neu || input$dazu))
    datneu()})
  output$datkorr <- DT::renderDataTable({
    dat <- loadData(paste(input$datwahlk,".csv",sep=""))
#    datred <- dat[Name == input$namek]
#    datred <- dat[between(as.Date(Timestamp), input$datek[1], input$datek[2])]
    dat})
  observeEvent(input$korr, {
    ind <- input$datkorr_rows_selected
    dat <- loadData(paste(input$datwahlk,".csv",sep=""))
    dat2 <- dat[ind]
    test <- try(loadData(paste(Sys.Date(),"Spieleg",".csv",sep="")))
    if("try-error" %in% class(test)) {
      dat2 <- rbind(dat2,test)
    }
    saveData(dat2, file=paste(Sys.Date(),input$datwahlk,".csv",sep=""))
    dat <- dat[-ind]
    saveData(dat, file=paste(input$datwahlk,".csv",sep=""))
  })
  output$donek <- renderText({
    req(input$korr)
    print("Zeilen entfernt")})
  output$sliderb <- renderUI({
    spiele <- dats$Spiel
    if(input$nbew2 == TRUE){
          datb <- loadData("Spieleb.csv")
          datb <- datb[Name == input$nameb]
          spiele <- dats$Spiel[!dats$Spiel %in% datb$Spiel]
    }
    selectInput("spielb", "Spiel",choices=spiele)
  })
  output$winnerg <- renderUI({
    choices <- c("Unbekannt", input$spieler, "alle", "keiner")
    selectInput("winner","Gewinner",choices=choices, multiple = TRUE)
  })
}
