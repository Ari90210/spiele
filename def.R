## Definitionen und Packages f?r die App
#setwd("H:/R-Files/Shiny/Spiele/spieleapp")

library(shiny)
library(data.table)
library(dplyr)
library(rdrop2)

options(stringsAsFactors = FALSE)

encod <- "ISO-8859-1"
#encod <- "UTF-8"
outputDir <- "spiele" # Verzeichnis auf Dropbox
token <- readRDS("droptoken.rds")
drop_acc(dtoken = token)
saveData <- function(data, fileName) {
  filePath <- file.path(tempdir(), fileName)
  write.table(data, filePath, row.names = FALSE, sep=",",fileEncoding = encod)
  drop_upload(filePath, path = outputDir)
}
loadData <- function(file,sep=",") {
  data <- as.data.table(drop_read_csv(paste(outputDir,file,sep="/"),header=TRUE, colClasses="character", sep=sep,encoding = encod, fileEncoding = encod))
  data
}

dats <- loadData("bgg.csv")
setorder(dats, Spiel)
spieler <- c("Ari", "Basti", "Flo", "Sabrina")
vars <- c("Spiel","Bewertung","ZuletztGespielt","ZuerstGespielt","AnzahlGespielt","AnzahlBewert","Erscheinungsjahr","AnzSpielerMin","AnzSpielerMax","MinSpielzeit","MaxSpielzeit","AbAlter","Autor","Kategorien","Wem","BewertungAri","BewertungBasti","BewertungFlo","BewertungSabrina","GewinnerAri","GewinnerBasti","GewinnerFlo","GewinnerSabrina","MeistGewonnen")

kategorien <- unique(strsplit(paste(unique(dats$Kategorien), collapse=","),",")[[1]])
