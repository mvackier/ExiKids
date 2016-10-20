#suppressMessages(library(dplyr))
library(data.table)
library(googlesheets)

setwd("~/ExiKids/src")
# Broncode inladen
source("woordjesOefenen.r")

# oplijsten van alle werkboeken 
lijstWerkboekenOp()

# Verander hier in deze variabele welke werkboek je wil openen
werkBoekNaam <- "Latijn"
testNL <- TRUE

# Open het vocabularium
openVocabularium(werkBoekNaam=werkBoekNaam)

# Er zijn nog meer opties maar de voornaamse zijn hier opgegeven, de anderen krijgen een standaardwaarde.
maakToets(aantalWoordjes=8, 
          werkBoekNaam=werkBoekNaam,
          moduleOmVanTeSelecteren = "2.3")

# je kan ook module weglaten
maakToets(aantalWoordjes=4, 
          werkBoekNaam=werkBoekNaam)

# Nieuwe opties voor joepeltje

# wisNL=TRUE of FALSE zal ofwel NL ofwel de andere taal wissen
maakToets(aantalWoordjes=8, 
          wisNL=testNL,
          werkBoekNaam=werkBoekNaam)


# controleer resultaat
# indien controleerVrijeVelden = FALSE dan worden deze niet gecontroleerd
controleerToets(werkBoekNaam=werkBoekNaam,
                controleerVrijeVelden=FALSE,
                controleerNL=testNL)


