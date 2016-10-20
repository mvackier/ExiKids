# Ensure are preloaded
library(data.table)
library(googlesheets)
suppressMessages(library(dplyr))

url <- "https://docs.google.com/spreadsheets/d/1oR0OsJ4VZHCIqbHD89jRf2zL9gHalGkLYoux1zSbya8/edit?usp=sharing"
# list all sheets
gs_ls()

# TODO : deel sommige sheets op Livias google
latReg <- gs_title("Latijn Vocabularium")
latReg

# opens in browser for editing
gs_browse(latReg)

woordDT <- as.data.table(gs_read(latReg, ws = "Woordjes"))
testDT <- woordDT[Module == "A",]
testDT[,NL:=""]
# in ander volgorde
testDT <- testDT[sample(1:nrow(testDT)),]

boring_ss <- gs_ws_delete(ss = latReg, ws = "Test")
boring_ss <- gs_ws_new(ss = latReg, ws_title = "Test2",  input = testDT)
gs_browse(boring_ss)

# assume filled in
# TODO : need to reread latReg?
resultDT <- as.data.table(gs_read(latReg, ws = "Test2"))

# merge
compDT <- merge(woordDT[,list(NL, LA)], resultDT[,list(NL, LA)], by="LA", suffixes = c("",".ans"))
compDT[,OK:=NL==NL.ans]
result <- sum(compDT$OK)*100/nrow(compDT)

sprintf("Resultaat is %f %%",result)
