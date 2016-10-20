
# Gehardcodeerd waar de werkboeken staan
werkBoeken <- data.table(
  werkBoekNaam=c("Latijn","Frans", "Techniek","Nederlands"),
  urlWerkboek=c("https://docs.google.com/spreadsheets/d/1oR0OsJ4VZHCIqbHD89jRf2zL9gHalGkLYoux1zSbya8/edit?usp=sharing",
                "https://docs.google.com/spreadsheets/d/16KH9d0mrGqdWol2zskMqW8wiLohAdU7ZOxRph4S9-dM/edit?usp=sharing",
                "https://docs.google.com/spreadsheets/d/1K0ud3HYZc5ERTMr3Mc5dYhOv7pQQWFILVfpjIbO8V1E/edit?usp=sharing",
                "https://docs.google.com/spreadsheets/d/1880d0-E1uSZLUIWHQTEjw9htgKGD9Z7VvTSL04sgkmQ/edit?usp=sharing")
)

lijstWerkboekenOp <- function() {
  return(werkBoeken$werkBoekNaam)
}

getWerkBoekUrl <- function(werkBoekNaam) {
  werkBoekUrl <- werkBoeken$urlWerkboek[werkBoeken$werkBoekNaam==werkBoekNaam]
  if (length(werkBoekUrl)==0) stop("Werkboek ", werkBoekNaam, " is niet geregistreerd")
  # URL if not what is used.
  # see gs_ls() 
  # Do not reeber anynore how I registered it first.
  return(paste0(werkBoekNaam, " Vocabularium"))
}

detecteerVrijeVelden <- function(woordjesTabel, taalCode) {
  setdiff(names(woordjesTabel), c(taalCode, "NL", "module"))
}

#
# Wat doet deze functie?
# - Leest uit werkboek 'werkBoekNaam' het blad 'bladMetWoordjes'.
# - Selecteert willekeurig 'aantalWoordjes' woordjes (eventueel indien opgegeven enekel van een bepaalde module anders uit alle woordjes)
# - Maakt een blad 'bladMetToets' aan en voeget er de geselecteerde woordjes toe, maar wist het Nederlands (eventueel overschrijft als het blad reeds bestaat)
maakToets <- function(aantalWoordjes=10, 
                      moduleOmVanTeSelecteren, 
                      werkBoekNaam,
                      # Indien TRUE zal NL wissen andere de andere taal
                      wisNL=TRUE,
                      bladMetToets="Toets", 
                      bladMetWoordjes="Woordjes",
                      bladMetResultaat="Resultaat",
                      autoOpen=FALSE) {
  if (missing(werkBoekNaam)) stop("Specifieer een werkboekNaam, de volgende zijn geregistreerd ", paste0(werkBoeken$werkBoekNaam, collapse = ", "))
  
  taalCode <- toupper(substr(werkBoekNaam,1,2))
  
  # no strictly a url.
  werkBoekUrl <- getWerkBoekUrl(werkBoekNaam)
  
  require(data.table)
  require(googlesheets)
  werkboek <- gs_title(werkBoekUrl)
  woordjesTabel <- as.data.table(gs_read(werkboek, ws = bladMetWoordjes))
  
  
  # Backup maken
  save(woordjesTabel, file=paste0(werkBoekNaam,"_", Sys.Date(),".rdata"))
  
  # selecteer optionaal van 1 module
  if (missing(moduleOmVanTeSelecteren)==FALSE) {
    woordjesTabel <- woordjesTabel[module %in% moduleOmVanTeSelecteren,]
  } 
  
  wisTaalCode <- ifelse(wisNL, "NL", taalCode)
  woordjesTabel[[wisTaalCode]] <- ""
  
  vrijeVelden <- detecteerVrijeVelden(woordjesTabel, taalCode)
  # wis vrije velden
  sapply(vrijeVelden, function(vrijVeld) {
    woordjesTabel[[vrijVeld]] <<- ""
  })

  # Selecteer in willekeurige volgorde 'aantalWoordjes'
  # Indien er minder zijn selecteer all
  if (aantalWoordjes>nrow(woordjesTabel)) {
    warning("Er zijn geen ",aantalWoordjes, " woordjes beschikbaar, al degene die er zijn (=",nrow(woordjesTabel),") worden gegeven")
    aantalWoordjes <- nrow(woordjesTabel)
  }
  toetsTabel <- woordjesTabel[sample(size = aantalWoordjes, x = 1:nrow(woordjesTabel), replace = F),]
  
  # Toest wegschrijven in blad
  alleBladen <- gs_ws_ls(ss=werkboek)
  # Overschrijven als het al bestaat
  if (bladMetToets %in% alleBladen) {
    gs_ws_delete(ss = werkboek, ws = bladMetToets, verbose=F)
    #TODO : reread blijkbaar nodig of gs_ws_new denkt dat het al bestaat
    werkboek <- gs_title(werkBoekUrl)
  } 
  if (bladMetResultaat %in% alleBladen) {
   gs_ws_delete(ss = werkboek, ws = bladMetResultaat)
  }
  toets_blad <- gs_ws_new(ss = werkboek, ws_title = bladMetToets,  input = toetsTabel)
  # automatisch openen?
  if (autoOpen) gs_browse(toets_blad)
}

openVocabularium <- function(werkBoekNaam) {
  if (missing(werkBoekNaam)) stop("Specifieer een werkboekNaam, de volgende zijn geregistreerd ", paste0(werkBoeken$werkBoekNaam, collapse = ", "))
  
  latReg <- gs_title(getWerkBoekUrl(werkBoekNaam))
  # opens in browser om aan te passen
  gs_browse(latReg)
}



controleerToets <- function(werkBoekNaam,
                            controleerNL=TRUE,
                            controleerVrijeVelden=TRUE,
                            bladMetToets="Toets", 
                            bladMetWoordjes="Woordjes",
                            bladMetResultaat="Resultaat",
                            autoOpen=FALSE) {
  if (missing(werkBoekNaam)) stop("Specifieer een werkboekNaam, de volgende zijn geregistreerd ", paste0(werkBoeken$werkBoekNaam, collapse = ", "))

  taalCode <- toupper(substr(werkBoekNaam,1,2))
  # de taalcode die veronderstel wordt te verbeteren
  controleerTaalCode <- ifelse(controleerNL, "NL", taalCode)
  
  # Add Voc...
  werkBoekUrl <- getWerkBoekUrl(werkBoekNaam)
  
  require(data.table)
  require(googlesheets)
  
  werkboek <- gs_title(werkBoekUrl)
  woordjesTabel <- as.data.table(gs_read(werkboek, ws = bladMetWoordjes))
  toetsTabel <- as.data.table(gs_read(werkboek, ws = bladMetToets))
  toetsTabel[, module:=NULL]

  # merge
  vergelijkTabel <- merge(toetsTabel, woordjesTabel, by=taalCode, suffixes = c(".jij",""), all.x=T, all.y=F, allow.cartesian=F)
  print(vergelijkTabel)


  vrijeVelden <- detecteerVrijeVelden(woordjesTabel, taalCode)
  vergelijkVelden <- ifelse(controleerVrijeVelden, c(controleerTaalCode, vrijeVelden), controleerTaalCode)
  
  # Cleanup that all NAs are gone
  sapply(vergelijkVelden, function(veld) {
      # warning because have NA logical and set it to character
      suppressWarnings({
        eval(expr = parse(text=paste0("vergelijkTabel[is.na(",veld,".jij),",veld,".jij:='']")))
        eval(expr = parse(text=paste0("vergelijkTabel[is.na(",veld,"),",veld,":='']")))
      })
  })
  
  resultaat <- sapply(vergelijkVelden, function(veld) {
    tolower(vergelijkTabel[[veld]])==tolower(vergelijkTabel[[paste0(veld,".jij")]])
  })

  isResultaatOK <- apply(resultaat, MARGIN = 1, sum, na.rm=T)==length(vergelijkVelden)
  vergelijkTabel[,resultaat:=ifelse(isResultaatOK, "Juist","Fout")]
  
  
  attach(vergelijkTabel)
  puntenInPercent <- sum(isResultaatOK)*100/nrow(vergelijkTabel)
  finaalResultaat <- paste0(sum(isResultaatOK), "/", nrow(vergelijkTabel), " ofwel ",sprintf("%.2f",puntenInPercent)," %")
  detach(vergelijkTabel)

  vergelijkTabel <- rbindlist(list(vergelijkTabel, data.table(resultaat=finaalResultaat)), fill = T, use.names = TRUE)
  setcolorder(vergelijkTabel, c("module","resultaat",taalCode,"NL.jij","NL","genus.jij","genus","type.jij","type","stam_ww.jij","stam_ww","stam_zn_2de_klasse.jij","stam_zn_2de_klasse"))
  
  
  print(vergelijkTabel)
  
  cat("Resultaat is ",finaalResultaat, "\n")

  # Overschrijf toets met resultaat
  # Toest wegschrijven in blad
  alleBladen <- gs_ws_ls(ss=werkboek)
  # Overschrijven als het al bestaat
  if (bladMetResultaat %in% alleBladen) {
    gs_ws_delete(ss = werkboek, ws = bladMetResultaat)
    #TODO : reread blijkbaar nodig of gs_ws_new denkt dat het al bestaat
    werkboek <- gs_title(werkBoekUrl)
  } 
  resultaat_blad <- gs_ws_new(ss = werkboek, ws_title = bladMetResultaat,  input = vergelijkTabel)
  
  # automatisch openen?
  if (autoOpen) gs_browse(resultaat_blad)
}

