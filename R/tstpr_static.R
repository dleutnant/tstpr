# tstpr.static -------------------------------------------------------------

.LUECKE <- as.numeric(c(194,189,240,125))
mode(.LUECKE)  <- "raw"

###Erlaubte Werte für einige Slots
.lDefart <- list("K","I","M")
.lAussage  <- list("Mes","Sum","Mit","Max","Min","DMax","DMin","Abl","Int","Dau","PDF","VF","Lck","Ant",
                   "OS","US","pSe","jSe","pVP","jVP","ERG","NER","jSN","pSN","Kon","XSy","YSy")
.lAussageGetDVal  <- list("Sum","Mit","Max","Min","DMax","DMin", "Lck")

.lHerkunft <- list("O","T","S","A","M","B","F","P")
.lReihenart <- list("Z","R")
.lQuelle <- list("L","S","H","P","T")
.lMESAUS  <- list("SUMLIN","SUML0", "DELTA", "INTENS")

### Slots für die Klassendefinition der Zeitreihe

.tstpZRID <- list(ZRID="")

.tstpAttributeIdent  <- list(Parameter="",
                             Ort="",
                             Subort="",
                             Defart="",
                             Aussage="",
                             XDistanz="",
                             XFaktor="",
                             Herkunft="",
                             Reihenart="",
                             Version="",
                             Quelle="",
                             ParMerkmal=""
)

.tstpAttributeDescrip  <- list(X="",
                               Y="",
                               GueltVon="",
                               GueltBis="",
                               Einheit="",                               
                               Messgenau="",
                               FToleranz="",
                               NWGrenze="",
                               Kommentar="",
                               Hoehe="",
                               YTyp="",
                               XEinheit="",
                               Publiziert="",
                               Hauptreihe=""
)

.tstpAttr <- c(.tstpZRID, .tstpAttributeIdent, .tstpAttributeDescrip, recursive=FALSE)

.tstpDFAttr  <- c("ZRID",
                  "PARAMETER",
                  "ORT",
                  "SUBORT",
                  "MAXFOCUS-Start",
                  "MAXFOCUS-End",
                  "VERSION",
                  "DEFART",
                  "REIHENART",
                  "QUELLE",
                  "AUSSAGE",
                  "HERKUNFT",
                  "XFAKTOR",
                  "XDISTANZ",
                  "PARMERKMAL",
                  "KOMMENTAR")