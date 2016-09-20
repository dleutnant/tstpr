#' Queries the tstp server
#'
#' An attribute pattern is sent to the tstp server. All time series matching the
#' pattern are returned to the client as a time series list together with all 
#' attributes and a number identifying it clearly (TSID) in XML format.
#' 
#' @title tstp_query
#' @param con A tstp_connection object.
#' @param ort query parameter (optional).
#' @param subort query parameter (optional).
#' @param param query parameter (optional).
#' @param defart query parameter (optional).
#' @param aussage query parameter (optional).
#' @param xdistanz query parameter (optional).
#' @param xfaktor query parameter (optional).
#' @param herkunft query parameter (optional).
#' @param reihenart query parameter (optional).
#' @param version query parameter (optional).
#' @param quelle query parameter (optional).
#' @param hauptreihe query parameter (optional).
#' @param zweck query parameter (optional).
#' @param ZRID query parameter (optional).
#' @param verbose logical. Provide additional details?
#' @param verbose.cache logical. Provide additional details of tstp cache?
#' @param debug logical. For debugging purposes only.
#' @param pm logical. For performance meausurement only.
#' @param all.attributes logical. Should all attributes be returned?
#' @return A data.frame containing query results.
#' @rdname tstp_query
#' @export
#' @author Dominik Leutnant
tstp_query <- function(con, ort=NULL, subort=NULL, param=NULL, defart=NULL, 
                       aussage=NULL, xdistanz=NULL, xfaktor=NULL, herkunft=NULL,
                       reihenart=NULL, version=NULL, quelle=NULL, hauptreihe=NULL,
                       zweck=NULL, ZRID=NULL,
                       verbose=FALSE, verbose.cache=FALSE, debug=FALSE, pm=FALSE,
                       all.attributes=FALSE) {
  
  .CheckClassObject(con)
  
  performance <- pm
  
  if (performance) print(paste(lubridate::now(), " tstp_query start")); 
  
  command <- "?Cmd=Query"
  if (!is.null(ort)) command  <- paste0(command,"&Ort=",ort)
  if (!is.null(subort)) command  <- paste0(command,"&Subort=",subort)
  if (!is.null(param)) command  <- paste0(command,"&Parameter=",param)
  if (!is.null(defart)) command  <- paste0(command,"&DefArt=",defart)
  if (!is.null(aussage)) command  <- paste0(command,"&Aussage=",aussage)
  if (!is.null(xdistanz)) command  <- paste0(command,"&Xdistanz=",xdistanz)
  if (!is.null(xfaktor)) command  <- paste0(command,"&Xfaktor=",xfaktor)
  if (!is.null(herkunft)) command  <- paste0(command,"&Herkunft=",herkunft)
  if (!is.null(reihenart)) command  <- paste0(command,"&ReihenArt=",reihenart)
  if (!is.null(version)) command  <- paste0(command,"&Version=",version)
  if (!is.null(quelle)) command  <- paste0(command,"&Quelle=",quelle)
  if (!is.null(hauptreihe)) command  <- paste0(command,"&Hauptreihe=", hauptreihe)
  if (!is.null(zweck)) command  <- paste0(command,"&Zweck=", zweck)
  if (!is.null(ZRID)) command  <- paste0(command,"&ZRID=", ZRID)
  command <- paste0(.BuildServerString(con), command)
  
  if (verbose) print(command)
  
  if (performance) print(paste(lubridate::now(), " before GET")); 
  res <- httr::GET(command, httr::authenticate(con@user,con@pass))
  if (performance) print(paste(lubridate::now(), " after GET")); 
  if (res$status_code != 200) stop(paste0("Status code ",res$status_code))
  
  if (performance) print(paste(lubridate::now(), " xmlParse")); 
  res <- XML::xmlParse(rawToChar(res$content))

  # if (debug) debug_query <<- res
  
  if (performance) print(paste(lubridate::now(), " xmlToDataFrame")); 
  
  dfTS  <- tryCatch( XML::xmlToDataFrame(res,
                         #colClasses = colClasses.tstp.toc,
                         homogeneous = TRUE,
                         stringsAsFactors = FALSE,
                         collectNames = FALSE) ,
                     error = function(e) data.frame(text = c("nichts gefunden") ) 
                     )
  
  # type conversion
  dfTS <- suppressWarnings(.ColumnTypeConversion(df = dfTS))

  if (performance) print(paste(lubridate::now(), " end")); 
  
  if ("text" %in% colnames(dfTS)) {
    warning("nothing found")
    return(NULL)
  } else {
    #sorting dataframe
    if (nrow(dfTS) > 1) dfTS <- dfTS[order(dfTS$ORT, dfTS$PARAMETER, dfTS$VERSION),]
    #reordering dataframe
    if (!all.attributes) dfTS <- dfTS[.tstpDFAttr]
    if (verbose.cache) print(paste0(nrow(dfTS), " item(s) in cache"))
    invisible(dfTS)
  }
}

#' Gets time series data from the tstp server
#'
#' Gets time series data from the tstp server (datetime-value pairs).
#' Use GETCOMBO to get texts (datetime-string pairs). A time series is requested by
#' passing a TSID argument. In addition, a time interval with 
#' Von=<Point in time> and Bis=<Point in time> (from and to)  must be specified,
#' optionally a quality level with Qual=[0..47] can be specified. The highest 
#' quality level used is selected as default. A Base64 encoded binary block 
#' containing the list of the datetime-value pairs is delivered. 'Time reference',
#' 'type of series' and 'unit' are written as XML attributes of the DEF element. 
#' Alternatively, the transmission can be carried out as ASCII which is 
#' selected via the parameter type=asc.
#' 
#' @title tstp_get
#' @param con A tstp_connection object.
#' @param tsid The timeseries ID. 
#' @param from Specifies the first date (\%Y-\%m-\%d).
#' @param to Specifies the last date (\%Y-\%m-\%d).
#' @param qual Sets the level of quality
#' @param type Determines the way data are transmitted ('asc' or 'bin').
#' @param verbose logical. Provide additional details?
#' @param debug logical. For debugging purposes only.
#' @param pm logical. For performance meausurement only.
#' @return A time series as an xts object.
#' @rdname tstp_get
#' @export
#' @author Dominik Leutnant
#' @seealso \code{\link[xts]{xts}}
tstp_get <- function(con,
                     tsid,
                     from=NULL, 
                     to=NULL, 
                     qual=1,
                     type='asc',
                     verbose=TRUE, 
                     debug=FALSE, 
                     pm=FALSE) {
  
  .CheckClassObject(con)
  
  performance <- pm
  
  if (performance) print(paste(lubridate::now(), " tstp_get start")); 
  if (missing(tsid)) stop("no id")
  if (!type %in% c('asc','bin')) stop(paste0("bad type: ", type))
  
  if (performance) print(paste(lubridate::now(), " tstp_query(con)")); 
  if (is.null(from) | is.null(to)) toc  <- tstp_query(con, ZRID = tsid)
  if (is.null(from)) {from <- toc[1,c('MAXFOCUS-Start')]}
  if (is.null(to)) {to <- toc[1,c('MAXFOCUS-End')]}
    
  if (performance) print(paste(lubridate::now(), " build query")); 
  command  <- "?Cmd=Get"
  command  <- paste0(command, "&ZRID=",tsid)
  command <- paste0(command,"&Von=",from)
  command <- paste0(command,"&Bis=",to)
  command  <- paste0(command,"&Qual=",qual)
  command  <- paste0(command,"&typ=",type)
  command <- paste0(.BuildServerString(con),command)
  
  if (debug) print(command)
  
  if (performance) print(paste(lubridate::now(), " before GET")); 
  res <- httr::GET(command, httr::authenticate(con@user, con@pass))
  
  if (performance) print(paste(lubridate::now(), " after GET")); 
  
  #if (debug) debug_get <<- res
  
  if (res$status_code != 200) stop(paste0("Status code ",res$status_code))
  
  #write(rawToChar(res$content), paste0("D:/error_xml_", type, ".xml"))
  if (performance) print(paste(lubridate::now(), " xmlTreeParse")); 
  doc <- XML::xmlTreeParse(rawToChar(res$content), options = XML::HUGE)
  
  if (performance) print(paste(lubridate::now(), " xmlRoot")); 
  r <- XML::xmlRoot(doc)
  
  data <- XML::xmlValue(r[[2]])
  ## TO DO: flow/height relationship currently not supported!

  if (type == 'asc') {
    
    if (performance) print(paste(lubridate::now(), " read.table")); 
    data <- utils::read.table(text = data, sep = ' ', stringsAsFactors = FALSE)

    colnames(data) <- c("index","data")
    if (performance) print(paste(lubridate::now(), " as.POSIXct")); 
    index  <-  as.POSIXct(data$index, format = "%Y-%m-%dT%H:%M:%SZ", tz = "GMT")
    if (performance) print(paste(lubridate::now(), " as.numeric")); 
    data <- as.numeric(data$data)
    if (performance) print(paste(lubridate::now(), " xts")); 
    ts <- xts::xts(data,index)
    if (performance) print(paste(lubridate::now(), " ts")); 
  } 
  
  else {
    data <- as.numeric(base64enc::base64decode(data))
    ts <- .ByteMatrixToXTS(data)
  }

  if (performance) print(paste(lubridate::now(), " tstp_assignattr")); 
  ts <- tstp_assignattr(con = con, xts = ts, tsid = tsid)
  
  if (performance) print(paste(lubridate::now(), " verbose")); 
  if (verbose) message( paste0("site: ", attr(ts, "Ort"),
                               ", parameter: ", attr(ts, "Parameter"),
                               ", Version: ", attr(ts, "Version"), 
                               ", elements: ", length(ts), 
                               ", object.size: ", utils::object.size(ts) ) )
  
  if (performance) print(paste(lubridate::now(), " return")); 
  invisible(ts)
  
}

#' Gets time series data from the tstp server (including texts)
#'
#' The time series server stores three types of data lists: the list with numbers, the 
#' list with texts and the list with isolated points. GETCOMBO gets all three 
#' data lists in one single request. GETCOMBO behaves like GET.
#' The return comprises three consecutive TSD elements. The first is for numbers,
#' the second for texts, the third for isolated points. In addition to the 
#' parameters of the GET command (VON, BIS, QUAL) there is READMODE, 
#' that can take INTERPOLIERT (interpolated), INNEN (inside) or AUSSEN
#' (outside). The default is INTERPOLIERT.
#' 
#' @title tstp_getcombo
#' @param con A tstp_connection object.
#' @param tsid The timeseries ID. 
#' @param from Specifies the first date (\%Y-\%m-\%d).
#' @param to Specifies the last date (\%Y-\%m-\%d).
#' @param qual Sets the level of quality.
#' @param readmode Readmode.
#' @param debug logical. For debugging purposes only.
#' @return A list of two xts objects.
#' @rdname tstp_getcombo
#' @export
#' @author Dominik Leutnant
#' @seealso \code{\link[xts]{xts}}
tstp_getcombo <- function(con,
                          tsid,
                          from = NULL, 
                          to = NULL, 
                          qual = 1,
                          readmode = c('INTERPOLIERT','INNEN', 'AUSSEN'),
                          debug = FALSE) {
  
  .CheckClassObject(con)
  
  message("WARNING: tstp_getcombo is currently unstable and throws an error if
          CDATA section is too big...")
  
  if (missing(tsid)) stop("no id")
  if (!readmode %in% c('INTERPOLIERT','INNEN','AUSSEN')) stop( paste0(
    "bad readmode: ", readmode))
  
  if (is.null(from) | is.null(to)) toc  <- tstp_query(con, ZRID = tsid)
  if (is.null(from)) {from <- toc[1,c('MAXFOCUS-Start')]}
  if (is.null(to)) {to <- toc[1,c('MAXFOCUS-End')]}
  
  command  <- "?Cmd=GetCombo"
  command  <- paste0(command, "&ZRID=",tsid)
  if (!is.null(from)) command <- paste0(command,"&Von=",from)
  if (!is.null(to)) command <- paste0(command,"&Bis=",to)
  command  <- paste0(command,"&Qual=",qual)
  command <- paste0(command, "&READMODE=", readmode)
  
  command <- paste0(.BuildServerString(con),command)
  print(command)
  
  res <- httr::GET(command, httr::authenticate(con@user,con@pass))
  
  #if (debug) debug_getcombo <<- res
  
  xmlstr <- rawToChar(res$content)
  
  # removing first line
  xmlstr <- base::substring(xmlstr,first = 45, last = nchar(xmlstr))
  # parsing xmlstring
    
  ## xmlstr <- xmlTreeParse(xmlstr, options=HUGE) ## OPTIONS HUGE?!?!
  xmlstr <- XML::xmlParseString(xmlstr) ## OPTIONS HUGE?!?!
  
  # extracting ANZ attribute of data and text 
  anz.data <- as.numeric(XML::xpathSApply(xmlstr,'//TSD/DEF',
                                          function(x) XML::xmlAttrs(x)['ANZ'])[1])
  
  anz.text <- as.numeric(XML::xpathSApply(xmlstr,'//TSD/DEF',
                                          function(x) XML::xmlAttrs(x)['ANZ'])[2])
  
  # extracting xmlValue
  xmlstr <-  XML::xpathSApply(xmlstr,'//TSD', XML::xmlValue)
  ## removing /n /t 
  xmlstr <- gsub('[\n\t]', '', xmlstr)
  
  if (anz.data > 0) {
    data <- as.numeric(base64enc::base64decode(xmlstr[1]))
    ts.data <- .ByteMatrixToXTS(data)
    ts.data <- tstp_assignattr(con = con, xts = ts.data, tsid = tsid)
  } else {
    warning("no data found on tsid")
    ts.data <- NA
  }
  
  if (anz.text > 0) {
    text <- as.numeric(base64enc::base64decode(xmlstr[2]))
    ts.text <- .StrByteVectorToXTS(text,items = anz.text)
    ts.text <- tstp_assignattr(con = con, xts = ts.text, tsid = tsid)
  } else {
    warning("no text found on tsid")
    ts.text  <- NA
  }
    
  ts.list <- list(ts.data,ts.text)
  
  invisible(ts.list)
  
}

#' Puts an xts-object to the tstp server
#'
#' A time series is selected by means of a TSID. Optionally, a quality 
#' level can be specified. The data is then submitted to the tstp server by POST
#' in an XML message and integrated on the server.
#' 
#' @title tstp_put
#' @param con A tstp_connection object.
#' @param xts An xts object to be uploaded.
#' @param text logical. Does the xts object contain characters?
#' @return The server response is returned.
#' @rdname tstp_put
#' @export
#' @author Dominik Leutnant
tstp_put <-  function(con, xts, text=FALSE) {
  
  .CheckClassObject(con)
  
  if (!attr(xts,"Defart") %in% .lDefart) stop("attribute 'Defart' is missing")
  if (!attr(xts,"Reihenart") %in% .lReihenart) stop("attribute 'Reihenart' is missing")
  command  <- "/?Cmd=Put"
  command  <- paste0(command, "&ZRID=", attr(xts,"ZRID"))
  command <- paste0(.BuildServerString(con),command)
  
  binxml <- .xts_to_binxml(xts, text)
  
  response <- httr::POST(command, 
                         httr::authenticate(con@user,con@pass),
                         body = binxml,
                         httr::add_headers(.headers = c("Content-Length" = nchar(binxml),
                                                        "Connection" = "Close")))
  
  response <- XML::xmlParse(rawToChar(response$content))
  
  return(response)
}

#' Assigns tstp attributes to an xts-object
#' 
#' Assign tstp attributes to the xts-object provided. 
#' 
#' @title tstp_assignattr
#' @param con A tstp_connection object.
#' @param xts An xts object atttributes are assigned to.
#' @param tsid The timeseries ID.
#' @return An xts object with assigned attributes.
#' @rdname tstp_assignattr
#' @export
#' @author Dominik Leutnant
#' @seealso \code{\link[xts]{xts}}
tstp_assignattr <- function(con, xts, tsid) {
  
  att <- tstp_query(con, ZRID = tsid, all.attributes = TRUE)

  xts::xtsAttributes(xts)  <- .tstpAttr
  
  # set Ident Attributes
  attr(xts,'ZRID')       <- as.character(att$ZRID)
  attr(xts,'Parameter')  <- as.character(att$PARAMETER)
  attr(xts,'Ort')        <- as.character(att$ORT)
  attr(xts,'Defart')     <- as.character(att$DEFART)
  attr(xts,'Aussage')    <- as.character(att$AUSSAGE)
  attr(xts,'XDistanz')   <- as.character(att$XDISTANZ)
  attr(xts,'XFaktor')    <- as.numeric(att$XFAKTOR)
  attr(xts,'Herkunft')   <- as.character(att$HERKUNFT)
  attr(xts,'Reihenart')  <- as.character(att$REIHENART)
  attr(xts,'Version')    <- as.numeric(att$VERSION)
  attr(xts,'Quelle')     <- as.character(att$QUELLE)
  attr(xts,'X')          <- as.numeric(att$X)
  attr(xts,'Y')          <- as.numeric(att$Y)
  attr(xts,'GueltVon')   <- as.character(att$GUELTVON)
  attr(xts,'GueltBis')   <- as.character(att$GUELTBIS)
  attr(xts,'Einheit')    <- as.character(att$EINHEIT)
  attr(xts,'Messgenau')  <- as.numeric(att$MESSGENAU)
  attr(xts,'FToleranz')  <- as.numeric(att$FTOLERANZ)
  attr(xts,'NWGrenze')   <- as.numeric(att$NWGRENZE)
  attr(xts,'Subort')     <- as.character(att$SUBORT)
  attr(xts,'Kommentar')  <- as.character(att$KOMMENTAR)
  attr(xts,'Hoehe')      <- as.numeric(att$HOEHE)
  attr(xts,'YTyp')       <- as.character(att$YTYP)
  attr(xts,'XEinheit')   <- as.character(att$XEINHEIT)
  attr(xts,'Publiziert') <- as.character(att$PUBLIZIERT)
  attr(xts,'ParMerkmal') <- as.character(att$PARMERKMAL)
  attr(xts,'Hauptreihe') <- as.character(att$HAUPTREIHE)
  attr(xts,'Zweck')      <- as.character(att$ZWECK)
  
  xtswithattributes  <- xts

  invisible(xtswithattributes)
}

#' Sets attributes of a timeseries
#'
#' An attribute of the time series selected by TSID is set. Identification 
#' attributes can not be set. Besides attributes you may pass INFO und 
#' LEBENSLAUF to set this fields of the time series.
#' 
#' @title tstp_setattr
#' @param con A tstp_connection object.
#' @param tsid The timeseries ID.
#' @param attribute The attribute to be set.
#' @param value The attribute's value.
#' @param verbose logical. Should informative outputs printed during function evaluation?
#' @return The server response is returned.
#' @rdname tstp_setattr
#' @export
#' @author Dominik Leutnant
#' @seealso \code{\link[xts]{xts}}
tstp_setattr <- function(con, tsid, attribute, value, verbose=FALSE){
  
  .CheckClassObject(con)
  
  if (length(attribute) != length(value)) stop("unequal length of input vector")
  
  command  <- "?Cmd=SetAttr"
  command  <- paste0(command, "&ZRID=",tsid)
  
  for (i in 1:length(attribute)) {
    if (!attribute[i] %in% names(.tstpAttributeDescrip)) {
      if (!attribute[i] == "INFO") {
        if (!attribute[i] == "LEBENSLAUF") {
          if (!attribute[i] == "Writable") stop(paste0("bad attribute: ",
                                                       attribute[i])) 
        }
      }
    }
    command <- paste0(command,"&Attr=",utils::URLencode(attribute[i], reserved = TRUE))
    command <- paste0(command,"&Wert=",utils::URLencode(value[i], reserved = TRUE))
    command <- paste0(.BuildServerString(con),command)
    if (verbose) print(command)
    response <- httr::GET(command, httr::authenticate(con@user,con@pass))
    response <- XML::xmlParse(rawToChar(response$content))
    response <- XML::xpathApply(response,path = "/TSR",XML::xmlValue)[[1]]
  }  
  
  return(response)
}

#' Deletes attributes of a timeseries
#'
#' Deletes tstp attibutes of the xts-object provided.
#' 
#' @title tstp_clearattr
#' @param xts An xts object to be cleared.
#' @return An xts object with empty tstp attributes.
#' @rdname tstp_clearattr
#' @export
#' @author Dominik Leutnant
#' @seealso \code{\link[xts]{xts}}
tstp_clearattr <- function(xts) {
    
  # set Ident Attributes
  attributes(xts)$ZRID        <- ""
  attributes(xts)$Parameter   <- ""
  attributes(xts)$Ort         <- ""
  attributes(xts)$Subort      <- ""
  attributes(xts)$Defart      <- ""
  attributes(xts)$Aussage     <- ""
  attributes(xts)$XDistanz    <- ""
  attributes(xts)$XFaktor     <- ""
  attributes(xts)$Herkunft    <- ""
  attributes(xts)$Reihenart   <- ""
  attributes(xts)$Version     <- ""
  attributes(xts)$Quelle      <- ""
  
  # set Descr Atttributes 
  attributes(xts)$X           <- ""
  attributes(xts)$Y           <- ""
  attributes(xts)$Hoehe       <- ""
  attributes(xts)$Messgenau   <- ""
  attributes(xts)$FToleranz   <- ""
  attributes(xts)$NWGrenze    <- ""
  attributes(xts)$Einheit     <- ""
  attributes(xts)$Kommentar   <- ""
  attributes(xts)$Publiziert  <- ""
  attributes(xts)$Hautpreihe  <- ""
  attributes(xts)$Zweck       <- ""
  
  return(xts)

}

#' Creates a timeseries
#'
#' A complete attribute set encoded as URL is submitted. A TSID of the series 
#' which was created (or found in the database) is returned. In case the 
#' creation failed, the TSID obtains the value 0.
#' 
#' @title tstp_create
#' @param con A tstp_connection object.
#' @param xts An xts object with tstp attributes.
#' @return A TSID (character) or 0.
#' @rdname tstp_create
#' @export
#' @author Dominik Leutnant
#' @seealso \code{\link[xts]{xts}}
tstp_create <- function(con, xts) {
  
  .CheckClassObject(con)
  
  command <- "?Cmd=Create"
  att <- xts::xtsAttributes(xts)[xts::xtsAttributes(xts) != ""]
  att  <- att[names(att) %in% names(.tstpAttr)] #vorher: .tstpAttributeIdent
  if (!all(c("Parameter", "Ort", "Subort", "Defart", "Reihenart") %in% names(att)) ) {
    stop("Missing tstp attributes: at least 'Parameter', 'Ort', 'Subort,
         'Defart', 'Reihenart' are required.")
  }
  namevalue <- paste0("&",names(att),"=", sapply(as.character(att), 
                                                 utils::URLencode, 
                                                 reserved = TRUE),
                      collapse = "")
  
  command <- paste0(command, namevalue)
  command <- paste0(.BuildServerString(con),command)
  print(command)
  res <- httr::GET(command, httr::authenticate(con@user,con@pass))
  tmp <- XML::xmlParse(rawToChar(res$content))
  tsid <- base::substr(XML::xpathApply(doc = tmp, path = "/TSR/TSATTR",
                                       XML::xmlValue),
                 6,
                 nchar(XML::xpathApply(doc = tmp, path = "/TSR/TSATTR",
                                       XML::xmlValue)))
  return(tsid)
}

#' Deletes a timeseries
#'
#' The TSID of the time series to be deleted is submitted.
#' 
#' @title tstp_delete
#' @param con A tstp_connection object.
#' @param tsid The timeseries ID.
#' @return The server response is returned.
#' @rdname tstp_delete
#' @export
#' @author Dominik Leutnant
tstp_delete <-  function(con, tsid) {
  
  for (i in 1:length(tsid)) {
    command  <- "?Cmd=Delete"
    command  <- paste0(command, "&ZRID=",tsid[i])
    command <- paste0(.BuildServerString(con),command)
    result <- httr::GET(command, httr::authenticate(con@user,con@pass))
    print(result)
  }
  invisible(result)
}

#' Deletes the quality level
#' 
#' A TSID, a time interval and a quality level are submitted. 
#' The quality is deleted in the interval range.
#' 
#' @param con A tstp_connection object.
#' @param tsid The timeseries ID.
#' @param qual Sets the level of quality.
#' @param from Specifies the first date (\%Y-\%m-\%d).
#' @param to Specifies the last date (\%Y-\%m-\%d).
#' @return The server response is returned.
#' @rdname tstp_deletequal
#' @export
#' @author Dominik Leutnant
tstp_deletequal <-  function(con, tsid, qual, from=NULL, to=NULL) {
  
  .CheckClassObject(con)
  
  if (is.null(from) | is.null(to)) toc  <- tstp_query(con, ZRID = tsid)
  if (is.null(from)) {from <- toc[1,c('MAXFOCUS-Start')]}
  if (is.null(to)) {to <- toc[1,c('MAXFOCUS-End')]}
  
  command  <- "?Cmd=DeleteQUAL"
  command  <- paste0(command, "&ZRID=",tsid)
  command <- paste0(command,"&Von=",from)
  command <- paste0(command,"&Bis=",to)
  command <- paste0(command,"&=Qual",qual)
  command <- paste0(.BuildServerString(con),command)
  result <- httr::GET(command, httr::authenticate(con@user,con@pass))

  return(result)
}

#' Inspects a timeseries
#'
#' Supply a time series id (tsid) and optionally a focus. Returned is the
#' maximal quality and the maximal physical quality on focus, the vita 
#' (Lebenslauf), the INFO field of the time series and the time of the last 
#' modification. Without a focus the maximal quality and maximal physical 
#' quality refer to the maxfocus of the time series.
#' 
#' @title tstp_inspect
#' @param con A tstp_connection object.
#' @param tsid The timeseries ID.
#' @param from Specifies the first date (\%Y-\%m-\%d).
#' @param to Specifies the last date (\%Y-\%m-\%d).
#' @return The server response is returned.
#' @rdname tstp_inspect
#' @export
#' @author Dominik Leutnant
tstp_inspect <- function(con, tsid, from=NULL, to=NULL) {
  
  .CheckClassObject(con)
  
  command  <- "?Cmd=Inspect"
  command  <- paste0(command, "&ZRID=",tsid)
  
  if (is.null(from) | is.null(to)) toc  <- tstp_query(con, ZRID = tsid)
  if (is.null(from)) {from <- toc[1,c('MAXFOCUS-Start')]}
  if (is.null(to)) {to <- toc[1,c('MAXFOCUS-End')]}
  
  command <- paste0(command,"&Von=",from)
  command <- paste0(command,"&Bis=",to)
  command <- paste0(.BuildServerString(con),command)
  result <- httr::GET(command, httr::authenticate(con@user,con@pass))
  return(result)
}

#' Queries the number of values
#' 
#' Supply a time series id (tsid) and optionally a focus. 
#' Returned is the number of quants (values) of the time series on focus. 
#' Without focus the total number is returned.
#' 
#' @title tstp_qnum
#' @param con A tstp_connection object.
#' @param tsid The timeseries ID.
#' @param from Specifies the first date (\%Y-\%m-\%d).
#' @param to Specifies the last date (\%Y-\%m-\%d).
#' @return The number of values of the focused time series is returned.
#' @rdname tstp_qnum
#' @export
#' @author Dominik Leutnant
tstp_qnum <- function(con, tsid, from=NULL, to=NULL) {
  
  .CheckClassObject(con)
  
  command  <- "?Cmd=QNUM"
  command  <- paste0(command, "&ZRID=",tsid)
  
  if (is.null(from) | is.null(to)) toc  <- tstp_query(con, ZRID = tsid)
  if (is.null(from)) {from <- toc[1,c('MAXFOCUS-Start')]}
  if (is.null(to)) {to <- toc[1,c('MAXFOCUS-End')]}
  
  command <- paste0(command,"&Von=",from)
  command <- paste0(command,"&Bis=",to)
  command <- paste0(.BuildServerString(con),command)
  result <- httr::GET(command, httr::authenticate(con@user,con@pass))
  result <- XML::xmlParse(rawToChar(result$content))
  result <- as.numeric(XML::xpathApply(result, path = "/TSR", XML::xmlValue)[[1]])
  return(result)
}

#' Forces the server to update a timeseries
#'
#' A TSID is submitted. The server reads in the time series once again and 
#' updates the MaxFocus. In case the time series has been modified without 
#' the server having noticed this, this command must be applied.
#' 
#' @title tstp_update
#' @param con A tstp_connection object.
#' @param tsid The timeseries ID.
#' @return The server response is returned.
#' @rdname tstp_update
#' @export
#' @author Dominik Leutnant
tstp_update <- function(con, tsid) {
  
  .CheckClassObject(con)
  
  command  <- "?Cmd=Update"
  command  <- paste0(command, "&ZRID=",tsid)
  command <- paste0(.BuildServerString(con),command)
  result <- httr::GET(command, httr::authenticate(con@user,con@pass))

  return(result)
}

#' Gets aggregated values of a specified interval
#'
#' A TSID, a time interval with Von=<point in time> and Bis=<point in time>, 
#' an interval width with IB=<interval range> (e.g., 5s, 30min, 1h, 7d, 
#' 1mon, 1a, ...), an interpretation with Aussage=<interpretation> (e.g. 
#' Mit(mean) - see chart 3 in appendix A) and optionally a quality layer with 
#' Qual=[0..50] are submitted. A time series with corresponding values
#' (interval values, continuous values or momentary values) is returned for the
#' time interval as binary block (see GET). The selected interval width IB 
#' defines the attributes time step factor and time step for the returned series.
#' 
#' @title tstp_getdval
#' @param con A tstp_connection object.
#' @param tsid The timeseries ID.
#' @param ib Specifies the interval width (e.g., 5s, 30min, 1h, 7d, 1mon, 1a, ...)
#' @param aus Specifies the way values are consolidated (Sum, Mit, Min, Max, 
#' DMax, DMin, Lck)
#' @param from Specifies the first date (\%Y-\%m-\%d).
#' @param to Specifies the last date (\%Y-\%m-\%d).
#' @param verbose logical. Provide additional details?
#' @param debug logical. For debugging purposes only.
#' @return A time series as an xts object.
#' @rdname tstp_getdval
#' @export
#' @author Dominik Leutnant
tstp_getdval <- function(con,
                         tsid,
                         ib,
                         aus,
                         from=NULL,
                         to=NULL,
                         verbose=FALSE,
                         debug=FALSE) {
  
  .CheckClassObject(con)
  
  if (!aus %in% .lAussageGetDVal) stop(paste0("bad aussage: ", aus))
  
  if (is.null(from) | is.null(to)) toc  <- tstp_query(con, ZRID = tsid)
  if (is.null(from)) {from <- toc[1,c('MAXFOCUS-Start')]}
  if (is.null(to)) {to <- toc[1,c('MAXFOCUS-End')]}
  
  command  <- "?Cmd=GetDVal"
  command  <- paste0(command, "&ZRID=",tsid)
  command <- paste0(command,"&Von=",from)
  command <- paste0(command,"&Bis=",to)
  command <- paste0(command,"&IB=",ib)
  command <- paste0(command,"&Aussage=",aus)
  command <- paste0(.BuildServerString(con),command)
  
  if (verbose) print(command)
  res <- httr::GET(command, httr::authenticate(con@user,con@pass))
  if (res$status_code != 200) stop(paste0("Status code ",res$status_code))
  
  #if (debug) debug_getdval <<- res
  
  doc <- XML::xmlTreeParse(rawToChar(res$content), options = XML::HUGE)
  r <- XML::xmlRoot(doc)
  
  data <- XML::xmlValue(r[[2]])
  
  data <- as.numeric(base64enc::base64decode(data))
  ts <- .ByteMatrixToXTS(data)
  ts <- tstp_assignattr(con = con, xts = ts, tsid = tsid)
  
  return(ts)
}

#' Gets floating amplitudes
#'
#' Calculates floating amplitutes (difference between the floating maxima and 
#' the floating minima). Supply a time series id (tsid), a focus with Von=time 
#' and Bis=time, and the width of the interval that is used to calculate the 
#' floating values (e.g. 1h): ib=width (e.g., 5s, 30min, 1h, 7d, 1mon, 1a, ...)
#' 
#' @title tstp_glamp
#' @param con A tstp_connection object.
#' @param tsid The timeseries ID.
#' @param ib Specifies the interval width (e.g., 5s, 30min, 1h, 7d, 1mon, 1a, 
#' ...)
#' @param from Specifies the first date (\%Y-\%m-\%d).
#' @param to Specifies the last date (\%Y-\%m-\%d).
#' @param verbose logical. Provide additional details?
#' @return A time series as an xts object.
#' @rdname tstp_glamp
#' @export
#' @author Dominik Leutnant
tstp_glamp <- function(con, tsid, ib, from=NULL, to=NULL, verbose=FALSE) {
  
  .CheckClassObject(con)
  
  if (is.null(from) | is.null(to)) toc  <- tstp_query(con, ZRID = tsid)
  if (is.null(from)) {from <- toc[1,c('MAXFOCUS-Start')]}
  if (is.null(to)) {to <- toc[1,c('MAXFOCUS-End')]}
  
  command  <- "?Cmd=GlAmp"
  command  <- paste0(command, "&ZRID=",tsid)
  command <- paste0(command,"&Von=",from)
  command <- paste0(command,"&Bis=",to)
  command <- paste0(command,"&IB=",ib)
  command <- paste0(.BuildServerString(con),command)
  
  if (verbose) print(command)
  res <- httr::GET(command, httr::authenticate(con@user,con@pass))
  if (res$status_code != 200) stop(paste0("Status code ",res$status_code))
  
  doc <- XML::xmlTreeParse(rawToChar(res$content), options = XML::HUGE)
  r <- XML::xmlRoot(doc)
  
  data <- XML::xmlValue(r[[2]])
  
  data <- as.numeric(base64enc::base64decode(data))
  ts <- .ByteMatrixToXTS(data)
  ts <- tstp_assignattr(con = con, xts = ts, tsid = tsid)
  
  return(ts)
}

#' Gets versions available
#'
#' Returns a vector with exisiting versions of the xts-object provided
#' 
#' @param con A tstp_connection object.
#' @param xts An xts object with tstp attributes.
#' @return A numeric vector with exisiting versions of the xts-object provided. 
#' @rdname tstp_getversion
#' @export
#' @author Dominik Leutnant
#' @seealso \code{\link[xts]{xts}}
tstp_getversion <- function(con, xts) {

  
  v <- as.numeric( levels(tstp_query(con, ort = attr(xts, "Ort"),
                             subort = attr(xts, "Subort"),
                             param = attr(xts, "Parameter"),
                             defart = attr(xts, "Defart"),
                             aussage = attr(xts, "Aussage"),
                             xdistanz = attr(xts,"XDistanz"),
                             xfaktor = attr(xts, "XFaktor"),
                             herkunft = attr(xts, "Herkunft"),
                             reihenart = attr(xts, "Reihenart"),
                             quelle = attr(xts, "Quelle") )$VERSION) )  
  
  return(v)
  
}

#' Sets values to specified interval
#'
#' Sets values on a given time series within given range.
#' 
#' @param con A tstp_connection object.
#' @param tsid The timeseries ID.
#' @param defart Specifies the defart.
#' @param reihenart Specifies the reihenart.
#' @param value The value to be set.
#' @param from Specifies the first date (\%Y-\%m-\%d).
#' @param to Specifies the last date (\%Y-\%m-\%d).
#' @param by Increment of the time sequence (e.g. "min")
#' @param tz Specifies the time zone.
#' @return \code{\link[tstpr]{tstp_put}} 
#' @note WARNING! This function overrides values without prompting!
#' @rdname tstp_setvalue
#' @export
#' @author Dominik Leutnant
tstp_setvalue <- function(con, tsid, defart=c("K","I","M"), reihenart=c("Z","R"), value=NA, 
                          from=NULL, to=NULL, by="mins", tz="GMT") {
  
  # create xts object with NA's
  time <- seq(as.POSIXct(from, tz = tz), as.POSIXct(to, tz = tz), by)
  values <- rep(x = value, times = length(time) )
  ts <- xts::xts(x = values, order.by = time)
  
  # assign time series ID, defart and reihenart
  attr(ts, 'ZRID') <- tsid
  attr(ts, 'Defart') <- defart
  attr(ts, 'Reihenart') <- reihenart
  
  # upload time series
  tstp_put(con = con, xts = ts, text = FALSE)
  
}

### converting Methods 

.xts_to_binxml <- function(xts, text, mesaus=NULL) {
  
  if (!is.null(mesaus)) {
    if (!mesaus %in% .lMESAUS) stop("bad MESAUS")
  }

  strREIHENART <-  attr(xts,"Reihenart")
  strDEFART  <-  attr(xts,"Defart")
  strEINHEIT  <-  attr(xts,"Einheit")
  strLEN <- nrow(xts) * 12
  strANZ  <- nrow(xts)
  
  if (text) {
  
    strText  <- "Ja"
    bm64agg <- .IndexStringToByteMatrix(xts)
    strLEN  <- length(bm64agg)
    bm64aggsep  <- base64enc::base64encode(bm64agg)
    
  } else {
    
    strText  <- "Nein"
    bm <- .IndexDataToByteMatrix(xts)
    bm64  <-  apply(bm,1,function(x) base64enc::base64encode(x))
    
    bm64agg <- sapply(seq(1,length(bm64),by = 4),
                      FUN = function(x) (paste0(bm64[x:(x + 3)], collapse = "")))
    
    bm64agg[length(bm64agg)] <- stringr::str_sub(utils::tail(bm64agg,1),
                                                 start = 1,
                                                 end = nchar(utils::tail(bm64agg,1)) - 2)
    
    bm64aggsep  <- paste0(bm64agg, "\n", collapse = "")    
  }
  
  str <- "<?XML version=\"1.0\" encoding=\"ISO-8859-1\"?>\n"
  str <- paste0(str, "<TSD RELEASE=\"1\">\n")
  str <- paste0(str, "<DEF REIHENART=\"", strREIHENART ,
                "\" TEXT=\"", strText, "\" DEFART=\"", strDEFART ,
                "\" EINHEIT=\"",strEINHEIT,
                "\" LEN=\"",strLEN, 
                "\" ANZ=\"", strANZ, "\"")
  
  if (!missing(mesaus)) str <- paste0(str," MESAUS=\"" , mesaus, "\"")
  str <- paste0(str, "/>\n")
  str  <- paste0(str,"<DATA><![CDATA[\n", bm64aggsep , "]]></DATA>\n")
  str  <- paste0(str,"</TSD>\n")
  binxml <- str

  invisible(binxml)
}

.matrix_to_binxml <- function(data, text, mesaus=NULL) {
  
  if (!is.null(mesaus)) {
    if (!mesaus %in% .lMESAUS) stop("bad MESAUS")
  }
  
  strREIHENART <-  "R"
  strDEFART  <-  "K"
  strEINHEIT  <-  "noUnit"
  strLEN <- nrow(data) * 12
  strANZ  <- nrow(data)
  
  if (text) {
    
    strText  <- "Ja"
    bm <- .RealMatrixToByteMatrix(data)
    bm64  <-  apply(bm,1,function(x) base64enc::base64encode(x))
    bm64agg <- sapply(seq(1,length(bm64), by = 4),
                      FUN = function(x) (paste0(bm64[x:(x + 3)],
                                                collapse = "")))
    bm64agg[length(bm64agg)] <- stringr::str_sub(utils::tail(bm64agg,1),
                                                 start = 1,
                                                 end = nchar(utils::tail(bm64agg,1)) - 2)
    bm64aggsep  <- paste0(bm64agg, "\n", collapse = "") 
    
  } else {
    
    strText  <- "Nein"
    bm <- .RealMatrixToByteMatrix(data)
    bm64  <-  apply(bm,1,function(x) base64enc::base64encode(x))
    bm64agg <- sapply(seq(1,length(bm64), by = 4),
                      FUN = function(x) (paste0(bm64[x:(x + 3)],collapse = "")))
    bm64agg[length(bm64agg)] <- stringr::str_sub(utils::tail(bm64agg,1),
                                                 start = 1,
                                                 end = nchar(utils::tail(bm64agg,1)) - 2)
    bm64aggsep  <- paste0(bm64agg, "\n", collapse = "")    
  }
  
  str <- "<?XML version=\"1.0\" encoding=\"ISO-8859-1\"?>\n"
  str <- paste0(str, "<TSD RELEASE=\"1\">\n")
  str <- paste0(str, "<DEF REIHENART=\"", strREIHENART ,
                "\" TEXT=\"", strText, "\" DEFART=\"", strDEFART ,
                "\" EINHEIT=\"",strEINHEIT,
                "\" LEN=\"",strLEN, 
                "\" ANZ=\"", strANZ, "\"")
  
  if (!missing(mesaus)) str <- paste0(str," MESAUS=\"" , mesaus, "\"")
  str <- paste0(str, "/>\n")
  str  <- paste0(str,"<DATA><![CDATA[\n", bm64aggsep , "]]></DATA>\n")
  str  <- paste0(str,"</TSD>\n")
  binxml <- str
  
  invisible(binxml)
}

.IndexDataToByteMatrix <- function(xts) {
  
  len <- nrow(xts)
  yr <- lubridate::year(zoo::index(xts))
  mn <- lubridate::month(zoo::index(xts))             
  dy <- lubridate::day(zoo::index(xts))
  hr <- lubridate::hour(zoo::index(xts))
  mi <- lubridate::minute(zoo::index(xts))
  se <- lubridate::second(zoo::index(xts))
  x  <- 1:(12 * len)
  dim(x) <- c(len,12)
  mode(x)  <- "raw"
  x[,1]  <- as.raw(0)
  x[,2]  <- as.raw(floor(as.double(yr))/256)
  x[,3]  <- as.raw(floor(yr) %% 256)
  x[,4]  <- as.raw(mn)
  x[,5]  <- as.raw(dy)
  x[,6]  <- as.raw(hr)
  x[,7]  <- as.raw(mi)
  x[,8]  <- as.raw(se)
  db <- raw()
  db <- as.numeric(writeBin(as.numeric(zoo::coredata(xts)), db, 4, "big"))
  len2  <- length(db)                 
  x[,9] <- as.raw(db[seq(1,len2,4)])
  x[,10] <- as.raw(db[seq(2,len2,4)])
  x[,11] <- as.raw(db[seq(3,len2,4)])
  x[,12] <- as.raw(db[seq(4,len2,4)])
  mode(x) <- "numeric"
  return(x)  
}

.RealMatrixToByteMatrix <- function(data) {
  
  len <- nrow(data)
  x  <- 1:(12 * len)
  dim(x) <- c(len,12)
  mode(x)  <- "raw"

  db <- raw()
  db <- as.numeric(writeBin(as.numeric(data[,1]),db,8,"little"))
  len1  <- length(db) 
  x[,1]  <- as.raw(db[seq(1,len1,8)])
  x[,2]  <- as.raw(db[seq(2,len1,8)])
  x[,3]  <- as.raw(db[seq(3,len1,8)])
  x[,4]  <- as.raw(db[seq(4,len1,8)])
  x[,5]  <- as.raw(db[seq(5,len1,8)])
  x[,6]  <- as.raw(db[seq(6,len1,8)])
  x[,7]  <- as.raw(db[seq(7,len1,8)])
  x[,8]  <- as.raw(db[seq(8,len1,8)])
  
  db <- raw()
  db <- as.numeric(writeBin(as.numeric(data[,2]),db,4,"little"))
  len2  <- length(db)                 
  x[,9] <- as.raw(db[seq(1,len2,4)])
  x[,10] <- as.raw(db[seq(2,len2,4)])
  x[,11] <- as.raw(db[seq(3,len2,4)])
  x[,12] <- as.raw(db[seq(4,len2,4)])
  mode(x) <- "numeric"
  return(x)  
}

.IndexStringToByteMatrix <- function(xts) {
  
  new.vector <- rep(0,length(xts))
  bin.str <- as.raw(1:4)
  
  for (i in seq_along(xts)) {
 
    yr <- lubridate::year(zoo::index(xts[i]))
    mn <- lubridate::month(zoo::index(xts[i]))             
    dy <- lubridate::day(zoo::index(xts[i]))
    hr <- lubridate::hour(zoo::index(xts[i]))
    mi <- lubridate::minute(zoo::index(xts[i]))
    se <- lubridate::second(zoo::index(xts[i]))
   
    raw.string <- charToRaw(as.character(zoo::coredata(xts[i])))
    len.raw.string  <- length(raw.string)

    x  <- rep(0, len.raw.string + 10 )
    mode(x)  <- "raw"
    
    x[1]  <- as.raw(0)
    x[2]  <- as.raw(floor(as.double(yr))/256)
    x[3]  <- as.raw(floor(yr) %% 256)
    x[4]  <- as.raw(mn)
    x[5]  <- as.raw(dy)
    x[6]  <- as.raw(hr)
    x[7]  <- as.raw(mi)
    x[8]  <- as.raw(se)
    
    if (len.raw.string == 0) {
      
      ## empty string --> var:8
      x[9] <-  as.raw(8)
      x <- x[-10]
      
    } else {
    
      if (len.raw.string < 256) {

        ## string contains not more than 255 characters --> var:6
        x[9] <- as.raw(6)
        x[10]  <-  as.raw(len.raw.string)
        
        for (j in 1:len.raw.string) {
          x[10 + j]  <- raw.string[j]
        }
        
      } else {

        ## string exceeds 255 characters --> var:7
        x[9] <-  as.raw(7)
        x[10:13] <- writeBin(len.raw.string, bin.str, size = 4)

        for (j in 1:len.raw.string) {
          x[13 + j]  <- raw.string[j]
        }
      }  
    }

  mode(x) <- "numeric"
  new.vector[i] <-  paste(x, collapse = " ")

  }
 
  x  <- paste(new.vector, collapse = " ")
  x  <- as.numeric(sapply(strsplit(x, split = " "), function(x) as.numeric(x)))

  return(x)
}

.StrByteVectorToXTS <- function(StrByteVector, items){
  
  index.tmp  <- text.tmp <- c(1:items)
  i <- j <- 1
  
  while (i < length(StrByteVector) ) {
    
    jahr  <- StrByteVector[i + 1] * 256 + StrByteVector[i + 2]
    monat  <- StrByteVector[i + 3]
    tag  <- StrByteVector[i + 4]
    std  <- StrByteVector[i + 5]
    min  <- StrByteVector[i + 6]
    sek  <- StrByteVector[i + 7]
    
    index.tmp[j] <- paste0(jahr,"-",monat,"-",tag, " ", std, ":", min, ":", sek)
    
    var <- StrByteVector[i + 8]
    
    ## empty string
    if (var == 8) {
      ## 8: no text  
      text.tmp[j]  <- ""
      j <- j + 1
      i  <- i + 9
      
    } else {
      
      ## string contains not more than 255 characters -->
      if (var == 6) {
        ## 6: length of text in nextbyte
        text.len <- StrByteVector[i + 9]
        text.tmp[j] <- readBin(as.raw(StrByteVector[(i + 10):(i + 10 + text.len - 1)]),
                               character(),
                               n = 1)
        j <- j + 1
        i <- i + 10 + text.len
      } else {
        
        ## string exceeds 255 characters -->
        if (var == 7) {
          ## 7: length of text in next 4 bytes:
          text.len <- as.numeric(readBin(as.raw(StrByteVector[(i + 9):(i + 12)]),
                                         integer(),
                                         n = 1,
                                         size = 4))
          text.tmp[j] <- readBin(as.raw(StrByteVector[(i + 13):(i + 13 + text.len - 1)]),
                                 character(),
                                 n = 1)
          j <- j + 1
          i <- i + 13 + text.len
        } else {
          stop("error in .StrByteVectorToXTS function --> could not identify type of variant")
        }
      } 
    }
  }
  
  dt <- as.POSIXct(index.tmp, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
  ts <- xts::xts(x = text.tmp, order.by = dt)
  
  return(ts)
}

.ByteMatrixToXTS <- function(ByteMatrix){
  rd <- ByteMatrix
  jahr  <- rd[seq(2,length(rd),12)] * 256 + rd[seq(3,length(rd),12)]
  monat <- rd[seq(4,length(rd),12)]
  tag <- rd[seq(5,length(rd),12)]
  std <- rd[seq(6,length(rd),12)]
  min <- rd[seq(7,length(rd),12)]
  sek <- rd[seq(8,length(rd),12)]
  d1 <- rd[seq(9,length(rd),12)]
  d2 <- rd[seq(10,length(rd),12)]
  d3 <- rd[seq(11,length(rd),12)]
  d4 <- rd[seq(12,length(rd),12)]
  
  dat <- cbind(d1,d2,d3,d4)
  mode(dat)  <- "raw"
  # nachfolgender Befehl langsam
  dbl  <- apply(dat[1:nrow(dat), ],
                MARGIN = 1,
                function(x) {.ByteArrayToDouble(rev(as.raw(x)),4)}
  )
  dt <- paste0(jahr,"-",monat,"-",tag, " ", std, ":", min, ":", sek)
  dt <- as.POSIXct(dt, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
  ts <- xts::xts(x = dbl, order.by = dt)
  return(ts)
}

.ByteMatrixToDataMatrix <- function(ByteMatrix){
  bm <- ByteMatrix
  x <- bm[,1:8]
  print(x)
  mode(x) <- "raw"
  xdbl <- apply(x[1:nrow(x), ],
                MARGIN = 1,
                function(x) {.ByteArrayToDouble(as.raw(x),8)}
  )
  

  y <- bm[,9:12]
  mode(y)  <- "raw"
  ydbl  <- apply(y[1:nrow(y), ],
                MARGIN = 1,
                function(x) {.ByteArrayToDouble(as.raw(x),4)}
  )
 
  xy <- cbind(xdbl,ydbl)
  return(xy)
}

.ByteArrayToDouble <- function(ByteArr,size){
  if (!identical(ByteArr,.LUECKE)) {
    dbl <- readBin(ByteArr,
                   double(), 
                   n = 1,
                   size = size,
                   signed = T,
                   endian = "little")
  } else
    {dbl <- NA
    warning("LUECKE detected")
    }
  return(dbl)
}

.ColumnTypeConversion <- function(df, verbose=FALSE) {
  
  # convert columns to most appropriate type
  # (type.convert needs characters!)
  df[] <- lapply(df, as.character)
  df[] <- lapply(df, utils::type.convert, as.is = TRUE)
  
  return(df)  
  
}