#' Upload files: currently nivus, ott and wtw filetypes are supported
#'
#' upload files to tstp
#' @title upload_files
#' @param con A tstp.connection object.
#' @param type "wtw", "nivus" or "ott"
#' @param ort site
#' @param subort subsite
#' @param version version
#' @param folder folder
#' @param use.pattern logical. use.pattern
#' @param pattern pattern
#' @param use.pattern.ex logical. use.pattern.ex
#' @param pattern.ex pattern.ex
#' @param upload logical. Upload?
#' @param parameter Specifies the parameters to be read (currently all but 
#' nivus ignore this parameter).
#' @param max.files maximum number of files to upload.
#' @return The names of uploaded xts-objects are invsibly returned.
#' @keywords internal
#' @rdname upload_files
#' @author Dominik Leutnant
upload_files <- function(con, 
                         type, 
                         ort,
                         subort,
                         version,
                         folder = NULL, 
                         use.pattern = TRUE, 
                         pattern = NULL, 
                         use.pattern.ex = FALSE, 
                         pattern.ex = NULL,
                         upload=FALSE,
                         parameter=NULL,
                         max.files=10) {
  
  if (!requireNamespace("tsconvert", quietly = TRUE)) {
    stop("tsconvert needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  .CheckClassObject(con)
  
  if (is.null(folder)) {
    filedir <- utils::choose.dir()  
  } else {
    filedir  <- folder
  }
  
  if (!type %in% c("nivus", "ott","wtw")) stop("bad type: currently nivus,
                                               ott and wtw filetypes are
                                               supported only.")
  
  if ( (use.pattern) & (is.null(pattern)) ) {
    pattern  <- switch(type, 
                       nivus = c("LABORXRI", "PARKPLAT", "PP", "GIEVENBE", 
                                 "GB", "SG"),
                       ott = c("\\.OML","\\.gz"),
                       wtw = c("TetraCon", "SensoLyt", "VisoTurb", "MIQIC2"))
  } 
  
  if ( (use.pattern.ex) & (is.null(pattern.ex)) ) {
    pattern.ex  <- switch(type,
                          ott = c("INFO\\.OML"))
  }
  
  files <- character()
  
  for (i in seq_along(pattern)) files <- c(files,
                                           (list.files(filedir,
                                                       full.names = TRUE, 
                                                       recursive = TRUE, 
                                                       pattern = pattern[i])))

  filelist <- file.info(files)
  
  if (use.pattern.ex) {
    files.ex  <- character()
    for (i in seq_along(pattern.ex)) files.ex <- c(files.ex,
                                                   (list.files(filedir,
                                                               full.names = TRUE,
                                                               recursive = TRUE,
                                                               pattern = pattern.ex[i])))
    filelist <- file.info(files[!files %in% files.ex])
  }
  
  filelist <- filelist[with(filelist, order(as.POSIXct(mtime))), ]
  filelist <- utils::tail(filelist, max.files)
  filelist  <- rownames(filelist)
  
  return.list.of.xts <- list()
  
  for (i in seq_len(length(filelist))) {
    
    print(filelist[i])
    
    list.of.xts <- switch(type, 
                          nivus = tsconvert::read_nivus(filelist[i], 
                                                        ncol = 4,
                                                        parameter = parameter),
                          ott = tsconvert::read_ott(filelist[i]),
                          wtw = tsconvert::read_wtw(filelist[i])
                          )
    
    for (i in seq_along(list.of.xts)) {
      attr(list.of.xts[[i]], 'Ort') <- ort
      attr(list.of.xts[[i]], 'Subort') <- subort
      attr(list.of.xts[[i]], 'Version') <- version
    }
    
    if (upload) lapply(list.of.xts, FUN = function(x) {upload_xts(con, 
                                                                  stats::na.omit(x))})
    
    return.list.of.xts <- c(list(list.of.xts), return.list.of.xts)
    
  }
  
  invisible(return.list.of.xts)
  
}

#' Uploads xts-objects
#'
#' Upload of an xts-object
#' 
#' @title upload_xts
#' @param con A tstp.connection object.
#' @param xts An xts object to be uploaded.
#' @param text logical. Does the xts object contain characters?
#' @return The server response is returned.
#' @rdname upload_xts
#' @export
#' @author Dominik Leutnant
upload_xts <- function(con, xts, text=FALSE){
  if (all(is.na(xts))) {
    warning("all(is.na(xts)) is TRUE, skipping xts...")
  return(NULL)
  } else {
    response  <- tstp_create(con, xts)
    if (response == 0) stop("tstp.create failed")
    print(response)
    attr(xts, "ZRID" ) <- response
    response <- tstp_put(con, xts, text)
    #if (gsub("[[:space:]]","",response) != "confirm" ) stop("tstpput failed")
    #print(response)
    return(response)
  } 
  
}
