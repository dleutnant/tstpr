#' Get multiple time series data from a tstp server
#' 
#' This function is basically a wrapper for \code{tstp_get} to get multiple time series with one call.
#' It creates xts-objects for each time series in the current R environment with 'site.parameter.version' syntax.
#' R objects with identical names are overwritten without warning.
#' 
#' @title get_timeseries
#' @param con A tstp.connection object.
#' @param version Query parameter (optional).
#' @param parameter Query parameter (optional).
#' @param site Query parameter (optional).
#' @param from Specifies the first date (\%Y-\%m-\%d).
#' @param to Specifies the last date (\%Y-\%m-\%d).
#' @param parallel logical. Loads timeseries data in parallel.
#' @param cpu Number of CPU's to be used.
#' @param verbose logical. Should informative outputs printed during function evaluation?
#' @param debug logical. For debugging purposes only.
#' @rdname get_timeseries
#' @importFrom foreach "%dopar%"
#' @importFrom foreach "%do%"
#' @export
#' @author Dominik Leutnant
get_timeseries <- function(con, 
                           version,
                           parameter,
                           site,
                           from=NULL,
                           to=NULL,  
                           parallel=FALSE, 
                           cpu=1,
                           verbose=F,
                           debug=FALSE) {
  
  if (!requireNamespace("foreach", quietly = TRUE)) {
    stop("foreach needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  tmp <- lubridate::now()
  remove.index <- NA
            
  ## create grid with possible combinations
  elements <- expand.grid(site,
                          parameter,
                          stringsAsFactors = FALSE)
            
  if (debug) print(elements)
            
  ## get ids of timeseries for each element of grid
  zrids <- foreach::foreach(i = 1:nrow(elements), .combine = 'c') %do% ({
      id <- as.character(tstp_query(con, 
                                    ort = elements$Var1[i],
                                    version = version, 
                                    param = elements$Var2[i])$ZRID)
      ## remove NULL return from element list arguments
      if ( identical(id,character(0)) ) {
        remove.index <- c(remove.index, i)
      }
      id
  })
  
  ## if remove.index is still NA
  if (!identical(as.numeric(stats::na.exclude(remove.index)), numeric(0))) {
    elements <- elements[-stats::na.omit(remove.index),]  
  }
  
  #if (debug) print(elements)
  
  #if (debug) debug_get_timeseries <<- zrids
  
  if (parallel) {
    
    if (!requireNamespace("doParallel", quietly = TRUE)) {
      stop("doParallel needed for this function to work. Please install it.",
           call. = FALSE)
    }
    
    doParallel::registerDoParallel(cores = cpu)
    
    if ( (!is.null(from)) && (!is.null(to)) ) {
      list.of.ts <- foreach::foreach(i = 1:length(zrids)) %dopar% (tstp_get(con,
                                                                            zrids[i],
                                                                            from = from,
                                                                            to = to))
    } else {
      list.of.ts <- foreach::foreach(i = 1:length(zrids)) %dopar% (tstp_get(con,
                                                                            zrids[i]))  
    }
    
  } else {
    
    if ( (!is.null(from)) && (!is.null(to)) ) {
      list.of.ts <- foreach::foreach(i = 1:length(zrids)) %do% (tstp_get(con,
                                                                         zrids[i],
                                                                         from = from,
                                                                         to = to) )  
      } else {
      list.of.ts <- foreach::foreach(i = 1:length(zrids)) %do% (tstp_get(con, 
                                                                         zrids[i]))  
    }
  }
  
  ## extract elements of list and create single variables
  for (i in 1:length(zrids)) {
    # WARNING: Variables are overridden without prompting!
    
    assign(paste(elements$Var1[i],
                 elements$Var2[i],
                 paste0("v",version),
                 sep = "."),
           list.of.ts[[i]] , 
           pos = .GlobalEnv)
  }
  
  ret <- c(paste(elements$Var1,
                 elements$Var2,
                 paste0("v",version),
                 sep = "."))
  
  if (verbose) (print(lubridate::now() - tmp))
  rm(tmp, list.of.ts, zrids, elements)
  
  invisible(ret)
}
