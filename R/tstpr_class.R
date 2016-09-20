#' tstp_srv class
#' 
#' The tstp_srv class is used to communicate with the tstp server. 
#' It stores the tstp server connection information.
#' 
#'@section Slots: 
#'  \describe{
#'    \item{\code{host}:}{Object of class \code{"character"} }
#'    \item{\code{port}:}{Object of class \code{"numeric"} }
#'    \item{\code{user}:}{Object of class \code{"character"} }
#'    \item{\code{pass}:}{Object of class \code{"character"} }
#'  }
#' @name tstp_srv-class
#' @rdname tstp_srv-class
#' @author Dominik Leutnant
methods::setClass("tstp_srv", 
                  slots = c(host = "character",
                            port = "numeric",
                            user = "character",
                            pass = "character"),
                  contains = character()
)

.BuildServerString <- function(tstpsrv) {
  paste0(tstpsrv@host,":", tstpsrv@port, "/")
}

.CheckClassObject <- function(tstp_connection=NULL) {
  if (is.null(tstp_connection)) {
    warning("tstp_connection object is NULL. Using group 'root' of config file.")
    tstp_connection <- tstp_connection(group = "root")
  } 
  if (class(tstp_connection) != "tstp_srv") stop("class must be of type 'tstp_srv'")
}

#' Creates a tstp_connection object
#' 
#' 
#' @title tstp_connection
#' @param host host
#' @param user user
#' @param port port
#' @param pass pass
#' @param group group
#' @param verbose logical. Provide additional details?
#' @param config.file The configuration file to be used if group is specified.
#' @param timeout Number of seconds to wait for a response until giving up. Can not be less than 1 ms. 
#' @rdname tstp_connection
#' @export
#' @author Dominik Leutnant
#' @seealso \code{\link[httr]{timeout}}
tstp_connection <-  function(host=NULL,
                             port=NULL,
                             user="user",
                             pass="pass",
                             group=NULL,
                             verbose=FALSE,
                             config.file="~/.tstp.cnf",
                             timeout=5) {
  
  if (!is.null(group)) {
  
    if (file.exists(config.file)) {
      
      con <- file(config.file, open = "r")
      
      lines <- readLines(con)
      
      grp <- grep(paste0("[",group,"]"), x = lines, fixed = T)
      
      if (identical(grp, integer(0))) stop("Group does not exist in config file.")
      
      host <- gsub("host=", "", grep("host=", lines[(grp + 1):(grp + 4)],
                                     fixed = T,
                                     value = T))
      port <- as.numeric(gsub("port=", "", 
                              grep("port=", lines[(grp + 1):(grp + 4)],
                                   fixed = T, 
                                   value = T)))
      user <- gsub("user=", "", 
                   grep("user=", lines[(grp + 1):(grp + 4)],
                        fixed = T, 
                        value = T))
      pass <- gsub("pass=", "", 
                   grep("pass=", lines[(grp + 1):(grp + 4)],
                        fixed = T, 
                        value = T))
      
      close(con)
      
    } else{
      
      stop("Config file does not exist.")
      
    }

  }

  tstp_srv <- methods::new("tstp_srv", 
                           host = host, 
                           port = port,
                           user = user, 
                           pass = pass)
  
  #url <- paste0(tstp_srv@host,":", tstp_srv@port, "/?Cmd=Query")
  # changing to a bad command (ping doesn't exist) to circumvent time outs 
  # of long query
  url <- paste0(tstp_srv@host,":", tstp_srv@port, "/?Cmd=ping")
  
  if (verbose) print(url)
  response  <- httr::GET(url, 
                         httr::authenticate(tstp_srv@user, tstp_srv@pass), 
                         httr::timeout(timeout))
  
  status <- httr::http_status(response)
  
  if (status$message != "Success: (200) OK") {
    stop(status$message)
  }
  message(status$message)
  invisible(tstp_srv)
}
