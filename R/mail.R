#' Test if mutt is installed on the host platform
#' See \url{http://www.mutt.org/}
#'
#' @family mail
#' @export
#' @return logical, TRUE if present
has_mutt <- function() {
   basename(Sys.which("mutt")) == "mutt"
}

#' Test if nail is installed on the host platform
#' See \url{http://nail.sourceforge.net/}
#'
#' @family mail
#' @export
#' @return logical, TRUE if present
has_mutt <- function() {
   basename(Sys.which("nail")) == "nail"
}


#' Send an email from R using the underlying mail services.
#' 
#' A simple wrapper around the platforms email engine.  Currently \code{mutt} and
#' \code{nail} are supported, but the host must have these installed.
#' 
#' @family mail
#' @export
#' @param ... arguments for the mailer of choice. Currently, only mutt is supported.
#' @param what the name of the mail engine to use, currently 'nail' and 'mutt' are supported
#' @return value returned by the system-level mail program, non-zero for failure.
sendmail <- function(..., what = c('nail', 'mutt')[1]){
   
   switch(tolower(what),
      "mutt" = muttmail(...),
      "nail" = nailmail(...),
      1)
}


#' Send a simple mail via mutt  - see \url{http://www.mutt.org/}
#'
#' @family mail
#' @export
#' @param to a character vector of one or more valid email addresses
#' @param sbj a character for the subject line (required)
#' @param msg - a character vector of one or more lines for the message body (NA to skip)
#' @param attachment the fully qualified filename to attach (if any, NA to skip)
#' @param verbose  logical, if TRUE then echo the command
#' @return 0 for success and non-zero otherwise
muttmail <- function(to = "name@somewhere.org", 
   sbj = "mutt mail", 
   msg = paste("at", Sys.time(), "you have mutt mail"), 
   attachment = NA,
   verbose = TRUE){
 
   if (!has_mutt()) stop("muttmail: mutt is not installed")
   address <- paste(to,collapse = " ")
   cmd <- paste("mutt", "-s", shQuote(sbj[1])) 
   if (!is.na(attachment)) cmd <- paste(cmd, "-a", attachment, "--")
   cmd <- paste(cmd, address)
   hasMsg <- (length(msg) > 0) || !is.na(msg[1])
   if (hasMsg) {
      msgFile <- tempfile()
      cat(msg, sep = "\n", file = msgFile)
      cmd <- paste(cmd, "<", msgFile)
   }
   if (verbose) cat(cmd, "\n")
   ok <- system(cmd)
   # clean up
   if (hasMsg) unlink(msgFile)
   return(ok) 
}


#' Send a simple mail via nail  - see \url{http://linux.die.net/man/1/nail}
#'
#' @family mail
#' @export
#' @param to a character vector of one or more valid email addresses
#' @param sbj a character for the subject line (required)
#' @param msg - a character vector of one or more lines for the message body (NA to skip)
#' @param attachment the fully qualified filename to attach (if any, NA to skip)
#' @param verbose  logical, if TRUE then echo the command
#' @return 0 for success and non-zero otherwise
nailmail <- function(to = "name@somewhere.org", 
   sbj = "nail mail", 
   msg = paste("at", Sys.time(), "you have nail mail"), 
   attachment = NA,
   verbose = TRUE){
   if (!has_nail()) stop("nailmail: nail is not installed")
   address <- paste(to,collapse = " ")
   cmd <- paste("nail", "-s", shQuote(sbj[1]))
   if (!is.na(attachment)) cmd <- paste(cmd, "-a", attachment)
   cmd <- paste(cmd, address)
   hasMsg <- (length(msg) > 0) || !is.na(msg[1])
   if (hasMsg) {
      msgFile <- tempfile()
      cat(msg, sep = "\n", file = msgFile)
      cmd <- paste(cmd, "<", msgFile)
   }
   # speak?
   if (verbose) cat(cmd, "\n")
   ok <- system(cmd)
   # clean up
   if (hasMsg) unlink(msgFile)
   return(ok)
}
