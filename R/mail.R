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
has_nail <- function() {
   basename(Sys.which("nail")) == "nail"
}


#' Test if mail is installed on the host platform
#'
#' @family mail
#' @export
#' @return logical, TRUE if present
has_mail <- function() {
   Sys.which("mail") != ""
}


#' Send an email from R using the underlying mail services.
#' 
#' A simple wrapper around the platforms email engine.  Currently \code{mail}, \code{mutt} and
#' \code{nail} are supported, but the host must have these installed.
#' Also supported is \code{charlie} whihc is a hardwired version of mail for use on charlie
#' 
#' @family mail
#' @export
#' @param ... arguments for the mailer of choice. Currently, only mutt is supported.
#' @param what the name of the mail engine to use, currently 'nail' and 'mutt' are supported
#' @return value returned by the system-level mail program, non-zero for failure.
sendmail <- function(..., what = c('mail', 'nail', 'mutt', 'charlie')[1]){
   
   switch(tolower(what),
      'mail' = mailmail(...),
      "mutt" = muttmail(...),
      "nail" = nailmail(...),
      "charlie" = charliemail(...),
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
muttmail <- function(to = "btupper@bigelow.org", 
   sbj = "muttmail", 
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
nailmail <- function(to = "btupper@bigelow.org", 
   sbj = "nailmail", 
   msg = paste("at", Sys.time(), "you have nail mail"), 
   attachment = NA,
   verbose = TRUE){
   if (!has_nail()) stop("nailmail: nail is not installed")
   address <- paste(to,collapse = " ")
   cmd <- paste("nail -vv", "-s", shQuote(sbj[1]))
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


#' Send a simple mail via mail  - see \url{http://linux.die.net/man/1/nail}
#'
#' @family mail
#' @export
#' @param to a character vector of one or more valid email addresses
#' @param sbj a character for the subject line (required)
#' @param msg - a character vector of one or more lines for the message body (NA to skip)
#' @param attachment the fully qualified filename to attach (if any, NA to skip)
#' @param verbose  logical, if TRUE then echo the command
#' @return 0 for success and non-zero otherwise
mailmail <- function(to = "btupper@bigelow.org", 
   sbj = "mailmail", 
   msg = paste("at", Sys.time(), "you have mail mail"), 
   attachment = NA,
   verbose = TRUE){
     
   mailapp <- Sys.which("mail")
   if ( mailapp == "") stop("mail application not available")
      
   # https://tecadmin.net/ways-to-send-email-from-linux-command-line/ 
   # mail -a [attachment] -s [subject] <to> < [message]
      
   # store the message in a temporary file
   msgfile <- tempfile()
   cat(msg, sep = "\n", file = msgfile)
      
   cmd <- sprintf("-s %s %s < %s", sbj, paste(to, collapse = ","), msgfile)
   
   if (!is.null(attachment) && !is.na(attachment)){
     cmd <- sprintf("-a %s %s", attachment, cmd)
   } 
   
   # speak?
   if (verbose) cat(paste(mailapp,cmd), "\n")  
     
   ok <- system2(mailapp, args = cmd)
      
   unlink(msgfile)  
      
   return(ok)  
}



#' Send a simple mail via mail, but hardwired for charlie
#'
#' @family mail
#' @export
#' @param to a character vector of one or more valid email addresses
#' @param sbj a character for the subject line (required)
#' @param msg - a character vector of one or more lines for the message body (NA to skip)
#' @param attachment the fully qualified filename to attach (if any, NA to skip)
#' @param verbose  logical, if TRUE then echo the command
#' @return 0 for success and non-zero otherwise
charliemail <- function(to = "btupper@bigelow.org", 
   sbj = "charliemail", 
   msg = paste("at", Sys.time(), "you have mail mail"), 
   attachment = NA,
   verbose = TRUE){
     
   # on charlie mail must have the path prepended
   mailapp = shQuote("LD_LIBRARY_PATH=/lib64/ mail")

   # https://tecadmin.net/ways-to-send-email-from-linux-command-line/ 
   # mail -a [attachment] -s [subject] <to> < [message]
      
   # store the message in a temporary file
   msgfile <- tempfile()
   cat(msg, sep = "\n", file = msgfile)
      
   cmd <- sprintf("-s %s %s < %s", sbj, paste(to, collapse = ","), msgfile)
   
   if (!is.null(attachment) && !is.na(attachment)){
     cmd <- sprintf("-a %s %s", attachment, cmd)
   } 
   
   # speak?
   if (verbose) cat(paste(mailapp,cmd), "\n")  
     
   ok <- system2(mailapp, args = cmd)
      
   unlink(msgfile)  
      
   return(ok)  
}
