#' A Logger reference class for simple message logging
#'
#' @field name character, the name of the Logger
#' @field filename the name of the file to write to
#' @field timer TimekeeperRefClass object
#' @field .echo logical, logical for internal use
#' @field .hasfile logical, for internal use
#' @field .timestamp logical, for internal use

LoggerRefClass <- setRefClass("LoggerRefClass",
   fields = list(
      name = 'character',
      filename = 'character',
      timer = 'ANY',
      .echo = 'logical',
      .hasfile = 'logical',
      .timestamp = 'logical',
      .autoroll = 'logical',
      .rollsize = 'integer'))

#' Log messages, possible echoed to the command line
#'
#' @family Logger
#' @name Logger_log
#' @param ... message parts, passed to \code{sprintf} so should start
#'    with a formatting string followed by one or more arguments. See \code{do_sprintf}
#'    to control this behavior.
#' @param leader character, statement prepended to the message
#' @param paste.sep character, by default a space separates message parts
#' @param append if TRUE then append to the log file, otherwise overwrite
#' @param sep character, by default \code{\\n} separates one message from subsequent ones
#' @param do_timestamp logical, if TRUE append a timestamp to the message
#' @param do_echo logical if TRUE then echo the output to the console.
#' @param do_sprintf logical, if TRUE (the default) the ... arguments are passed to \code{sprintf}
#'    if false the ... are passed to \code{paste}
#' @return the message is returned invisibly
NULL
LoggerRefClass$methods(
   log = function(..., leader = paste0('[',.self$name,']'),
      paste.sep = " ", append = TRUE, sep = "\n",
      do_timestamp = .self$.timestamp, do_echo = .self$.echo,
      do_sprintf = TRUE){

   if (length(list(...)) < 2){
      content <- paste(..., collapse = paste.sep)
   } else {
      content <- if (do_sprintf) sprintf(...) else paste(..., collapse = paste.sep)
   }
   msg <- ifelse(do_timestamp,
      paste(leader, format(Sys.time()), content , sep = paste.sep) ,
      paste(leader, content, sep = paste.sep) )
   if (.self$.hasfile) {
      cat(msg, sep = sep, file = .self$filename, append = append)
   }
   if (do_echo) {
      cat(msg, sep = sep)
      flush.console()
   }
   invisible(msg)
   })

#' Set the filename attribute
#'
#' @family Logger
#' @name Logger_setfilename
#' @param file the name of the file
NULL
LoggerRefClass$methods(
   setfilename = function(file){
      .self$field('filename',file)
      .self$.hasfile <- !is.na(.self$filename)
   })

#' Output an informative message
#'
#' @name Logger_info
#' @family Logger
#' @param ... message parts, will be pasted together with paste.sep
#' @param leader character, statement prepended to the message
NULL
LoggerRefClass$methods(
   info = function(..., leader = paste0('[', .self$name,' info]')){
      .self$log(..., leader = leader)
   })

#' Output a warning message
#'
#' @name Logger_warn
#' @family Logger
#' @param ... message parts, will be pasted together with paste.sep
#' @param leader character, statement prepended to the message
NULL
LoggerRefClass$methods(
   warn = function(..., leader = paste0('[', .self$name,' warning]')){
      .self$log(..., leader = leader)
   })

#' Output an error message
#'
#' @name Logger_error
#' @family Logger
#' @param ... message parts, will be pasted together with paste.sep
#' @param leader character, statement prepended to the message
NULL
LoggerRefClass$methods(
   error = function(..., leader = paste0('[', .self$name,' error]')){
      .self$log(..., leader = leader)
   })

#' Output the elapsed time
#'
#' Useage: X$elapsed(name = 1)
#
#' @name Logger_elapsed
#' @family Logger
NULL
LoggerRefClass$methods(
   elapsed = function(watchname = .self$name){
      .self$info(.self$timer$elapsed(name = watchname) )
   })

#' Create a script report
#'
#' @family Logger
#' @name Logger_scriptreport
#' @param name character, the name of the script or what have you
#' @param args character vector of script arguments
#' @param do_echo logical, set to TRUE to echo to the command line
#' @examples \dontrun{
#'    args <- commandArgs(trailingOnly = FALSE)
#'    X <- Logger(name = 'test log')
#'    X$scriptreport(args)
#' }
NULL
LoggerRefClass$methods(
   scriptreport = function(name = .self$name,
      args = commandArgs(trailingOnly = FALSE),
      do_echo = .self$.echo){
   old_echo <- .self$.echo
   .self$.echo <- do_echo
   old_timestamp <- .self$.timestamp
   .self$.timestamp <- FALSE
   leader <- ""
   .self$log("Script report for %s at %s", name, Sys.time(), leader = leader)
   if (!is.null(args)) {
      .self$log("   script args follow...",leader = leader, do_sprintf = FALSE)
      .self$log( paste("      ", paste(args, collapse = " "), collapse = " "), leader = leader, do_sprintf = FALSE)
   }
   .self$log("   whoami: %s", system("whoami", intern = TRUE), leader = leader)
   .self$log("   hostname: %s", system("hostname", intern = TRUE), leader = leader)
   .self$log("   Rscript: %s", Sys.which("Rscript"), leader = leader)
   .self$log("   R: %s", Sys.which("R"), leader = leader)
   .self$log("   %s", R.version.string, leader = leader)
   .self$.echo <- old_echo
   .self$.timestamp <- old_timestamp
   invisible(NULL)
   })


#' Retrieve the tail of a log file
#'
#' @family Logger
#' @name Logger_tail
#' @param n number of lines to retrieve
NULL
LoggerRefClass$methods(
    tail = function(n = 10){


    if (!file.exists(.self$filename)){
        .self$error("unable to tail as the log file doesn't exist yet")
        return(character(0))
    }
    app <- Sys.which("tail")
    if (nchar(app) > 0){
        cmd <- sprintf("%s -n %i %s", app, as.integer(n), .self$filename)
        x <- system(cmd, intern = TRUE)
    } else {
        .self$error("tail command not found")
       x <-  character(0)
    }


    x
    })


#' Roll the log file to a new one based upon file size
#'
#' @name Logger_roll
#' @param max_size numeric the maximum log file size in bytes to trigger rotation
#' @return invisible, the size of the current log file (possibly rotated)
NULL
LoggerRefClass$methods(
    roll = function(max_size = 1e5){

    if (!file.exists(.self$filename)){
        .self$error("unable to roll as the log file doesn't exist yet")
        return(0L)
    }

    fi = file.info(.self$filename)
    if (fi$size[1] > max_size){
        ff  = list.files(dirname(.self$filename),
                         pattern = glob2rx(paste0(basename(.self$filename),".*")),
                         full.names = TRUE, recursive = FALSE)
        dst = paste0(.self$filename, ".", length(ff)+1)
        ok  = file.rename(.self$filename, dst)
        cat("[New Log File]", as.character(Sys.time()), "\n",
            file = .self$filename, append = FALSE)
        fi = file.info(.self$filename)
    }
    invisible(fi$size[1])
    })

######
#     methods-above functions-below
######

#' Create a LoggerRefClass object
#'
#' @family Logger
#' @export
#' @param name the name of the logger, I usually make it the name of the script
#' @param filename the name of the file to send the messages to
#' @param do_echo logical to output to sterr
#' @param do_timestamp if TRUE then prepend a time stamp to messages
#' @param start_watch logical, if TRUE start the stopwatch
#' @param roll_file logical, if TRUE then check to see if the file should be rolled
#'      before use.
#' @param ... further arguments for rolling the log file
#' @return a LoggerRefClass object
Logger <- function(name = 'logger',
   filename = NA,
   do_echo = interactive(),
   do_timestamp = TRUE,
   start_watch = TRUE,
   roll_file = TRUE,
   roll_size = 100000L) {

   X <- LoggerRefClass$new()
   X$field("name", name)
   X$field(".hasfile", !is.na(filename))
   if (!is.na(filename)) X$setfilename(filename)
   X$field(".echo", do_echo)
   X$field(".timestamp", do_timestamp)
   X$field("timer", Timekeeper(name = name, start_watch = start_watch))
   X$field(".autoroll",  roll_file)
   X$field(".rollsize", roll_size)
   if (X$.autoroll) bytes = X$roll_file(max_size = X$.rollsize)
   X
}
