# Logger.R
# info, warn, error
LoggerRefClass <- setRefClass("LoggerRefClass",
   fields = list(
      name = 'character',
      echo = 'logical',
      filename = 'character',
      hasfile = 'logical',
      timestamp = 'logical',
      timer = 'ANY'))

#' Log messages, possible echoed to the command line
#' 
#' @family Logger
#' @name Logger_log
#' @param ... message parts, will be pasted together with paste.sep
#' @param leader character, statement prepended to the message
#' @param paste.sep character, by default a space separates message parts
#' @param append if TRUE then append to the log file, otherwise overwrite
#' @param sep character, by default \code{\\n} separates one message from subsequent ones
#' @param do_timestamp logical, if TRUE append a timestamp to the message
#' @param do_echo logical if TRUE then echo the output to the console.
#' @return the message is returned invisibly
NULL
LoggerRefClass$methods(
   log = function(..., leader = paste0('[',.self$name,']'), 
      paste.sep = " ", append = TRUE, sep = "\n",
      do_timestamp = .self$timestamp, do_echo = .self$echo){
   
   msg <- ifelse(do_timestamp,
      paste(leader, format(Sys.time()), ..., sep = paste.sep) ,
      paste(leader, ..., sep = paste.sep) )
   if (.self$hasfile) {
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
      .self$hasfile <- !is.na(.self$filename)
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
      do_echo = .self$echo){
   old_echo <- .self$echo
   .self$echo <- do_echo
   old_timestamp <- .self$timestamp
   .self$timestamp <- FALSE
   leader <- ""
   .self$log("Script report for", name, "at", Sys.time(), leader = leader)
   if (!is.null(args)) {
      .self$log("   script args follow...",leader = leader)
      .self$log( paste("      ", args, collapse = "\n"), leader = leader)
   }
   .self$log("   whoami:", system("whoami", intern = TRUE), leader = leader)
   .self$log("   hostname:", system("hostname", intern = TRUE), leader = leader)
   .self$log("   Rscript:", system('bash --login -c "which Rscript"', intern = TRUE), leader = leader)
   .self$log("   R:", system('bash --login -c "which R"', intern = TRUE), leader = leader)
   .self$log("  ", R.version.string, leader = leader) 
   .self$echo <- old_echo
   .self$timestamp <- old_timestamp
   invisible(NULL)
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
#' @return a LoggerRefClass object
Logger <- function(name = 'logger', 
   filename = NA, 
   do_echo = TRUE,
   do_timestamp = TRUE,
   start_watch = TRUE) {
 
   X <- LoggerRefClass$new()
   X$field("name", name)
   X$field("hasfile", !is.na(filename))
   if (!is.na(filename)) X$setfilename(filename)
   X$field("echo", do_echo)
   X$field("timestamp", do_timestamp)
   X$field("timer", Timekeeper(name = name, start_watch = start_watch))
   #if (start_watch) X$info("begin logging")
   X
}
      