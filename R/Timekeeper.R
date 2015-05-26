# Timekeeper.R

#' A class for storing one or more StopwatchRefClass objects
#'
#' @field fob a lost of StopwatchRefClass objects
TimekeeperRefClass <- setRefClass('TimekeeperRefClass',
   fields = list(fob = 'list'))
   
#' Start a stopwatch
#'
#' @family Stopwatch Timekeeper
#' @name Timekeeper_start
#' @param name either a name or index number (1,2,3,...) for the stopwatch
NULL
TimekeeperRefClass$methods(
   start = function(name = 1){
      if (.self$has_watch(name)) {
         # start it if needed
         if (.self$fob[[name[1]]]$is_running) {
            cat("Timekeeper::start Stopwatch,", name, "is already running")
         } else {
            .self$fob[[name[1]]]$start()
         }
      } else {
         .self$fob[[name[1]]] <- Stopwatch(name[1], start_watch = TRUE)
      }
      invisible(NULL)
   })
  
#' Determine if a specified stopwatch exists
#' 
#' @family Stopwatch Timekeeper
#' @name Timekeeper_has_watch
#' @param name either a name or index number (1,2,3,...) for the stopwatch
#' @return logical TRUE if the specified name points to a Stopwatch
NULL
TimekeeperRefClass$methods(
   has_watch = function(name = 1){
      if (missing(name)) {
         cat("Timekeeper::has_watch name or index is required\n")
         return(FALSE)
      }
      return(!is.null(.self$fob[[name[1]]]) )
   })

#' Stop a stopwatch
#'
#' @family Stopwatch Timekeeper
#' @name Timekeeper_stop
#' @param name either a name or index number (1,2,3,...) for the stopwatch
#' @param ... further arguments for StopWatch (espcially interval)
NULL
TimekeeperRefClass$methods(
   stop = function(name = 1,...){
      if (missing(name)){
         cat("Timekeeper::stop name or index is required\n")
         return(invisible(NULL))
      } 
      if (.self$has_watch(name)) {
         if (!.self$fob[[name[1]]]$is_running) {
            cat("Timekeeper::stop Stopwatch,", name, "not running\n")
         } else {
            .self$fob[[name[1]]]$stop(...)
         }
      } else {
         cat("Timekeeper::stop There is no Stopwatch at index/name:", name[1], "\n")
      }
      invisible(NULL)
   })


#' Reset a stopwatch
#'
#' @family Stopwatch Timekeeper
#' @name Timekeeper_reset
#' @param name either a name or index number (1,2,3,...) for the stopwatch
NULL
TimekeeperRefClass$methods(
   reset = function(name = 1){
      if (.self$has_watch(name)) {
         .self$fob[[name[1]]]$reset()
      } else {
         cat("Timekeeper::reset There is no Stopwatch at index/name:", name[1], "\n")
      }
      invisible(NULL)
   })


#' Determine elapsed time for a stopwatch
#'
#' @family Stopwatch Timekeeper
#' @name Timekeeper_elapsed
#' @param name either a name or index number (1,2,3,...) for the stopwatch
#' @param to_string logical, if TRUE then call the Stopwatch's to_string method
#' @param ... further arguments for the \code{to_string} method
NULL
TimekeeperRefClass$methods(
   elapsed = function(name = 1, to_string = TRUE, ...){
      x <- invisible(NULL)
      if (.self$has_watch(name)) {
         x <- ifelse(to_string,
            .self$fob[[name[1]]]$to_string(...),
            .self$fob[[name[1]]]$elapsed(...))
      } else {
         cat("Timekeeper::elapsed There is no Stopwatch at index/name:", name[1], "\n")
      }
      x
   })
   

#####
#     methods above, functions below
#####

#' Create a Timekeeper which holds one or more stopwatches on its fob.
#' 
#' @family Stopwatch Timekeeper
#' @export
#' @param name if start_watch is TRUE, then a name must be provided
#' @param start_watch logical, if TRUE start a new Stopwatch
#' @examples \dontrun{
#'    # create a container class, starting a stopwatch right away
#'    timer <- Timekeeper(start_watch = TRUE, name = 'master')
#'    Sys.sleep(3)
#'    # add another stopwatch
#'    timer$start("another")
#'    # let's see what we have so far
#'    timer
#'    timer$stop("another")
#'    timer$elapse("master") }
#' @return an instance of TimekeeperRefClass
Timekeeper <- function(name = 1, start_watch = TRUE){
   X <- TimekeeperRefClass$new()
   X$field("fob", list())
   if (start_watch) X$start(name = name)
   X
}