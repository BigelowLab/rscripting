# Stopwatch.R
StopwatchRefClass <- setRefClass("StopwatchRefClass", 
   fields = list(
      name = 'character',
      t0 = 'POSIXct',
      t1 = 'POSIXct',
      is_running = 'logical',
      dt = 'list'))  # a list of accumulated time differences
      
#' Start the timer, resets the start time to the current time
#' 
#' @family Stopwatch
#' @name Stopwatch_start
NULL
StopwatchRefClass$methods(
   start = function() {
      .self$t0 <- Sys.time()
      .self$is_running <- TRUE
      invisible(NULL)
   })

#' Stop the timer
#' 
#' @family Stopwatch
#' @name Stopwatch_stop
#' @param accumulate logical, if TRUE then add the elapsed time to the
#'    previous elapsed time 
#' @param ... futher arguments for \code{add_elpased} and \code{get_elapsed} method
#' @return the POSIXct elapsed time
NULL
StopwatchRefClass$methods(
   stop = function(interval = NULL ) {
      if (!.self$is_running) stop("Stopwatch is not running, cannot stop")
      .self$t1 <- Sys.time()
      .self$is_running <- FALSE
      .self$add_elapsed(difftime(.self$t1, .self$t0),interval = interval)
      return(.self$get_elapsed(interval = interval))
   })

#' Report the current elapsed time without stopping the clock
#' 
#' @family Stopwatch
#' @name Stopwatch_current_elapsed
#' @return the POSIXct stop time (invisibly)
NULL
StopwatchRefClass$methods(
   current_elapsed = function() {
      difftime(Sys.time(), .self$t0)
   })

#' Report the current elapsed time without stopping the clock
#' 
#' @family Stopwatch
#' @name Stopwatch_elapsed
#' @param ... further arguments for the \code{get_elapsed} method
#' @return the POSIXct stop time (invisibly)
NULL
StopwatchRefClass$methods(
   elapsed = function(...) {
      if (.self$is_running) {
         dtime <- .self$current_elapsed()
      } else {
         dtime <- .self$get_elapsed(...)
      }
      dtime
   })
   
#' Get the elapsed time, possibly accumulating.  If the watch is running then 
#' it is current less start time.  Users should not use this method directly but
#' use the \code{elapsed} method instead.
#' 
#' @family Stopwatch
#' @name Stopwatch_get_elapsed
#' @return the difftime (invisibly)
NULL
StopwatchRefClass$methods(
   get_elapsed = function(accumulate = FALSE, interval = NULL) {
      if (.self$is_running) {
         dtime <- .self$current_elapsed()
      } else {
         n <- length(.self$dt)
         if (n == 0) return(NULL)
         if (!is.null(interval)) {
               index <- interval
         } else {
            if (accumulate){
               index <- seq_len(n)
            } else {
               index <-  n
            }
         }
         dtime <- .self$dt[index]
         if (accumulate && length(dtime) > 1) {
            dtime <- do.call(sum, dtime)
         } else {
            if (length(index) == 1) dtime <- dtime[[1]]
         }
      }
      dtime
   })

#' Add an elapsed time.  If the watch is running then 
#' it is current less start time.  Users should not use this method directly.
#' 
#' @family Stopwatch
#' @name Stopwatch_add_elapsed
#' @param interval elapsed time segments can be tagged by name for later retrieval by name
#' @return the number of dt elements (invisibly)
NULL
StopwatchRefClass$methods(
   add_elapsed = function(dtime, interval = NULL) {
      if (!inherits(dtime,'difftime')) stop("dtime must be of class 'difftime'")
      n <- length(.self$dt)
      if (!is.null(interval)) {
         index <- interval
      } else {
         index <- n + 1
      }
      .self$dt[[index]] <- dtime
      invisible(length(.self$dt))
   })


#' Reset the stopwatch, eliminate acculumated elapsed time intervals. Also
#' if the watch was running, stop it.
#' 
#' @family Stopwatch
#' @name Stopwatch_reset
NULL
StopwatchRefClass$methods(
   reset = function(){
      .self$t0 <- .self$t1 <- Sys.time()
      .self$is_running <- FALSE
      .self$dt <- list()
      invisible(NULL)
   })


#' 
#' Show the elapsed time
#' 
#' @family Stopwatch
#' @name Stopwatch_show
#' @param ... futher arguments
NULL
StopwatchRefClass$methods(
   show = function(...) {
      cat("Stopwatch:", .self$name, "\n")
      cat(" start:", format(.self$t0), "\n")
      if (.self$is_running){
         cat(" stopwatch is running\n")
         cat(" current:", format(Sys.time()), "\n")
      } else {
         cat(" stop:", format(.self$t1), "\n")
      }
      cat(" elapsed:", format(.self$elapsed(), ...), "\n") 
   })

#' Return a formatted elapsed time string
#' 
#' @family Stopwatch
#' @name Stopwatch_to_string
#' @param ... futher arguments for \code{\link{format}} function accepting \code{\link{difftime}}
NULL
StopwatchRefClass$methods(
   to_string = function(..., addname = FALSE) {
      paste0(ifelse(addname, paste0(.self$name, ' '),""), "elapsed time: ", 
         format(.self$elapsed(), ...))
   })



######
#     methods above - functions below
######  
#' Create a StopwatchRefClass object
#' 
#' @export
#' @family Stopwatch Timekeeper
#' @param name the name to assign to this object
#' @param start_watch logical, if TRUE then start the Stopwatch
#' @return a \code{StopwatchRefClass} object
#' @examples \dontrun{
#'    # create stop watch
#'    X <- Stopwatch(name = 'timex', start = FALSE)
#'    #start it
#'    X$start()
#'    X$elapsed()
#'    X
#'    Sys.sleep(5)
#'    # When stopping you can name the interval for later retreival
#'    X$stop(name = 'first')
#'    X
#'    X$elapsed(name = "first")
#'    X$start()
#'    Sys.sleep(2)
#'    X$stop(name = "second")
#'    X$elapsed(name = 'second')
#'    X$elapsed(name = 'first')
#'    # now accumulate the elapsed times
#'    X$elapsed(accumulate = TRUE)}
Stopwatch <- function(name='', start_watch = FALSE){
   x <- StopwatchRefClass$new()
   x$field("name", name)
   x$field("is_running", FALSE)
   x$field("t0", Sys.time())
   x$field("dt", list())
   if (start_watch) x$start()
   x
}