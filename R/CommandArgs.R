# CommandArgs.R
######
#     CommandArgsRefClass
######
CommandArgsRefClass <- setRefClass("CommandArgsRefClass",
   fields = list(
      cmdargs = 'character',  # character vector
      app = 'character',   # the name of the application
      options = 'character',    # character vector of --options
      filename = 'character', # the name of the script called
      Args = 'list',  # of ArgumentRefClass objects
      help = 'character')
   )
   
   
#' Parse the argument vector into \code{\link{Argument}} objects
#'
#' @family CommandArgs
#' @name CommandArgs_parse_arguments
#' @param args input of the value of \code{\link{commandArgs}} or NULL to use
#'    what is already stored
#' @return a vector of logicals, one per argument
NULL
CommandArgsRefClass$methods(
   parse_arguments = function(args=NULL){
      if (!is.null(args)) x$field("cmdargs", args)
      if (is.null(.self$cmdargs)) stop("args must be supplied in parse_arguments() or CommandArgs()")
      allargs <- .self$cmdargs
      .self$field("app", allargs[[1]])
      ix <- grep("--file", allargs)
      if (length(ix[[1]]) > 0){
         .self$field("filename", gsub("--file=", "", allargs[ix[[1]]]))
         if (ix > 2) .self$field("options", allargs[2:(ix[1]-1)])
      }
      
      OK <- TRUE
      ix <- grep("--args", allargs, fixed = TRUE)
      if ((length(ix) > 0) && (length(allargs) > ix[1] ) ) {
         trailingArgs <- allargs[(ix[1] + 1) : length(allargs)]
         nm <- names(.self$Args)
         OK <- rep(FALSE, length(nm)) ; names(OK) <- nm
         for (n in nm) OK[[n]] <- .self$Args[[n]]$parse_argument(trailingArgs)
      } else {
         .self$print_help()
         OK <- FALSE
      }
      invisible(OK)
   })

#' Add an argument definition
#' See \code{link{Argument}} for input details
#'
#' @family CommandArgs
#' @name CommandArgs_add_argument
#' @param name the name of the argument to add
#' @param ... furtehr arguments for the \code{\link{Argument}} to be defined
NULL
CommandArgsRefClass$methods(
   add_argument = function(name, ...){
      .self$Args[[name]] <- Argument(name, ...)
   })

#' Add an argument definition
#' See \code{link{Argument}} for input details
#'
#' @family CommandArgs
#' @name CommandArgs_add
#' @param name the name of the argument to add
#' @param ... further arguments for the \code{\link{Argument}} to be defined
NULL
CommandArgsRefClass$methods(
   add = function(name, ...){
      .self$Args[[name]] <- Argument(name, ...)
   })


#' Print information about the object
#'
#' @family CommandArgs
#' @name CommandArgs_show
NULL
CommandArgsRefClass$methods(
   show = function(){
      cat("Reference Class:", classLabel(class(.self)), "\n")
      cat("args:", dQuote(paste(.self$cmdargs, collapse = " ")), "\n")
      cat("app:", .self$app, "\n")
      cat("options:", .self$options, "\n")
      cat("filename:", .self$filename, "\n")
      cat("Argument listing follows...\n")
      nm <- names(.self$Args)
      for (n in nm) .self$Args[[n]]$show()
   })
  
#' Print usage help
#'
#' @family CommandArgs
#' @name CommandArgs_print_help
NULL
CommandArgsRefClass$methods(
   print_help = function(commandstring = 'program_name'){
      nm <- names(.self$Args)
      u <- sapply(.self$Args, function(x) x$get_usage())
      u <- strwrap(c("Usage: ",commandstring, u), exdent = 5, width = 80)
      writeLines(u)
      cat("\nArgument details follow\n")
      for (n in nm) .self$Args[[n]]$print_help()
   })

  
#' Test is the named argument is present
#'
#' @family CommandArgs
#' @name CommandArgs_get   
#' @param name the name of the argument to test
#' @return logical indicating is (or is not) present
NULL
CommandArgsRefClass$methods(
   has = function(name){
      name %in% names(.self$Args)
   })

#' Get the value(s) of an argument
#' 
#' @family CommandArgs
#' @name CommandArgs_get   
#' @param name the name of the argument to retrieve
#' @param ... futher arguments for the \code{Argument$get()} method requested
#' @return the value of the argument (type varies)
NULL
CommandArgsRefClass$methods(
   get = function(name, ...){
      .self$Args[[name]]$get(...)
   })
  

#' Get all arguments
#' 
#' @family CommandArgs
#' @name CommandArgs_get_all   
#' @param ... futher arguments for the \code{Argument$get()} method requested
#' @return a list of key-value pairs
NULL
CommandArgsRefClass$methods(
   get_all = function(...){
      name <- names(.self$Args)
      names(name) <- name
      lapply(name, function(x, ARGS,...) {ARGS[[x]]$get(...)}, .self$Args, ...)
   })
    
    
# Note from Joe
# the convention is single dash for single letter args and double dash for multi-letter
   
######
#     methods above, functions below
######
#' Generate a CommandArgs reference
#' 
#' @family CommandArgs
#' @export
#' @param args a character vector as returned by \code{\link{commandArgs}} 
#'    or NULL
#' @param help a character vector of helpful information
#' @return a CommandArgsRefClass instance
CommandArgs <- function(args = commandArgs(trailingOnly = FALSE),
   help = NULL){
   x <- CommandArgsRefClass$new()
   x$field("Args", list())
   if (!is.null(args)) x$field("cmdargs", args)
   if (!is.null(help)) x$field("help", help) 
   x
}

