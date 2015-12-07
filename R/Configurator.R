#' A class for configuration elements
#' 
#' @field filename the fully qualified name of the configuration file
#' @field container a list of configuration elements - one element per named [section]
#' @field comments a list of configuration comments by [section] plus a [.header]
#     section for comments preceding the first section
#' @field sep the default key-value pair separator ("=") for writing
#' @field comm the default comment character ("#") flagging comment lines
ConfiguratorRefClass <- setRefClass('ConfiguratorRefClass', 
   fields = list(
      filename = 'character',
      container = 'list',
      comments = 'list',
      sep = 'character',
      comm = 'character')
)

#' Parse one or more raw text lines of key=value or key:value form.
#'
#' @family CONFIGURATOR
#' @name ConfiguratorRefClass_parse_line
#' @param a character vector of key=value sets
#' @return a named character vector, names are keys data are the values
NULL
ConfiguratorRefClass$methods(
   parse_section = function(s){
      'Given one or more raw text lines, parse the key=value pairs into the 
      expected named character vector'
      # any headers?
      ic <- grepl(paste0("^", .self$comm), s)
      if (any(ic)) {
         com <- s[ic]
         s <- s[!ic]
      } else {
         com <- NULL
      }
      if (length(s) > 0) {
         ix <- regexpr(.self$sep, s)
         r <- vector(mode = "character")
         for (i in seq_along(s)){
            if (ix[i] > 0 ){
               nm <- substring(s[i], 1, ix[i]-1)
               # strip leading spaces
               val <- sub("^ +", "", substring(s[i], ix[i] + 1, nchar(s[i]) ) )
               r[[nm]] <- val
            }
         }
      } else {
         r <- NULL
      }
      list(keyvals = r, comm = com)
   }
)
#' Read a config file comprised of [sections] of key=value pairs.
#'
#' @family CONFIGURATOR
#' @name ConfiguratorRefClass_read_file
#' @param filename the fully qualified name of the configuration file
#' @return the configurations stored in a list
NULL
ConfiguratorRefClass$methods(
   read_file = function(filename = .self$filename){
      'read the specified configuration file'
      if (!file.exists(filename)) stop(paste("file must exist: ", filename))
      #x <- scan(filename, what = character(), quiet = TRUE, sep = "\n")
      x <- readLines(filename)
      ix <- grep("^\\[", x)
      if (length(ix) == 0) {
         stop("No section [headers] found in config file - please check file")
      }
      # leading comments
      if (ix[1] > 1){
         .self$comments <- list(.header = x[1:ix[1]])
      } else {
         .self$comments <- list()
      }
      len <- nchar(x[ix])
      nm <- substring(x[ix], rep(2, length(len)), len-1)
      iy <- c(ix, length(x)+1)
      L <- list()
      C <- .self$comments
      for (i in seq_along(nm)){
         p <- parse_section(x[(iy[i] + 1) : (iy[i+1]-1) ])
         L[[nm[i]]] <- p[['keyvals']] 
         if ("comm" %in% names(p)) C[[nm[i]]] <- p[['comm']]
      }       
      .self$container <- L
      .self$comments <- C
      invisible(L)
   }
)

#' Write a config file comprised of [sections] of key=value pairs.
#'
#' @family CONFIGURATOR
#' @name ConfiguratorRefClass_write_file
#' @param filename the fully qualified name of the configuration file
#' @param append logical, if TRUE append to the specified file otherwise truncate
#' @return logical named result form file.exists
NULL
ConfiguratorRefClass$methods(
   write_file = function(filename = .self$filename, append = FALSE){
      'write the specified configuration file'
      if (is.null(.self$container)) stop("There is nothing to write")
      if (length(.self$container) == 0) stop("There is nothing to write")
      open_mode <- ifelse(append, 'at', 'wt')
      ff <- file(filename, open = open_mode)
      L <- .self$container
      C <- .self$comments
      if (".header" %in% names(C)) for (h in C[['.header']]) cat(h, "\n", file = ff)
      nmL <- names(L)
      for (nm in nmL){
         cat(paste0("[", nm, "]\n"), file = ff)
         if (nm %in% names(C)) for (h in C[[nm]]) cat(h, "\n", file = ff)
         keys <- names(L[[nm]])
         vals <- L[[nm]]
         for (k in keys) cat(paste0(k, .self$sep, vals[[k]], "\n"), file = ff)
      }
      close(ff)     
      sapply(filename, file.exists)
   }
)

#' Retrieve one keyword/value pair by section or an entire section
#'
#' @family CONFIGURATOR 
#' @name ConfiguratorRefClass_get
#' @param section the name of the [section] to search
#' @param name the keyword name of the value to retrieve, if missing then the contents of the section are returned
#' @param default the value to return if the key or section is not found (by default NULL)
#' @return character value(s) or \code{default} if not found
#' @examples \dontrun{
#'    X$get("mySection", "myName")
#' }
NULL
ConfiguratorRefClass$methods(
   get =  function(section, name, default = NULL){
      'Retrieve the specified item in the sectioned named or NULL of not found'
      x <- .self$container[[section[1]]]
      if (!is.null(x) && !missing(name)) {
         if (name[1] %in% names(x)) {
            x <-x[[name[1]]]
         } else { 
            x <- default
         }
      }
      x
   } # get
)

#' Set one keyword/value pair by section
#'
#' @family CONFIGURATOR 
#' @name ConfiguratorRefClass_set
#' @param section the name of the [section] to search
#' @param name the keyword name of the value to set
#' @param value the value, must survive \code{\link{as.character}} conversion
#' @return logical TRUE if successful 
#' @examples \dontrun{
#'    X$set("mySection", "myName", someValue)
#' }
NULL
ConfiguratorRefClass$methods(
   set =  function(section, name, value){
      'Set the value of the specified item in the sectioned named'
      value <- try(as.character(value))
      OK <- !inherits(value, 'try-error')
      if (!OK) {
         cat(value, "\n")
         return(OK)
      }
      .self$container[[section[1]]][[name[1]]] <- value  
      invisible(TRUE)
   } # get
)


#' Print the contents of the Configurator
#'
#' @family CONFIGURATOR 
#' @name ConfiguratorRefClass_show
#' @param ... further arguments for \code{cat}
#' @return NULL invisibly
NULL 
ConfiguratorRefClass$methods(
   show = function(...){
      L <- .self$container
      C <- .self$comments
      nmL <- names(L)
      if (".header" %in% names(C)) for (h in C[['.header']]) cat(h, "\n")
      for (nm in nmL){
         cat(paste0("[", nm, "]\n"), ...)
         if (nm %in% names(C)) for (h in C[[nm]]) cat(h, "\n")
         keys <- names(L[[nm]])
         vals <- L[[nm]]
         for (k in keys) cat(paste0(k, .self$sep, vals[[k]], "\n"), ...)
      }
   }
) #show

#' Create a new ConfiguratorRefClass and possibly read the contents of a 
#' config file.
#'
#' @family CONFIGURATOR
#' @export
#' @param filename a fully qualified path to a config file
#' @param sep character, the character separating key-value pairs, by default "="
#' @param comm character, the character found in the first spot of a comment line, by default "#"
#' @return an instance of ConfiguratorRefClass
Configurator <- function(filename, sep = "=", comm = "#"){
   x <- ConfiguratorRefClass$new()
   x$field("sep", sep[1])
   x$field("comm", comm[1])
   if (!missing(filename)) {
      x$filename <- filename[1]
      x$read_file(filename[1])
   }
   invisible(x)
}
