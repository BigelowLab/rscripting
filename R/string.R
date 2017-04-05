#' Trim leading spaces, trailing spaces or both.
#'
#' @export
#' @param x character vector
#' @param where character of either 'leading', 'trailing', 'both' (default)
#' @param replace_with character, by default ""
#' @return trimmed character vector of same length as the input
str_trim <- function(x, where = c('leading', 'trailing', 'both')[3],
    replace_with = ""){

    p <- switch(tolower(where),  
        'both' = "^\\s+|\\s+$",
        'leading' = "^\\s+",
        'trailing' = "\\s+$")
    
    gsub(p, replace_with, x)
}

#' Perform grepl on multiple patterns; it's like  AND-ing or OR-ing successive
#'  grepl statements.
#' 
#' @export
#' @param pattern character vector of patterns
#' @param x the character vector to search
#' @param op logical vector operator back quoted, defaults to `|`
#' @param ... further arguments for \code{grepl} like \code{fixed} etc.
#' @return logical vector
mgrepl <- function(pattern, x, op = `|`, ... ){
   Reduce(op, lapply(pattern, grepl, x, ...))
}