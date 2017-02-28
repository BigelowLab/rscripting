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