# btupper@scgc-clarity ~ $ free -h
#              total       used       free     shared    buffers     cached
# Mem:           31G        30G       418M       268K       174M        19G
# -/+ buffers/cache:        11G        20G
# Swap:           0B         0B         0B

#' Retrieves information on the system memory resources on platforms where the
#' `free` command exists.
#'
#' @export
#' @param extra character extra arguments for the free command
#' @return data.frame or NULL
system_memory <- function(extra = ""){
    free_cmd <- Sys.which("free")
    if (nchar(free_cmd) == 0){
        cat("free command not found\n")
        return(NULL)
    }    
    if (nchar(extra) > 0) free_cmd <- paste(free_cmd, extra)
    x <- system(free_cmd, intern = TRUE)
    y <- gsub("[[:space:]]+", ",", x)
    z <- read.table(text = y[1:2], header = TRUE, row.names = 1, sep = ",")
    z[,'used_p'] <- z[,'used']/z[,'total'] * 100
    z[,'free_p'] <- z[,'free']/z[,'total'] * 100
    z
}