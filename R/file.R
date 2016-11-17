#' Retrieve a temporary directory name 
#'
#' This fucntion attempts to create uniquely name directories.
#' 
#' @family FILE
#' @export
#' @param pattern character if provided this will be prepended on the temp directory returned
#' @param ramdisk logical if TRUE, then try to get a path into the ramdisk first, if not
#'    available then use the standard tempdir
#' @param create logical if TRUE then create the path
#' @return the fully qualified temporary path
get_temp_dirname <- function(pattern = "", ramdisk = file.exists("/dev/shm"), create = TRUE){
   if (ramdisk){
      stub <- "/dev/shm"
      if (!file.exists(stub)) {
         stub <- tempdir()
      }
   } else {
      stub <- tempdir()
   }
   path <- tempfile(pattern=pattern, tmpdir = stub)
   if (create){ 
      ok <- dir.create(path, recursive = TRUE)
      if (!ok) warning(paste("Unable to create temporary directory", path))
   }
   path
}

#' Create a temporary file name
#'
#' @family FILE
#' @export
#' @param pattern character if provided this will be prepended on the filename returned
#' @param path charcater the path to attach the file within
#' @param ext charcater any extension to add to the filename generated
get_temp_filename <- function(pattern = "", path = get_temp_dirname(), ext = ''){

   file.path(path, basename(tempfile(pattern = pattern, fileext = ext )) )
}
 

#' Sync two directories using the system \code{rsync}
#'
#' In rsync there is a complete test on the change of a file before copying.  Using 
#' cp_dir perfoms a simpler test for changes, so it might be faster.  I think for
#' most pipeline uses it may be better to use cp_dir since pipelines are a once-and-done
#' task. 
#'
#' @family FILE
#' @export
#' @param src character the fully qualified source directory 
#' @param dst character the fully qualified destination directory
#' @param flags charcater flags for rsync, by default '-rvc'
#' @return the output of the systems rsync command (0 = good!)
rsync_dirs <- function(src, dst, flags = '-rcv'){
   if (file.exists(src[1]) && file.exists(dst[1]) ){ 
      cmd <- sprintf("rsync %s %s %s", flags, src, dst)
      ok <- system2(cmd)
   } else {
      cat("Please verify that src and dst exist\n")
      ok <- 1 
   }
   ok
}

#' Copy the contents of one directory to another using system's \code{cp}.  
#'
#' Note that differs from rsync_dirs
#' 
#'
#' @family FILE
#' @export
#' @param src character the fully qualified source directory 
#' @param dst character the fully qualified destination directory
#' @param flags charcater flags for cp, by default "-uR"
#' @return the output of the systems cp command (0 = good!)
cp_dir <- function(src, dst, flags = "-uR"){
   if (file.exists(src[1]) && file.exists(dst[1]) ){
      cmd <- sprintf("cp %s %s %s", flags, src, dst)
      ok <- system2(cmd)
   } else {
      cat("Please verify that src and dst exist\n")
      ok <- 1 
   }  
   ok
}

#' Divide a fully qualified filename into directory, name and extension parts
#'
#' @family FILE 
#' @export
#' @param filename the fully qaulified file name to parse
#' @param extSep the extension separator
#' @return a character vector of file parts, or a list of 
#'    character vectors if the input has more than one element
file_parts <- function(filename, extSep = "."){
   
   if (length(filename) > 1) {
      r <- lapply(filename, file_parts)
   } else {
      DIR <- dirname(filename)
      BASE <- basename(filename)
      ix <- gregexpr(extSep, BASE, fixed =TRUE)[[1]]
      if (ix[1] > 0) {
         EXT <- substring(BASE, ix[length(ix)]+1, nchar(BASE))
         name <- gsub(paste0(extSep, EXT), "", BASE, fixed = TRUE)
      } else {
         EXT = ""
         name = BASE
      }   
      r <- c(dir = DIR, name = name, ext = EXT)
   }
   return(r)
}
