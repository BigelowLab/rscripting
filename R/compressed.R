
#' Test if a given file is compressed by zip or tgz, gzip.
#' 
#' @family COMPRESSED
#' @export
#' @param filename the fully qualified name of the file to test
#' @return a character vector of 'zip', 'gzip' or "tgz", "unknown"
test_compressed <- function(filename){
   ok <- is_zip(filename) ; if (ok) return("zip")
   ok <- is_tgz(filename) ; if (ok) return("tgz")
   ok <- is_gzip(filename) ; if (ok) return("gzip")
   ok <- is_bzip2(filename); if (ok) return("bzip2")
   return("unknown")
}

#' Test if a file is tarred and gzip (.tar, .gz or tgz)
#' 
#' We guess that the first four bytes tell us enough to know what kind of zip
#' file we have.  For more information see \url{http://pank.org/blog/archives/000202.html}
#' tgz's first four bytes will be 1f 8b 08 00
#' @family COMPRESSED
#' @export
#' @param filename the fully qualified name of the file to test
#' @return logical, TRUE if the input is likely gzipped
is_tgz <- function(filename) {
   if (missing(filename)) stop("filename is required")
   if (length(filename) > 1) return(sapply(filename, is_tgz))
   fi <- file.info(filename)
   if (fi[,"isdir"]) return(FALSE)
   
   con <- file(filename, open = "rb")
   x = readBin(con, "raw", size = 1, n = 4, endian = "little")
   close(con)
   identical(rawToChar(x, multiple = TRUE), c("\037", "\x8b", "\b", ""))
}
 


#' Test if a file is gzipped
#' 
#' We guess that the first two bytes tell us enough to know what kind of zip
#' file we have.  For more information see \url{http://www.gzip.org/zlib/rfc-gzip.html#file-format}
#' and \url{https://www.ietf.org/rfc/rfc1952}
#' 
#' @family COMPRESSED
#' @export
#' @param filename the fully qualified name of the file to test
#' @return logical, TRUE if the input is likely gzipped
is_gzip <- function(filename){
   # ID1 (IDentification 1)
   # ID2 (IDentification 2)
   # These have the fixed values ID1 = 31 (0x1f, \037), ID2 = 139
   #   (0x8b, \213), to identify the file as being in gzip format.
   
   if (missing(filename)) stop("filename is required")
   if (length(filename) > 1) return(sapply(filename, is_gzip))
   fi <- file.info(filename)
   if (fi[,"isdir"]) return(FALSE)
   
   con <- file(filename, open = "rb")
   x = readBin(con, "raw", size = 1, n = 2, endian = "little")
   close(con)
   identical(rawToChar(x, multiple = TRUE), c("\037", "\x8b"))
}


#' Test if a file is gzipped
#' 
#' We guess that the first two bytes tell us enough to know it is bzip2
#' file we have.  For more information see \url{https://en.wikipedia.org/wiki/Bzip2#File_format}
#' and \url{https://www.ietf.org/rfc/rfc1952}
#' 
#' @family COMPRESSED
#' @export
#' @param filename the fully qualified name of the file to test
#' @return logical, TRUE if the input is likely gzipped
is_bzip2 <- function(filename){
   # ID1 (IDentification 1)
   # ID2 (IDentification 2)
   # These have the fixed values ID1 = 31 (0x1f, \037), ID2 = 139
   #   (0x8b, \213), to identify the file as being in gzip format.
   
   if (missing(filename)) stop("filename is required")
   if (length(filename) > 1) return(sapply(filename, is_gzip))
   fi <- file.info(filename)
   if (fi[,"isdir"]) return(FALSE)
   
   con <- file(filename, open = "rb")
   x = readBin(con, "raw", size = 1, n = 2, endian = "little")
   close(con)
   identical(rawToChar(x, multiple = TRUE), c("B", "Z"))
}

#' Test if a file is zipped.
#' 
#' @family COMPRESSED
#' @export
#' @param filename the fully qualified name of the file to test
#' @return logical, TRUE if the input is likely zipped
is_zip <- function(filename){

   if (missing(filename)) stop("filename is required")
   if (length(filename) > 1) return(sapply(filename, is_zip))
   fi <- file.info(filename)
   if (fi[,"isdir"]) return(FALSE)
   
   con <- file(filename, open = "rb")
   x = readBin(con, "integer", size = 4, endian = "little")
   close(con)
   sprintf("0x%X", x) == "0x4034B50"  
}


#' Unzip a zipped archive.
#' 
#' Unzipping a file can be a bit tricky if the file was zipped on a Mac OSX
#' which may include MACOSX/ and DS_STORE stuff we don't usually want.
#' This function tries to clean up the output.
#' 
#' @family COMPRESSED
#' @export
#' @param filename the fully qualified name of the file to unzip
#' @param extra extra arguments for the system's \code{unzip} command
#' @return a character vector of the uncompressed filenames including relative paths
un_zip <- function(filename, extra = "-x *MACOSX*/* *DS_Store*"){
   
   if (length(filename) > 1) return(lapply(filename, un_zip, extra = extra))

   if (!is.zip(filename)) stop("Input file must be zipped")
   
   oldDir <- setwd(dirname(filename))
   
   FF0 <- dir(recursive = TRUE, full.names = TRUE)
   
   CMD <- paste("unzip -q", shQuote(filename), extra)
   ok <- system(CMD, ignore.stdout = TRUE)
   if (ok != 0) stop(paste("Error running:", CMD))
   
   FF1 <- dir( recursive = TRUE, full.names = TRUE)
   ix <- !(FF1 %in% FF0)
   if (any(ix)) {
      FF1 <- normalizePath(FF1[ix])
   } else {
      FF1 <- ""
   }
   setwd(oldDir)
   FF1
}

#' Unpack a tarred and gzipped archive.
#' 
#' 
#' @family COMPRESSED
#' @export
#' @param filename the fully qualified name of the file to unpack
#' @param extra extra arguments for the system's \code{tar} command
#' @return a character vector of the uncompressed filenames including relative paths
un_tgz <- function(filename, extra = "-xf"){

   if (length(filename) > 1) return(lapply(filename, un_tgz))
   
   if (!is.tgz(filename)) stop("Input file must be tarred and gzipped")
   
   oldDir <- setwd(dirname(filename))
   
   FF0 <- dir( recursive = TRUE, full.names = TRUE)
   
   CMD <- paste("tar", extra, filename)
   ok <- system(CMD, ignore.stdout = TRUE)
   if (ok != 0) stop(paste("Error running:", CMD))
   
   FF1 <- dir( recursive = TRUE, full.names = TRUE)
   ix <- !(FF1 %in% FF0)
   if (any(ix)) {
      FF1 <- normalizePath(FF1[ix])
   } else {
      FF1 <- ""
   }
   setwd(oldDir)
   FF1
}


#' Unpack a tarred and bzip2-ped archive.
#' 
#' @family COMPRESSED
#' @export
#' @param filename the fully qualified name of the file to unpack
#' @param extra extra arguments for the system's \code{tar} command
#' @return a character vector of the uncompressed filenames including relative paths
un_bzip2 <- function(filename, extra = "-xf"){

   if (length(filename) > 1) return(lapply(filename, un_bzip2))
   
   if (!is.tgz(filename)) stop("Input file must be tarred and bzip2-ped")
   
   oldDir <- setwd(dirname(filename))
   
   FF0 <- dir( recursive = TRUE, full.names = TRUE)
   
   CMD <- paste("tar", extra, filename)
   ok <- system(CMD, ignore.stdout = TRUE)
   if (ok != 0) stop(paste("Error running:", CMD))
   
   FF1 <- dir( recursive = TRUE, full.names = TRUE)
   ix <- !(FF1 %in% FF0)
   if (any(ix)) {
      FF1 <- normalizePath(FF1[ix])
   } else {
      FF1 <- ""
   }
   setwd(oldDir)
   FF1
}


#' Unpack a gzipped archive.
#' 
#' 
#' @family COMPRESSED
#' @export
#' @param filename the fully qualified name of the file to unpack
#' @param out the name of the outfile to create, if NA then it is whatever gets unpacked.
#'    If you specify \code{out} be sure that \code{extra} gets at least '-cd'
#' @param extra extra arguments for the system's \code{gzip} command
#' @return a character vector of the uncompressed filenames including relative paths
un_gz <- function(filename, out = NA, extra = "-d"){

   if (length(filename) > 1) return(lapply(filename, un_tgz))
   
   if (!is.gzip(filename)) stop("Input file must be gzipped")
   
   oldDir <- setwd(dirname(filename))
   FF0 <- dir( recursive = TRUE, full.names = TRUE)
   
   CMD <- paste("gzip", extra, filename)
   if (!is.na(out)) CMD <- paste(CMD, ">", out)
      
   ok <- system(CMD, ignore.stdout = TRUE)
   if (ok != 0) stop(paste("Error running:", CMD))
   
   FF1 <- dir(recursive = TRUE, full.names = TRUE)
   ix <- !(FF1 %in% FF0)
   if (any(ix)) {
      FF1 <- normalizePath(FF1[ix])
   } else {
      FF1 <- ""
   }
   setwd(oldDir)
   FF1
}

#' Unpack a compressed archive: zip, gzip, and gzipped tarballs.  
#' 
#' This is a loose routine to unpack and list the contents of a file, possibely 
#' a compressed file. If a directory is provided then return a list of the contents,  if an
#' uncompressed file (or some other thing) is provided then return just that.
#'
#' See \code{\link{un_zip}}, \code{\link{un_gz}} and \code{\link{un_tgz}}
#' 
#' 
#' @family COMPRESSED
#' @export
#' @param filename the fully qualified name(s) of the file to unpack
#' @param ... extra arguments for the system's  decompression command (unzip, gzip -d, tar -x)
#' @return a character vector of the uncompressed filenames including relative paths
#'    or a list of such if multiple filenames are provided
unpack_archive <- function(filename, ...){

   if (length(filename) > 1) return(lapply(filename, unpack_archive, ...))
      
   R <- switch(test.compressed(filename),
      "zip" = un_zip(filename, ...),
      "tgz" = un_tgz(filename, ...),
      "gzip" = un_gz(filename, ...),
      "unknown")
   
   # if we were passed a directory, then we simply mine the directory for the contents
   # if it isn't then we simple return the filename
   if (R[1] == "unknown"){
      info <- file.info(filename)
      if (info[filename, "isdir"]) {
         R <- normalizePath(dir(filename, full.names = TRUE, recursive = TRUE))
      } else {
         R <- filename
      }
   }
   
   return(R)
}

#' Create a zipped or tarred and gzipped archive of a directory
#' 
#' @export
#' @family COMPRESSED
#' @param directory one or more directories to pack. NOTE they are not packed together.
#' @param fmt character, indicates output format.  Currently just 'tgz' and 'zip'
#' @param outfile the full qualified name(s) of the output archive(s)
#' @return named logical indicating the outfile exists (or not!)
pack_directory <- function(directory, fmt = c('tgz', 'zip')[1],
   outfile = file.path(dirname(directory), 
      paste0(basename(directory), c('tgz'='.tar.gz', zip='.zip')[fmt])) ){
   
   for (i in seq_along(directory)){   
       if (fmt == "tgz"){
          cmd <- paste("tar -zcf", shQuote(outfile[[i]]), "-C", shQuote(directory[[i]]), ".")
          ok <- system(cmd)
       } else if (fmt == 'zip'){
          # whole lot of tricks here
          # move the parent of the target directory
          orig <- setwd(dirname(directory[[i]]))
          # zip in that parent directory
          cmd <- paste("zip -rq", shQuote(basename(outfile[[i]])), shQuote(basename(directory[[i]])))
          #cat(cmd, "\n")
          ok <- system(cmd)
          # go back to where we started
          setwd(orig)
          # move the zipped file here
          ok <- file.rename(file.path(dirname(directory[[i]]), basename(outfile[[i]])), outfile[[i]])
          # whew!
       } else {
          stop(paste("format not known:", fmt))
       }
   }
   
   sapply(unname(outfile), file.exists)
}