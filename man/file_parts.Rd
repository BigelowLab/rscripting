% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/file.R
\name{file_parts}
\alias{file_parts}
\title{Divide a fully qualified filename into directory, name and extension parts}
\usage{
file_parts(filename, extSep = ".")
}
\arguments{
\item{filename}{the fully qaulified file name to parse}

\item{extSep}{the extension separator}
}
\value{
a character vector of file parts, or a list of
   character vectors if the input has more than one element
}
\description{
Divide a fully qualified filename into directory, name and extension parts
}
\seealso{
Other FILE: \code{\link{cp_dir}},
  \code{\link{get_temp_dirname}},
  \code{\link{get_temp_filename}}, \code{\link{rsync_dirs}}
}
