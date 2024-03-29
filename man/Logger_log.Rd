% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Logger.R
\name{Logger_log}
\alias{Logger_log}
\title{Log messages, possible echoed to the command line}
\arguments{
\item{...}{message parts, passed to \code{sprintf} so should start
with a formatting string followed by one or more arguments. See \code{do_sprintf}
to control this behavior.}

\item{leader}{character, statement prepended to the message}

\item{paste.sep}{character, by default a space separates message parts}

\item{append}{if TRUE then append to the log file, otherwise overwrite}

\item{sep}{character, by default \code{\\n} separates one message from subsequent ones}

\item{do_timestamp}{logical, if TRUE append a timestamp to the message}

\item{do_echo}{logical if TRUE then echo the output to the console.}

\item{do_sprintf}{logical, if TRUE (the default) the ... arguments are passed to \code{sprintf}
if false the ... are passed to \code{paste}}
}
\value{
the message is returned invisibly
}
\description{
Log messages, possible echoed to the command line
}
\seealso{
Other Logger: \code{\link{Logger_elapsed}},
  \code{\link{Logger_error}}, \code{\link{Logger_info}},
  \code{\link{Logger_scriptreport}},
  \code{\link{Logger_setfilename}},
  \code{\link{Logger_tail}}, \code{\link{Logger_warn}},
  \code{\link{Logger}}
}
