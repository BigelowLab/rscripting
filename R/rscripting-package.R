#' rscritping: convenience tools for Rscript and more
#'
#' The rscritping package provides a numbr of conveneince tools to help the
#' developer manage scripting, argument parsing, timing, file archiving and
#' the like.
#'
#' @name rscripting
#' @docType package
#' @importFrom methods new
#'
#' @section Argument parsing:
#'
#'    Convenience handling of Rscript arguments modeled after Python's
#'    argparse module. Provides CommandArgsRefClass and ArgumentsRefClass for 
#'    managing arguments. A simple example is
#'    provided below.  See argparse for inspiration... 
#'    \url{http://docs.python.org/dev/library/argparse.html}
#' 
#' 
#' @section Logging:
#' 
#'    A simple logger to allow info/warn/error messaging.
#'    Logged messages are possibly sent to a regular file, but the user can 
#'    opt to echo the messages to the console. Also included is a simple
#'    StopwatchRefClass class for multi-part timing as well as a TimekeeperRefClass which 
#'    will manage one or more stopwatches.  Here is a rough outline of the 'is-as' 
#'    'has-a' relationships among these classes
#'
#'    \itemize{
#'       \item A LoggerRefClass object contains a TimekeeperRefClass object.
#'       \item A TimekeeperRefClass object contains a list of one or more StopwatchRefClass objects referred to by name or index position
#'       \item A StopwatchRefClass contains one or more intervals referred to by 'interval' name or index position
#'    }
#'
#'
#' @section Configurations:
#'
#'    A small set of tools for managing configuration files, formatted like INI files in Windows.  
#'    See \url{http://effbot.org/librarybook/configparser.htm} and 
#'    \url{http://docs.python.org/release/2.6.8/library/configparser.html} for inspiration.
#'
#'
#' @examples
#' \dontrun{   
#' ###
#' #  Using CommandArgs
#' ###
#' 
#'    # define a vector of arguments, usually the value of commandArgs(trailingOnly = FALSE) 
#'    # but in this example we have contrived a variety of space-delimited arguments
#'    
#'    args <- c("/Library/Frameworks/R.framework/Resources/bin/exec/R", "--slave", 
#'       "--no-restore", "--vanilla", 
#'       "--file /Users/Shared/code/R/test-scripts/commandArgs.Rscript", 
#'       "--args", "-bob", "-no-bob" , "zip=", "itty", "doodah")
#'    
#'    # define a function called when the value of an argument is retrieved, you'll 
#'    # see this when we define the 'dog' argument below. Note that the first argument 
#'    # is the ArgumentRefClass object and the the value is the first element of the 
#'    # \code{value} field which may more than one element.
#'    squareDog <- function(x, ...){
#'       x$value[[1]]^2
#'    }
#'    
#'    # create CommandArgs instance using our dummy arguments
#'    X <- CommandArgs(args)
#'    
#'    # define 4 different arguments, two logicals, one with an 'action' 
#'    #  and one with multiple elements 
#'    X$add_argument("bob", type = "set_true", default = FALSE)
#'    X$add_argument("nobob", flag = "--no-bob", type = "set_false", default = TRUE)
#'    X$add_argument("dog", type = "numeric", default = 3, 
#'       required = FALSE, action = 'squareDog')
#'    X$add_argument("zip", type = "character", narg = 2, 
#'       choices = c("itty", "bitty", "doodah", "boom"), default = "boom")
#'    
#'    #run the parser
#'    X$parse_arguments()
#'    
#'    # print the whole shebang
#'    X$show()
#'    
#'    #  get the values of the arguments
#'    X$get("dog")
#'    X$get("bob")
#'    X$get("zip")
#' 
#' ###
#' #  Using Configurator
#' ###
#' 
#'    Here's an example from SCGC's setup tool, 'install-packages.Rscript'
#'    whcih expects a '~/.Rinstallrc' configuration file like the following
#' 
#'    [lib_path]
#'    user=/Users/ben/Library/R/3.0/library
#'    system=/Library/Frameworks/R.framework/Versions/3.0/Resources/library
#'    [download_uri]
#'    ben_dropbox=https://dl.dropboxusercontent.com/u/8433654/packages
#'    [packages]
#'    commandArguments=commandArguments_0.1.tar.gz
#'    configurator=configurator_0.1.tar.gz
#'    ...
#'
#'    X <- Configurator('~/.Rinstallrc')
#'    X$get('download_uri', 'ben_dropbox')
#'    X$set('download_uri', 'bbc', 'http://www.bbc.com/')
#'
#'
#' ###
#' #  Using Logger
#' ###
#'    X <- Logger(name = 'logger-example', file = 'test.log', start_watch = TRUE)
#'    X$warn( "You gotta big problem here!", do_sprintf = FALSE)
#'    X$info("I like %s", "sailing", do_timestamp = FALSE)
#'    X$error("I ate all the %0.2f", pi, do_echo = FALSE)
#'    X$elapsed() 
#' 
#' ###
#' #  Using Stopwatch
#' ###   
#'    # An example with Stopwatch
#'    SW <- Stopwatch(name = 'timex', start = FALSE)
#'    #start it
#'    SW$start()
#'    SW$elapsed()
#'    SW
#'    Sys.sleep(2)
#'    # When stopping you can name the interval for later retreival
#'    SW$stop(interval = 'first')
#'    SW
#'    SW$elapsed(interval = "first")
#'    SW$start()
#'    Sys.sleep(2)
#'    SW$stop(interval = "second")
#'    Sys.sleep(1)
#'    SW$elapsed(interval = 'second')
#'    SW$elapsed(interval = 'first')
#'    # now accumulate the elapsed times
#'    SW$elapsed(accumulate = TRUE)
#'
#' ###
#' #  Using Stopwatch
#' ###      
#'    # An example with 
#'    # create a container class, starting a stopwatch right away
#'    timer <- Timekeeper(start_watch = TRUE, name = 'master')
#'    Sys.sleep(3)
#'    # add another stopwatch
#'    timer$start("two")
#'    timer
#'    timer$stop("two", interval = "another")
#'    timer$elapsed("master")
#' }
NULL