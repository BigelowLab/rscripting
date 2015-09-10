##rscripting
 
An R package for simple argument parsing, logging, configuration files, email, and file/directory archiving.

### Installation

It's fairly easy to install using Hadley Wickham's [devtools](http://cran.r-project.org/web/packages/devtools/index.html).

```r
library(devtools)
install_github('btupper/rscripting')
```

### Logging

Provides a `LoggerRefClass` reference class for logging messages to a file and/or the console.  See the [wiki](https://github.com/btupper/rscripting/wiki/Logging) for details.  If you need a more robust logger please see [futile.logger](https://github.com/zatonovo/futile.logger).

### Argument parsing

Provides a `CommandArgsRefClass` reference class for parsing R and Rscript arguments. See the [wiki](https://github.com/btupper/rscripting/wiki/Argument-parsing) for details.  If you prefer a (beautiful) stand alone argument parsing package  please see [argparser](https://bitbucket.org/djhshih/argparser/overview).

### Configurations (INI-style file formatted data)

Provides a `ConfiguratorRefClass` reference class for simple [INI-style](https://en.wikipedia.org/wiki/INI_file) formatted files.  See the [wiki](https://github.com/btupper/rscripting/wiki/Configurations) for details.

### Email

Provides wrapper functions to send mail using [nail](http://nail.sourceforge.net/) or [mutt](http://www.mutt.org/)

### File/directory archiving

Provides a function to `pack_directory` as tar.gz or .zip to compress a directory.  Also provides `unpack_archive` to unpack a compressed file and return a listing of it's contents.