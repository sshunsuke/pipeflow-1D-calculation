# The MIT License (MIT)
# Copyright (c) 2016 Shunsuke S.
# This code is released under the MIT License, 
# see http://opensource.org/licenses/mit-license.php

# A library for debug log. 

DEBUG_LOG <- (function() {
  FILE      <- "log/debug.log"
  DEF_LEVEL <- list(
    FATAL = 1,
    ERROR = 2,
    WARN  = 3,
    INFO  = 4,
    DEBUG = 5,
    TRACE = 6
  )
  LEVEL   <- DEF_LEVEL$INFO
  CONSOLE <- TRUE
  
  indent <- (function(n) {
    s <- "  "
    l <- numeric(n)
    l[1] <- ""
    for (i in 2:n) {
      l[i] <- paste(l[i-1], s, sep="")
    }
    l
  })(10)
  
  list(FILE=FILE, DEF_LEVEL=DEF_LEVEL, LEVEL=LEVEL, CONSOLE=CONSOLE, indent=indent)
})()

FATAL <- function(..., i=0) {
  if (DEBUG_LOG$LEVEL >= DEBUG_LOG$DEF_LEVEL$ERROR) {
    s <- sprintf(...)
    write( paste("FATAL: ", DEBUG_LOG$indent[i+1], s, sep=""), file=DEBUG_LOG$FILE, append=TRUE )
    stop(s)
  }
}

ERROR <- function(..., i=0) {
  if (DEBUG_LOG$LEVEL >= DEBUG_LOG$DEF_LEVEL$ERROR) {
    s <- sprintf(...)
    write( paste("ERROR: ", DEBUG_LOG$indent[i+1], s, sep=""), file=DEBUG_LOG$FILE, append=TRUE )
    stop(s)
  }
}

WARN <- function(..., i=0) {
  if (DEBUG_LOG$LEVEL >= DEBUG_LOG$DEF_LEVEL$WARN) {
    s <- sprintf(...)
    write( paste("WARN : ", DEBUG_LOG$indent[i+1], s, sep=""), file=DEBUG_LOG$FILE, append=TRUE )
    warning(s)
  }
}

INFO <- function(..., i=0, console=TRUE) {
  if (DEBUG_LOG$LEVEL >= DEBUG_LOG$DEF_LEVEL$INFO) {
    s <- sprintf(...)
    if (console && DEBUG_LOG$CONSOLE) { cat(DEBUG_LOG$indent[i+1], s, "\n") }
    write( paste("INFO : ", DEBUG_LOG$indent[i+1], s, sep=""), file=DEBUG_LOG$FILE, append=TRUE )
  }
}

DEBUG <- function(..., i=0, console=TRUE) {
  if (DEBUG_LOG$LEVEL >= DEBUG_LOG$DEF_LEVEL$DEBUG) {
    s <- sprintf(...)
    if (console && DEBUG_LOG$CONSOLE) { cat(DEBUG_LOG$indent[i+1], s, "\n") }
    write( paste("DEBUG: ", DEBUG_LOG$indent[i+1], s, sep=""), file=DEBUG_LOG$FILE, append=TRUE )
  }
}

TRACE <- function(..., i=0, console=TRUE) {
  if (DEBUG_LOG$LEVEL >= DEBUG_LOG$DEF_LEVEL$TRACE) {
    s <- sprintf(...)
    if (console && DEBUG_LOG$CONSOLE) { cat(DEBUG_LOG$indent[i+1], s, "\n") }
    write( paste("TRACE: ", DEBUG_LOG$indent[i+1], s, sep=""), file=DEBUG_LOG$FILE, append=TRUE )
  }
}

CAT <- function(..., i=0) {
  cat(sprintf(...), "\n")
}

parseList <- function(l, br=3, indent="  ") {
  s <- indent
  counter <- 0
  n <- names(l)
  for (k in n) {
    v <- l[[k]]
    if (counter < br) {
      s <- paste(s, sprintf("%s=%s, ", k, v), sep="")
      counter <- counter + 1
    } else {
      s <- paste(s, "\n", indent, sprintf("%s=%s, ", k, v), sep="")
      counter <- 0
    }
  }
  paste(s, "\n", sep="")
}

