# The MIT License (MIT)
# Copyright (c) 2016 Shunsuke S.
# This code is released under the MIT License, 
# see http://opensource.org/licenses/mit-license.php

# A library for debug log. 

DEBUG_LOG <- (function() {
  FILE        <- "log/debug.log"
  LEVEL_ERROR <- 2
  LEVEL_WARN  <- 3
  LEVEL_INFO  <- 4
  LEVEL_DEBUG <- 5
  LEVEL_TRACE <- 6
  LEVEL       <- LEVEL_DEBUG
  CONSOLE <- TRUE
  
  INDENT <- (function(n) {
    s <- "  "
    l <- numeric(n)
    l[1] <- ""
    for (i in 2:n) {
      l[i] <- paste(l[i-1], s, sep="")
    }
    l
  })(10)
  
  OUTPUT <- function(level, indent, msg, console=TRUE) {
    s <- paste(indent, msg, sep="")
    if (console && DEBUG_LOG$CONSOLE) { cat(s, "\n") }
    write( paste(level, s, sep=" "), file=DEBUG_LOG$FILE, append=TRUE )
  }
  
  list(FILE=FILE, LEVEL_ERROR=LEVEL_ERROR, LEVEL_WARN=LEVEL_WARN, LEVEL_INFO=LEVEL_INFO,
       LEVEL_DEBUG=LEVEL_DEBUG, LEVEL_TRACE=LEVEL_TRACE, LEVEL=LEVEL, CONSOLE=CONSOLE,
       INDENT=INDENT, OUTPUT=OUTPUT)
})()


TRACE <- function(..., i=0) {
  if (DEBUG_LOG$LEVEL >= DEBUG_LOG$LEVEL_TRACE) {
    DEBUG_LOG$OUTPUT("TRACE:", DEBUG_LOG$INDENT[i+1], sprintf(...))
  }
}

DEBUG <- function(..., i=0) {
  if (DEBUG_LOG$LEVEL >= DEBUG_LOG$LEVEL_DEBUG) {
    DEBUG_LOG$OUTPUT("DEBUG:", DEBUG_LOG$INDENT[i+1], sprintf(...))
  }
}

INFO <- function(..., i=0) {
  if (DEBUG_LOG$LEVEL >= DEBUG_LOG$LEVEL_INFO) {
    DEBUG_LOG$OUTPUT("INFO :", DEBUG_LOG$INDENT[i+1], sprintf(...))
  }
}

WARN <- function(..., i=0) {
  if (DEBUG_LOG$LEVEL >= DEBUG_LOG$LEVEL_WARN) {
    DEBUG_LOG$OUTPUT("WARN :", DEBUG_LOG$INDENT[i+1], sprintf(...))
    warning(sprintf(...))
  }
}

ERROR <- function(..., i=0) {
  if (DEBUG_LOG$LEVEL >= DEBUG_LOG$LEVEL_WARN) {
    DEBUG_LOG$OUTPUT("ERROR :", DEBUG_LOG$INDENT[i+1], sprintf(...))
    warning(sprintf(...))
  }
}

CAT <- function(..., i=0) {
  if (DEBUG_LOG$LEVEL >= DEBUG_LOG$LEVEL_INFO) {
    INFO(..., i=i)
  } else if (DEBUG_LOG$CONSOLE) {
    cat(paste(DEBUG_LOG$INDENT[i+1], sprintf(...), sep=""), "\n")
  }
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


