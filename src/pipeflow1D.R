source("src/debugLog.R")

source("src/unitConverter.R")
source("src/dimensionlessNumbers.R")
source("src/darcyWeisbach.R")

# attach( dl )

dn$LOG <- TRUE



pf1d <- list()

pf1d$cp <- function() {
  
  circularPipeArea <- function(do, di) {
    if (do <= di) {
      warning(sprintf("OD is less than or equal ID. OD=%f, ID=%f", do, di))
    }
    (do^2 - di^2) * pi / 4
  }
  
  circularPipeDe <- function(do, di) {
    if (do <= di) {
      warning(sprintf("OD is less than or equal ID. OD=%f, ID=%f", do, di))
    }
    area <- circularPipeArea(do, di)
    Wp   <- (do + di) * pi
    4 * area / Wp
  }
  annulus.properties <- function(Do, Di) {
    De <- Do - Di
    Ao <- (Do / 2)^2 * pi
    Ai <- (Di / 2)^2 * pi
    Aa <- circularPipeArea(Do, Di)
    So <- Do * pi
    Si <- Di * pi
    list(De=De, Ao=Ao, Ai=Ai, Aa=Aa, So=So, Si=Si)
  }
  
  list(area=circularPipeArea, De=circularPipeDe, annulus.properties=annulus.properties)
}


hydraulicDiameter <- function(A, S) {
  4 * A / S
}


