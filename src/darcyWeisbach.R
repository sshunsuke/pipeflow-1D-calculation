source("src/debugLog.R")
source("src/dimensionlessNumbers.R")

# =============================================================================
# Fanning Friction Factor
#   Fanning friction factor is one-fourth of the Darcy friction factor.
# =============================================================================
fFrictionFactor <- (function(){
  
  # Fanning Friction Factor.
  frictionFactor.laminar <- function(Re) {
    16 / Re
  }
  
  # Fanning Friction Factor
  # This equation is valid only to smooth pipes. (Blasius)
  frictionFactor.blasius <- function(Re, C=0.0791, i=0) {
    C / (Re ^ 0.25)
  }
  
  # Fanning Friction Factor of Turbulent Flow.  (Colebrook)
  frictionFactor.colebrook <- function(roughness, D, Re, interval=c(0, 5), warn=TRUE, i=0) {
    if (fFrictionFactor$LOG) {
      dl$TRACE("START: fFrictionFactor$colebrook_()", i=i)
      dl$TRACE("roughness=%f, D=%f, Re=%f", roughness, D, Re, i=i+1)
    }
    
    if (Re <= 4000 && warn==TRUE) {
      if (fFrictionFactor$LOG) {
        WARN("Re <= 4000 !", i=i+1)
      } else {
        warning("Re <= 4000 !")
      }
    }
    
    fun <- function(f) {
      (1 / sqrt(f)) + 4 * log10( roughness / D / 3.71 + 1.256 / Re / sqrt(f))
    }
    f <- uniroot(fun, interval)  # newton.method
    
    if (fFrictionFactor$LOG) {
      dl$TRACE("Return: %f  [$f.root=%f, $iter=%d, $estim.prec=%f]",
               f$root, f$f.root, f$iter, f$estim.prec, i=i+1)
      dl$TRACE("END: fFrictionFactor$colebrook_()", i=i)
    }
    
    f$root
  }
  
  list(
    LOG = FALSE,
    laminar = frictionFactor.laminar,
    blasius = frictionFactor.blasius,
    colebrook = function(roughness, D, Re, interval=c(0, 5), warn=TRUE, i=0) {
      f <- function(roughness_, D_, Re_){
        fFrictionFactor$colebrook_(roughness_, D_, Re_, interval=interval, warn=warn, i=i)
      }
      mapply(f, roughness, D, Re)
    },
    colebrook_ = frictionFactor.colebrook
  )
})()


DarcyWeisbach <- function(density, velocity, D, viscosity, roughness, i=0) {
  if (missing(roughness)) {
    roughness <- 0
  }
  Re <- dn$Reynolds(density, velocity, D, viscosity)
  
  dl$DEBUG("START: DarcyWeisbach()", i=i)
  dl$DEBUG("roughness=%f, D=%f, Re=%f", roughness, D, Re, i=i+1)
  
  ff <- fFrictionFactor$laminar(Re)
  if (Re > 4000) {
    ff <- fFrictionFactor$blasius(Re)
  } else if (Re > 2300) {
    f2300 <- fFrictionFactor.laminar(2300)
    f4000 <- fFrictionFactor.colebrook(roughness, D, 4000, warn=FALSE)
    ff <- (f4000 - f2300) / (4000 - 2300) * (Re - 2300) + f2300
  }
  
  fd <- fanning2darcy(ff)
  
  dP_dL <- fd * density * (velocity ^ 2) / (2 * D)
  
  dP_dL
}





shearStress <- function(f, v, density) {
  f * density * v * abs(v) / 2
}


