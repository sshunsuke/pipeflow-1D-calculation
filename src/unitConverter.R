# =============================================================================
# Functions for unit conversion.
# =============================================================================

if (exists('inch2m') == TRUE) {
  detach( unitConverter )
}

unitConverter <- (function(){
  
  l <- list(
    # Length
    inch2m = function(inch) { inch * 0.0254 },
    m2inch = function(m) { m / 0.0254 },
    ft2m   = function(ft) { ft * 0.3048 },
    m2ft   = function(m) { m / 0.3048 },
    
    # Temperature
    K2C = function(K) { K - 273.15 },
    C2K = function(C) { C + 273.15 },
    F2C = function(F) { (F - 32) * 5 / 9 },
    
    # Pressure
    psi2Pa = function(psi) { psi * 6894.76 }
  )
  
  return(l)
})()

if (exists('inch2m') == FALSE) {
  attach( unitConverter )
}


