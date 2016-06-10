source("debugLog.R")

# Dimensionless number.


dn <- (function() {
  args_msg <- function(fn, ...) {
    args <- c(...)
    
    s <- sprintf("Number of Arguments is not correct.\n  dn$%s()\n", fn)
    for (i in 1:length(c(...))) {
      s <- paste(s, sprintf("    args[%d]:  %s\n", i, args[i]), sep="")
    }
    
    if (dn$LOG == TRUE) {
      WARN(s)
    } else {
      warning(s)
    }
  }

  # Reynolds Number
  Reynolds <- function(density, velocity, length, viscosity, i=0) {
    if (missing(viscosity)) {
      args_msg("Reynolds", "density [kg/m3]", "velocity [m/s]", "length [m]", "viscosity [N-s/m2]")
      return(NA)
    }
    Re <- (density * velocity * length) / viscosity
    if (dn$LOG == TRUE) {
      TRACE("dn$Reynolds()", i=i)
      TRACE("density: %.1f [kg/m3], velocity: %.3f [m/s], length: %.3f [m], viscosity: %f [N-s/m2]",
            density, velocity, length, viscosity, i=i+1)
      TRACE("Re: %.1f", Re, i=i+1)
    }
    Re    
  }
  
  # Prandtl Number
  prandtl <- function(Cp, viscosity, k) {
    if (missing(k)) {
      args_msg("prandtl()", "Cp(specific heat) [J/Kg-K]", "viscosity [N-s/m2]", "k(thermal conductivity) [W/m-K]")
      return(NA)
    }
    (Cp * viscosity) / k
  }
  
  # Nusselt Number (Sieder and Tate)  [Re > 10000]
  nusselt_sieder_tate <- function(Re, Pr) {
    if (missing(Pr)) {
      args_msg("dn$nusselt$sieder_tate()", "Re (Reynolds)", "Pr (Prandtl)")
      return(NA)
    }
    if (Re <= 10000) {
      WARN("This equation is valid, when Re > 10^4")
      return(NA)
    }
    0.027 * Re^0.8 * Pr * (1/3)
  }

  # Nusselt Number (Ramm)  [2300 < Re < 10000]
  nusselt_ramm <- function(Re, Pr) {
    if (missing(Pr)) {
      args_msg("dn$nusselt$ramm()", "Re (Reynolds)", "Pr (Prandtl)")
      return(NA)
    }
    if (Re <= 2300 || Re >= 10000) {
      WARN("This equation is valid, when 2300 < Re < 10000")
      return(NA)
    }
    0.027 * Re^0.8 * Pr * (1/3) * (1 - (6 * 10^5) / Re^1.8)
  }

  # Surface Averaged Nusselt Number for a cylinder in cross flow at various velocities.
  # (Churchill and Bernstein)  [Pr-Re >= 0.2]
  nusselt_churchill_bernstein <- function(Re, Pr) {
    if (missing(Pr)) {
      args_msg("dn$nusselt$churchill_bernstein()",
               "Re (Reynolds number with the cylinder diameter)", "Pr (Prandtl)")
      return(NA)
    }
    if (Re*Pr < 0.2) {
      WARN("This equation is valid, when Re*Pr >= 0.2")
      return(NA)
    }
    0.3  +  (0.62 * Re^(1/2) * Pr^(1/3))  /  (1 + (0.4/Pr)^(2/3))^(1/4)  *  (1 + (Re/282000)^(5/8)) ^ (4/5)
  }

  nusselt <- list(
    sieder_tate = nusselt_sieder_tate,
    ramm = nusselt_ramm,
    churchill_bernstein = nusselt_churchill_bernstein
  )
  
  list(LOG = TRUE,
       Re=Reynolds, Reynolds=Reynolds,
       Pr=prandtl, prandtl=prandtl,
       Nu=nusselt, nusselt=nusselt
  )
})()


