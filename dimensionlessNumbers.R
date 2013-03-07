
dn <- (function() {
  args_msg <- function(...) {
    args <- c(...)
    s <- ""
    for (i in 1:length(c(...))) {
      s <- paste(s, sprintf("  args[%d]:  %s\n", i, args[i]), sep="")
    }
    s
  }


  # Reynolds Number
  reynolds <- function(density, velocity, length, viscosity) {
    if (missing(viscosity)) {
      message("Number of Arguments is not correct.")
      message(args_msg("density [kg/m3]", "velocity [m/s]",  "length [m]", "viscosity [N-s/m2]"))
      return(NA)
    }
    (density * velocity * length) / viscosity
  }
  
  # Prandtl Number
  prandtl <- function(Cp, viscosity, k) {
    if (missing(k)) {
      message("Number of Arguments is not correct.")
      message(args_msg("Cp(specific heat) [J/Kg-K]", "viscosity [N-s/m2]", "k(thermal conductivity) [W/m-K]"))
      return(NA)
    }
    (Cp * viscosity) / k
  }
  
  # Nusselt Number (Sieder and Tate)  [Re > 10000]
  nusselt_sieder_tate <- function(Re, Pr) {
    if (missing(Pr)) {
      message("Number of Arguments is not correct.")
      message(args_msg("Re (Reynolds)", "Pr (Prandtl)"))
      return(NA)
    }
    if (Re <= 10000) {
      warning("This equation should be used, when Re > 10^4")
      return(NA)
    }
    0.027 * Re^0.8 * Pr * (1/3)
  }

  # Nusselt Number (Ramm)  [2300 < Re < 10000]
  nusselt_ramm <- function(Re, Pr) {
    if (missing(Pr)) {
      message("Number of Arguments is not correct.")
      message(args_msg("Re (Reynolds)", "Pr (Prandtl)"))
      return(NA)
    }
    if (Re <= 2300 || Re >= 10000) {
      warning("This equation should be used, when 2300 < Re < 10000")
      return(NA)
    }
    0.027 * Re^0.8 * Pr * (1/3) * (1 - (6 * 10^5) / Re^1.8)
  }


  # Surface Averaged Nusselt Number for a cylinder in cross flow at various velocities.
  # (Churchill and Bernstein)  [Pr-Re >= 0.2]
  nusselt_churchill_bernstein <- function(Re, Pr) {
    if (missing(Pr)) {
      message("Number of Arguments is not correct.")
      message(args_msg("Re (Reynolds number with the cylinder diameter)", "Pr (Prandtl)"))
      return(NA)
    }
    if (Re*Pr < 0.2) {
      warning("This equation should be used, when Re*Pr >= 0.2")
      return(NA)
    }
    0.3  +  (0.62 * Re^(1/2) * Pr^(1/3))  /  (1 + (0.4/Pr)^(2/3))^(1/4)  *  (1 + (Re/282000)^(5/8)) ^ (4/5)
  }


  list(Re = reynolds,
       Pr = prandtl,
       Nu_sieder_tate = nusselt_sieder_tate,
       Nu_ramm = nusselt_ramm,
       Nu_churchill_bernstein = nusselt_churchill_bernstein
      )
})()

