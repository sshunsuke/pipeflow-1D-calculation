# Examples.

source("src/pipeflow1D.R")

# ============================================================
# dimensionlessNumbers.R
# ============================================================

# ------------------------------------------------------------
# Setting for output of detailed log.
# ------------------------------------------------------------
dn$LOG <- TRUE
DEBUG_LOG$LEVEL <- DEBUG_LOG$DEF_LEVEL$TRACE

# ------------------------------------------------------------
# Calculate Reynolds Number.
# ------------------------------------------------------------
dn$Reynolds(1000, 1, 0.2, 0.0017)
dn$Re(1000, 1, 0.2, 0.0017)        # short name
dn$Reynolds(1000)                  # error case

(function(){
  vis <- 1:100 * 0.001
  plot(vis, dn$Re(1000, 1, 0.1, vis), type='l')
})()



# ============================================================
# dimensionlessNumbers.R
# ============================================================
fFrictionFactor$LOG <- TRUE

fFrictionFactor$blasius(10000)
plot(1:100*10000, fFrictionFactor$blasius(1:100*10000)*4 )

DarcyWeisbach(1000, 1, 0.1, 0.0017) 


