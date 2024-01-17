par(mfrow = c(2, 4))
# Plot 1

hist(LCWangForwardCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="LC")
hist(RHWangForwardCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="RH")
hist(CBDWangForwardCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="CBD")
hist(M6WangForwardCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="M6")

hist(LCWangSwapCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="LC")
hist(RHWangSwapCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="RH")
hist(CBDWangSwapCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="CBD")
hist(M6WangSwapCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="M6")

title("S-forward Valuation Under Wang Transform", line = -1, outer = TRUE)
title("S-swap Valuation Under Wang Transform", line = -30, outer = TRUE)

# Plot 2
hist(LCPropForwardCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="LC")
hist(RHPropForwardCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="RH")
hist(CBDPropForwardCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="CBD")
hist(M6PropForwardCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="M6")

hist(LCPropSwapCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="LC")
hist(RHPropSwapCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="RH")
hist(CBDPropSwapCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="CBD")
hist(M6PropSwapCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="M6")

title("S-forward Valuation Under Prop Transform", line = -1, outer = TRUE)
title("S-swap Valuation Under Prop Transform", line = -30, outer = TRUE)


# Plot 3
hist(LCDualForwardCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="LC")
hist(RHDualForwardCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="RH")
hist(CBDDualForwardCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="CBD")
hist(M6DualForwardCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="M6")

hist(LCDualSwapCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="LC")
hist(RHDualSwapCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="RH")
hist(CBDDualSwapCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="CBD")
hist(M6DualSwapCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="M6")

title("S-forward Valuation Under Dual Transform", line = -1, outer = TRUE)
title("S-swap Valuation Under Dual Transform", line = -30, outer = TRUE)

# Plot 4
hist(LCGiniForwardCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="LC")
hist(RHGiniForwardCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="RH")
hist(CBDGiniForwardCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="CBD")
hist(M6GiniForwardCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="M6")

hist(LCGiniSwapCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="LC")
hist(RHGiniSwapCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="RH")
hist(CBDGiniSwapCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="CBD")
hist(M6GiniSwapCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="M6")

title("S-forward Valuation Under Gini Transform", line = -1, outer = TRUE)
title("S-swap Valuation Under Gini Transform", line = -30, outer = TRUE)

# Plot 5
hist(LCExponentialForwardCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="LC")
hist(RHExponentialForwardCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="RH")
hist(CBDExponentialForwardCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="CBD")
hist(M6ExponentialForwardCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="M6")

hist(LCExponentialSwapCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="LC")
hist(RHExponentialSwapCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="RH")
hist(CBDExponentialSwapCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="CBD")
hist(M6ExponentialSwapCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="M6")

title("S-forward Valuation Under Exponential Transform", line = -1, outer = TRUE)
title("S-swap Valuation Under Exponential Transform", line = -30, outer = TRUE)

# Plot 6
hist(LCStDevForwardCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="LC")
hist(RHStDevForwardCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="RH")
hist(CBDStDevForwardCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="CBD")
hist(M6StDevForwardCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="M6")

hist(LCStDevSwapCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="LC")
hist(RHStDevSwapCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="RH")
hist(CBDStDevSwapCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="CBD")
hist(M6StDevSwapCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="M6")

title("S-forward Valuation Under Std. Dev.", line = -1, outer = TRUE)
title("S-swap Valuation Under Std. Dev.", line = -30, outer = TRUE)

# Plot 7
hist(LCVarForwardCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="LC")
hist(RHVarForwardCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="RH")
hist(CBDVarForwardCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="CBD")
hist(M6VarForwardCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="M6")

hist(LCVarSwapCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="LC")
hist(RHVarSwapCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="RH")
hist(CBDVarSwapCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="CBD")
hist(M6VarSwapCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="M6")

title("S-forward Valuation Under Var Transform", line = -1, outer = TRUE)
title("S-swap Valuation Under Var Transform", line = -30, outer = TRUE)

# Plot 8
hist(LCMadForwardCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="LC")
hist(RHMadForwardCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="RH")
hist(CBDMadForwardCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="CBD")
hist(M6MadForwardCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="M6")

hist(LCMadSwapCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="LC")
hist(RHMadSwapCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="RH")
hist(CBDMadSwapCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="CBD")
hist(M6MadSwapCDF, xlab="Valuation", xlim=c(-0.1, 0.5), ylim=c(0,1200), main="M6")

title("S-forward Valuation Under Mad Transform", line = -1, outer = TRUE)
title("S-swap Valuation Under Mad Transform", line = -30, outer = TRUE)

