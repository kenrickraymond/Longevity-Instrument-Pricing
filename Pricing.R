# Reduce chances of scientific notation
options(digits=4, scipen=999)
# Generic libraries used
library(demography)
library(StMoMo)
library(lifecontingencies)
library(stats)

# Notation:
# Years_for is the number of years the mortality model is forecasted. Years_for is also the longest time horizon for which we are pricing instruments.

################################################################################################################################
########################################################## Preparation #########################################################
###############################################################################################################################
# The Data
EWMaleData

# Converting central exposure to initial exposure for logit models
EWMaleIniData = central2initial(EWMaleData)

# Restrict the Data
ages.fit = 60:89
years.fit = seq(1961,2011,1)
wxt = genWeightMat(ages = ages.fit, years = years.fit) # Matrix of age weights. See StMoMo vignette page 16 (https://cran.r-project.org/web/packages/StMoMo/vignettes/StMoMoVignette.pdf)

# InitialiZe mortality models
LC = lc(link = "log")
RH = rh(link = "log", cohortAgeFun = "1") # cohortAgeFun = "NP" sets the coefficient of the cohort term to be a variable not equal to 1
CBD = cbd(link="logit") # Logit link refers to q_{x,t}/{1 - q_{x,t}}
M6 = m6(link="logit")

# Fit Mortality Models
LCfit = fit(LC, data=EWMaleData, ages.fit=ages.fit, years.fit=years.fit, wxt = wxt)
RHfit = fit(RH, data=EWMaleData, ages.fit=ages.fit, years.fit=years.fit, wxt = wxt)
CBDfit = fit(CBD, data = EWMaleIniData, ages.fit = ages.fit, years.fit=years.fit, wxt = wxt)
M6fit = fit(M6, data = EWMaleIniData, ages.fit = ages.fit, years.fit=years.fit, wxt = wxt)

# Goodness of Fit
goodness_of_fit_table = data.frame(matrix(nrow = 4, ncol = 2, c(AIC(LCfit),
                                               AIC(RHfit),
                                               BIC(LCfit),
                                               BIC(RHfit),
                                               AIC(CBDfit),
                                               AIC(M6fit),
                                               BIC(CBDfit),
                                               BIC(M6fit))
))
colnames(goodness_of_fit_table) = c("AIC","BIC")
rownames(goodness_of_fit_table) = c("LC","RH","CBD","M6")
goodness_of_fit_table

# Plot fitted models
# plot(LCfit)
# plot(RHfit)
# plot(CBDfit)
# plot(M6fit)

# Residual analysis
LCres <- residuals(LCfit)
RHres <- residuals(RHfit)
CBDres <- residuals(CBDfit)
M6res <- residuals(M6fit)

plot(LCres, type = "colourmap", reslim = c(-3.5, 3.5), main="LC")
plot(RHres, type = "colourmap", reslim = c(-3.5, 3.5), main="RH")
plot(CBDres, type = "colourmap", reslim = c(-3.5, 3.5), main="CBD")
plot(M6res, type = "colourmap", reslim = c(-3.5, 3.5), main="M6")

# Parameters for forecasting and simulations
years_for = 20 # Generate h-year ahead forecasts
nsim = 1000 # Number of simulations when simulating future mortality scenarios

################################################################################################################################
#################################################### Pricing Parameter #########################################################
################################################################################################################################

# Parameters for obtaining the pricing parameter
# We obtain the pricing parameter using annuity rates for the reference population since market prices for longevity-linked instruments are not publically available
payment = 6000 # 2012 Rates for UK Population
total = 100000
interest_rate = 0.0204 # Based on the 15-year Gilt rate
discount_factor = exp(- interest_rate)

source("getLambda.R")
LCWanglambda = getLambda(1, "LC", "Wang") # Initial estimation is required because of root-finding solution
LCProplambda = getLambda(1.5, "LC", "Proportional")
LCDuallambda = getLambda(1.3, "LC", "Dual")
LCGinilambda = getLambda(0.5, "LC", "Gini")
LCExponentiallambda = getLambda(1, "LC", "Exponential")
LCStdevlambda = getLambda(0, "LC", "Stdev") # No initial estimation is necessary, closed form expressions for lambda are derived for real world measures
LCVarlambda = getLambda(0, "LC", "Var")
LCMadlambda = getLambda(0, "LC", "Mad")

RHWanglambda = getLambda(1, "RH", "Wang")
RHProplambda = getLambda(1.5, "RH", "Proportional")
RHDuallambda = getLambda(1.3, "RH", "Dual")
RHGinilambda = getLambda(0.5, "RH", "Gini")
RHExponentiallambda = getLambda(1, "RH", "Exponential")
RHStdevlambda = getLambda(0, "RH", "Stdev")
RHVarlambda = getLambda(0, "RH", "Var")
RHMadlambda = getLambda(0, "RH", "Mad")

CBDWanglambda = getLambda(1, "CBD", "Wang")
CBDProplambda = getLambda(1.5, "CBD", "Proportional")
CBDDuallambda = getLambda(1.3, "CBD", "Dual")
CBDGinilambda = getLambda(0.5, "CBD", "Gini")
CBDExponentiallambda = getLambda(1, "CBD", "Exponential")
CBDStdevlambda = getLambda(0, "CBD", "Stdev")
CBDVarlambda = getLambda(0, "CBD", "Var")
CBDMadlambda = getLambda(0, "CBD", "Mad")

M6Wanglambda = getLambda(1, "M6", "Wang")
M6Proplambda = getLambda(1.5, "M6", "Proportional")
M6Duallambda = getLambda(1.3, "M6", "Dual")
M6Ginilambda = getLambda(0.5, "M6", "Gini")
M6Exponentiallambda = getLambda(1, "M6", "Exponential")
M6Stdevlambda = getLambda(0, "M6", "Stdev")
M6Varlambda = getLambda(0, "M6", "Var")
M6Madlambda = getLambda(0, "M6", "Mad")

lambda_table = data.frame(matrix(nrow = 8, ncol = 4, c(LCWanglambda, LCProplambda, LCDuallambda, LCGinilambda, LCExponentiallambda, LCStdevlambda, LCVarlambda, LCMadlambda,
                                                          RHWanglambda, RHProplambda, RHDuallambda, RHGinilambda, RHExponentiallambda, RHStdevlambda, RHVarlambda, RHMadlambda,
                                                          CBDWanglambda, CBDProplambda, CBDDuallambda, CBDGinilambda, CBDExponentiallambda, CBDStdevlambda, CBDVarlambda, CBDMadlambda,
                                                          M6Wanglambda, M6Proplambda, M6Duallambda, M6Ginilambda, M6Exponentiallambda, M6Stdevlambda, M6Varlambda, M6Madlambda)
))
rownames(lambda_table) = c("Wang", "Proportional", "Dual", "Gini", "Exponential", "StDev", "Var", "Mad")
colnames(lambda_table) = c("LC","RH","CBD","M6")
lambda_table

################################################################################################################################
#################################################### Adjustment Parameter ######################################################
################################################################################################################################
source("SurvivorForward.R")
LC_Wang_Forward_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPremium(years_for, LCWanglambda, "LC", "Wang", nsim=nsim) ) )
LC_Prop_Forward_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPremium(years_for, LCProplambda, "LC", "Proportional", nsim=nsim) ) )
LC_Dual_Forward_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPremium(years_for, LCDuallambda, "LC", "Dual", nsim=nsim) ) )
LC_Gini_Forward_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPremium(years_for, LCGinilambda, "LC", "Gini", nsim=nsim) ) )
LC_Exponential_Forward_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPremium(years_for, LCExponentiallambda, "LC", "Exponential", nsim=nsim) ) )
LC_Std_Forward_Premium = as.numeric(lapply(1:years_for, function(years_for) survivorForwardPremium(years_for, LCStdevlambda, "LC", "Stdev", nsim=nsim) ) )
LC_Var_Forward_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPremium(years_for, LCVarlambda, "LC", "Var", nsim=nsim) ) )
LC_Mad_Forward_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPremium(years_for, LCMadlambda, "LC", "Mad", nsim=nsim) ) )

RH_Wang_Forward_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPremium(years_for, RHWanglambda, "RH", "Wang", nsim=nsim) ) )
RH_Prop_Forward_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPremium(years_for, RHProplambda, "RH", "Proportional", nsim=nsim) ) )
RH_Dual_Forward_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPremium(years_for, RHDuallambda, "RH", "Dual", nsim=nsim) ) )
RH_Gini_Forward_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPremium(years_for, RHGinilambda, "RH", "Gini", nsim=nsim) ) )
RH_Exponential_Forward_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPremium(years_for, RHExponentiallambda, "RH", "Exponential", nsim=nsim) ) )
RH_Std_Forward_Premium = as.numeric(lapply(1:years_for, function(years_for) survivorForwardPremium(years_for, RHStdevlambda, "RH", "Stdev", nsim=nsim) ) )
RH_Var_Forward_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPremium(years_for, RHVarlambda, "RH", "Var", nsim=nsim) ) )
RH_Mad_Forward_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPremium(years_for, RHMadlambda, "RH", "Mad", nsim=nsim) ) )

CBD_Wang_Forward_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPremium(years_for, CBDWanglambda, "CBD", "Wang", nsim=nsim) ) )
CBD_Prop_Forward_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPremium(years_for, CBDProplambda, "CBD", "Proportional", nsim=nsim) ) )
CBD_Dual_Forward_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPremium(years_for, CBDDuallambda, "CBD", "Dual", nsim=nsim) ) )
CBD_Gini_Forward_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPremium(years_for, CBDGinilambda, "CBD", "Gini", nsim=nsim) ) )
CBD_Exponential_Forward_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPremium(years_for, CBDExponentiallambda, "CBD", "Exponential", nsim=nsim) ) )
CBD_Std_Forward_Premium = as.numeric(lapply(1:years_for, function(years_for) survivorForwardPremium(years_for, CBDStdevlambda, "CBD", "Stdev", nsim=nsim) ) )
CBD_Var_Forward_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPremium(years_for, CBDVarlambda, "CBD", "Var", nsim=nsim) ) )
CBD_Mad_Forward_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPremium(years_for, CBDMadlambda, "CBD", "Mad", nsim=nsim) ) )

M6_Wang_Forward_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPremium(years_for, M6Wanglambda, "M6", "Wang", nsim=nsim) ) )
M6_Prop_Forward_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPremium(years_for, M6Proplambda, "M6", "Proportional", nsim=nsim) ) )
M6_Dual_Forward_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPremium(years_for, M6Duallambda, "M6", "Dual", nsim=nsim) ) )
M6_Gini_Forward_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPremium(years_for, M6Ginilambda, "M6", "Gini", nsim=nsim) ) )
M6_Exponential_Forward_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPremium(years_for, M6Exponentiallambda, "M6", "Exponential", nsim=nsim) ) )
M6_Std_Forward_Premium = as.numeric(lapply(1:years_for, function(years_for) survivorForwardPremium(years_for, M6Stdevlambda, "M6", "Stdev", nsim=nsim) ) )
M6_Var_Forward_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPremium(years_for, M6Varlambda, "M6", "Var", nsim=nsim) ) )
M6_Mad_Forward_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPremium(years_for, M6Madlambda, "M6", "Mad", nsim=nsim) ) )


source("SurvivorSwap.R")
LC_Wang_Swap_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorSwapPremium(years_for, LCWanglambda, "LC", "Wang", nsim=nsim) ) )
LC_Prop_Swap_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorSwapPremium(years_for, LCProplambda, "LC", "Proportional", nsim=nsim) ) )
LC_Dual_Swap_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorSwapPremium(years_for, LCDuallambda, "LC", "Dual", nsim=nsim) ) )
LC_Gini_Swap_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorSwapPremium(years_for, LCGinilambda, "LC", "Gini", nsim=nsim) ) )
LC_Exponential_Swap_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorSwapPremium(years_for, LCExponentiallambda, "LC", "Exponential", nsim=nsim) ) )
LC_Std_Swap_Premium = as.numeric(lapply(1:years_for, function(years_for) survivorSwapPremium(years_for, LCStdevlambda, "LC", "Stdev", nsim=nsim) ) )
LC_Var_Swap_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorSwapPremium(years_for, LCVarlambda, "LC", "Var", nsim=nsim) ) )
LC_Mad_Swap_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorSwapPremium(years_for, LCMadlambda, "LC", "Mad", nsim=nsim) ) )

RH_Wang_Swap_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorSwapPremium(years_for, RHWanglambda, "RH", "Wang", nsim=nsim) ) )
RH_Prop_Swap_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorSwapPremium(years_for, RHProplambda, "RH", "Proportional", nsim=nsim) ) )
RH_Dual_Swap_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorSwapPremium(years_for, RHDuallambda, "RH", "Dual", nsim=nsim) ) )
RH_Gini_Swap_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorSwapPremium(years_for, RHGinilambda, "RH", "Gini", nsim=nsim) ) )
RH_Exponential_Swap_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorSwapPremium(years_for, RHExponentiallambda, "RH", "Exponential", nsim=nsim) ) )
RH_Std_Swap_Premium = as.numeric(lapply(1:years_for, function(years_for) survivorSwapPremium(years_for, RHStdevlambda, "RH", "Stdev", nsim=nsim) ) )
RH_Var_Swap_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorSwapPremium(years_for, RHVarlambda, "RH", "Var", nsim=nsim) ) )
RH_Mad_Swap_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorSwapPremium(years_for, RHMadlambda, "RH", "Mad", nsim=nsim) ) )

CBD_Wang_Swap_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorSwapPremium(years_for, CBDWanglambda, "CBD", "Wang", nsim=nsim) ) )
CBD_Prop_Swap_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorSwapPremium(years_for, CBDProplambda, "CBD", "Proportional", nsim=nsim) ) )
CBD_Dual_Swap_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorSwapPremium(years_for, CBDDuallambda, "CBD", "Dual", nsim=nsim) ) )
CBD_Gini_Swap_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorSwapPremium(years_for, CBDGinilambda, "CBD", "Gini", nsim=nsim) ) )
CBD_Exponential_Swap_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorSwapPremium(years_for, CBDExponentiallambda, "CBD", "Exponential", nsim=nsim) ) )
CBD_Std_Swap_Premium = as.numeric(lapply(1:years_for, function(years_for) survivorSwapPremium(years_for, CBDStdevlambda, "CBD", "Stdev", nsim=nsim) ) )
CBD_Var_Swap_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorSwapPremium(years_for, CBDVarlambda, "CBD", "Var", nsim=nsim) ) )
CBD_Mad_Swap_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorSwapPremium(years_for, CBDMadlambda, "CBD", "Mad", nsim=nsim) ) )

M6_Wang_Swap_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorSwapPremium(years_for, M6Wanglambda, "M6", "Wang", nsim=nsim) ) )
M6_Prop_Swap_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorSwapPremium(years_for, M6Proplambda, "M6", "Proportional", nsim=nsim) ) )
M6_Dual_Swap_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorSwapPremium(years_for, M6Duallambda, "M6", "Dual", nsim=nsim) ) )
M6_Gini_Swap_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorSwapPremium(years_for, M6Ginilambda, "M6", "Gini", nsim=nsim) ) )
M6_Exponential_Swap_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorSwapPremium(years_for, M6Exponentiallambda, "M6", "Exponential", nsim=nsim) ) )
M6_Std_Swap_Premium = as.numeric(lapply(1:years_for, function(years_for) survivorSwapPremium(years_for, M6Stdevlambda, "M6", "Stdev", nsim=nsim) ) )
M6_Var_Swap_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorSwapPremium(years_for, M6Varlambda, "M6", "Var", nsim=nsim) ) )
M6_Mad_Swap_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorSwapPremium(years_for, M6Madlambda, "M6", "Mad", nsim=nsim) ) )


source("Robustness.R")
forward_years=15
maturity_length=10

LCWangForwardCDF = forwardCDF(forward_years,maturity_length, LCWanglambda, "LC", "Wang", LC_Wang_Forward_Premium[forward_years], nsim=1000)
RHWangForwardCDF = forwardCDF(forward_years,maturity_length, RHWanglambda, "RH", "Wang", RH_Wang_Forward_Premium[forward_years], nsim=1000)
CBDWangForwardCDF = forwardCDF(forward_years,maturity_length, CBDWanglambda, "CBD", "Wang", CBD_Wang_Forward_Premium[forward_years], nsim=1000)
M6WangForwardCDF = forwardCDF(forward_years,maturity_length, M6Wanglambda, "M6", "Wang", M6_Wang_Forward_Premium[forward_years], nsim=1000)

LCPropForwardCDF = forwardCDF(forward_years,maturity_length, LCProplambda, "LC", "Proportional", LC_Prop_Forward_Premium[forward_years], nsim=1000)
RHPropForwardCDF = forwardCDF(forward_years,maturity_length, RHProplambda, "RH", "Proportional", RH_Prop_Forward_Premium[forward_years], nsim=1000)
CBDPropForwardCDF = forwardCDF(forward_years,maturity_length, CBDProplambda, "CBD", "Proportional", CBD_Prop_Forward_Premium[forward_years], nsim=1000)
M6PropForwardCDF = forwardCDF(forward_years,maturity_length, M6Proplambda, "M6", "Proportional", M6_Prop_Forward_Premium[forward_years], nsim=1000)

LCDualForwardCDF = forwardCDF(forward_years,maturity_length, LCDuallambda, "LC", "Dual", LC_Dual_Forward_Premium[forward_years], nsim=1000)
RHDualForwardCDF = forwardCDF(forward_years,maturity_length, RHDuallambda, "RH", "Dual", RH_Dual_Forward_Premium[forward_years], nsim=1000)
CBDDualForwardCDF = forwardCDF(forward_years,maturity_length, CBDDuallambda, "CBD", "Dual", CBD_Dual_Forward_Premium[forward_years], nsim=1000)
M6DualForwardCDF = forwardCDF(forward_years,maturity_length, M6Duallambda, "M6", "Dual", M6_Dual_Forward_Premium[forward_years], nsim=1000)

LCGiniForwardCDF = forwardCDF(forward_years,maturity_length, LCGinilambda, "LC", "Gini", LC_Gini_Forward_Premium[forward_years], nsim=1000)
RHGiniForwardCDF = forwardCDF(forward_years,maturity_length, RHGinilambda, "RH", "Gini", RH_Gini_Forward_Premium[forward_years], nsim=1000)
CBDGiniForwardCDF = forwardCDF(forward_years,maturity_length, CBDGinilambda, "CBD", "Gini", CBD_Gini_Forward_Premium[forward_years], nsim=1000)
M6GiniForwardCDF = forwardCDF(forward_years,maturity_length, M6Ginilambda, "M6", "Gini", M6_Gini_Forward_Premium[forward_years], nsim=1000)

LCExponentialForwardCDF = forwardCDF(forward_years,maturity_length, LCExponentiallambda, "LC", "Exponential", LC_Exponential_Forward_Premium[forward_years], nsim=1000)
RHExponentialForwardCDF = forwardCDF(forward_years,maturity_length, RHExponentiallambda, "RH", "Exponential", RH_Exponential_Forward_Premium[forward_years], nsim=1000)
CBDExponentialForwardCDF = forwardCDF(forward_years,maturity_length, CBDExponentiallambda, "CBD", "Exponential", CBD_Exponential_Forward_Premium[forward_years], nsim=1000)
M6ExponentialForwardCDF = forwardCDF(forward_years,maturity_length, M6Exponentiallambda, "M6", "Exponential", M6_Exponential_Forward_Premium[forward_years], nsim=1000)

LCStDevForwardCDF = forwardCDF(forward_years,maturity_length, LCStDevlambda, "LC", "StDev", LC_Std_Forward_Premium[forward_years], nsim=1000)
RHStDevForwardCDF = forwardCDF(forward_years,maturity_length, RHStDevlambda, "RH", "StDev", RH_Std_Forward_Premium[forward_years], nsim=1000)
CBDStDevForwardCDF = forwardCDF(forward_years,maturity_length, CBDStDevlambda, "CBD", "StDev", CBD_Std_Forward_Premium[forward_years], nsim=1000)
M6StDevForwardCDF = forwardCDF(forward_years,maturity_length, M6StDevlambda, "M6", "StDev", M6_Std_Forward_Premium[forward_years], nsim=1000)

LCVarForwardCDF = forwardCDF(forward_years,maturity_length, LCVarlambda, "LC", "Var", LC_Var_Forward_Premium[forward_years], nsim=1000)
RHVarForwardCDF = forwardCDF(forward_years,maturity_length, RHVarlambda, "RH", "Var", RH_Var_Forward_Premium[forward_years], nsim=1000)
CBDVarForwardCDF = forwardCDF(forward_years,maturity_length, CBDVarlambda, "CBD", "Var", CBD_Var_Forward_Premium[forward_years], nsim=1000)
M6VarForwardCDF = forwardCDF(forward_years,maturity_length, M6Varlambda, "M6", "Var", M6_Var_Forward_Premium[forward_years], nsim=1000)

LCMadForwardCDF = forwardCDF(forward_years,maturity_length, LCMadlambda, "LC", "Mad", LC_Mad_Forward_Premium[forward_years], nsim=1000)
RHMadForwardCDF = forwardCDF(forward_years,maturity_length, RHMadlambda, "RH", "Mad", RH_Mad_Forward_Premium[forward_years], nsim=1000)
CBDMadForwardCDF = forwardCDF(forward_years,maturity_length, CBDMadlambda, "CBD", "Mad", CBD_Mad_Forward_Premium[forward_years], nsim=1000)
M6MadForwardCDF = forwardCDF(forward_years,maturity_length, M6Madlambda, "M6", "Mad", M6_Mad_Forward_Premium[forward_years], nsim=1000)

hist(LCWangForwardCDF)
hist(RHWangForwardCDF)
hist(CBDWangForwardCDF)
hist(M6WangForwardCDF)

alpha = 0.05
LCWangVaR = quantile(LCWangForwardCDF, probs=alpha)
RHWangVaR = quantile(RHWangForwardCDF, probs=alpha)
CBDWangVaR = quantile(CBDWangForwardCDF, probs=alpha)
M6WangVaR = quantile(M6WangForwardCDF, probs=alpha)

LCPropVaR = quantile(LCPropForwardCDF, probs=alpha)
RHPropVaR = quantile(RHPropForwardCDF, probs=alpha)
CBDPropVaR = quantile(CBDPropForwardCDF, probs=alpha)
M6PropVaR = quantile(M6PropForwardCDF, probs=alpha)

LCDualVaR = quantile(LCDualForwardCDF, probs=alpha)
RHDualVaR = quantile(RHDualForwardCDF, probs=alpha)
CBDDualVaR = quantile(CBDDualForwardCDF, probs=alpha)
M6DualVaR = quantile(M6DualForwardCDF, probs=alpha)

LCGiniVaR = quantile(LCGiniForwardCDF, probs=alpha)
RHGiniVaR = quantile(RHGiniForwardCDF, probs=alpha)
CBDGiniVaR = quantile(CBDGiniForwardCDF, probs=alpha)
M6GiniVaR = quantile(M6GiniForwardCDF, probs=alpha)

LCExponentialVaR = quantile(LCExponentialForwardCDF, probs=alpha)
RHExponentialVaR = quantile(RHExponentialForwardCDF, probs=alpha)
CBDExponentialVaR = quantile(CBDExponentialForwardCDF, probs=alpha)
M6ExponentialVaR = quantile(M6ExponentialForwardCDF, probs=alpha)

LCStDevVaR = quantile(LCStDevForwardCDF, probs=alpha)
RHStDevVaR = quantile(RHStDevForwardCDF, probs=alpha)
CBDStDevVaR = quantile(CBDStDevForwardCDF, probs=alpha)
M6StDevVaR = quantile(M6StDevForwardCDF, probs=alpha)

LCVarVaR = quantile(LCVarForwardCDF, probs=alpha)
RHVarVaR = quantile(RHVarForwardCDF, probs=alpha)
CBDVarVaR = quantile(CBDVarForwardCDF, probs=alpha)
M6VarVaR = quantile(M6VarForwardCDF, probs=alpha)

LCMadVaR = quantile(LCMadForwardCDF, probs=alpha)
RHMadVaR = quantile(RHMadForwardCDF, probs=alpha)
CBDMadVaR = quantile(CBDMadForwardCDF, probs=alpha)
M6MadVaR = quantile(M6MadForwardCDF, probs=alpha)

Forward_VaR_table = data.frame(matrix(nrow = 8, ncol = 4, c(LCWangVaR, LCPropVaR, LCDualVaR, LCGiniVaR, LCExponentialVaR, LCStDevVaR, LCVarVaR, LCMadVaR,
                                                       RHWangVaR, RHPropVaR, RHDualVaR, RHGiniVaR, RHExponentialVaR, RHStDevVaR, RHVarVaR, RHMadVaR,
                                                       CBDWangVaR, CBDPropVaR, CBDDualVaR, CBDGiniVaR, CBDExponentialVaR, CBDStDevVaR, CBDVarVaR, CBDMadVaR,
                                                       M6WangVaR, M6PropVaR, M6DualVaR, M6GiniVaR, M6ExponentialVaR, M6StDevVaR, M6VarVaR, M6MadVaR)
))
rownames(Forward_VaR_table) = c("Wang", "Proportional", "Dual", "Gini", "Exponential", "StDev", "Var", "Mad")
colnames(Forward_VaR_table) = c("LC","RH","CBD","M6")
Forward_VaR_table

source("Robustness.R")

LCWangSwapCDF = SwapCDF(forward_years,maturity_length, LCWanglambda, "LC", "Wang", LC_Wang_Swap_Premium[forward_years], nsim=1000)
RHWangSwapCDF = SwapCDF(forward_years,maturity_length, RHWanglambda, "RH", "Wang", RH_Wang_Swap_Premium[forward_years], nsim=1000)
CBDWangSwapCDF = SwapCDF(forward_years,maturity_length, CBDWanglambda, "CBD", "Wang", CBD_Wang_Swap_Premium[forward_years], nsim=1000)
M6WangSwapCDF = SwapCDF(forward_years,maturity_length, M6Wanglambda, "M6", "Wang", M6_Wang_Swap_Premium[forward_years], nsim=1000)

LCPropSwapCDF = SwapCDF(forward_years,maturity_length, LCProplambda, "LC", "Proportional", LC_Prop_Swap_Premium[forward_years], nsim=1000)
RHPropSwapCDF = SwapCDF(forward_years,maturity_length, RHProplambda, "RH", "Proportional", RH_Prop_Swap_Premium[forward_years], nsim=1000)
CBDPropSwapCDF = SwapCDF(forward_years,maturity_length, CBDProplambda, "CBD", "Proportional", CBD_Prop_Swap_Premium[forward_years], nsim=1000)
M6PropSwapCDF = SwapCDF(forward_years,maturity_length, M6Proplambda, "M6", "Proportional", M6_Prop_Swap_Premium[forward_years], nsim=1000)

LCDualSwapCDF = SwapCDF(forward_years,maturity_length, LCDuallambda, "LC", "Dual", LC_Dual_Swap_Premium[forward_years], nsim=1000)
RHDualSwapCDF = SwapCDF(forward_years,maturity_length, RHDuallambda, "RH", "Dual", RH_Dual_Swap_Premium[forward_years], nsim=1000)
CBDDualSwapCDF = SwapCDF(forward_years,maturity_length, CBDDuallambda, "CBD", "Dual", CBD_Dual_Swap_Premium[forward_years], nsim=1000)
M6DualSwapCDF = SwapCDF(forward_years,maturity_length, M6Duallambda, "M6", "Dual", M6_Dual_Swap_Premium[forward_years], nsim=1000)

LCGiniSwapCDF = SwapCDF(forward_years,maturity_length, LCGinilambda, "LC", "Gini", LC_Gini_Swap_Premium[forward_years], nsim=1000)
RHGiniSwapCDF = SwapCDF(forward_years,maturity_length, RHGinilambda, "RH", "Gini", RH_Gini_Swap_Premium[forward_years], nsim=1000)
CBDGiniSwapCDF = SwapCDF(forward_years,maturity_length, CBDGinilambda, "CBD", "Gini", CBD_Gini_Swap_Premium[forward_years], nsim=1000)
M6GiniSwapCDF = SwapCDF(forward_years,maturity_length, M6Ginilambda, "M6", "Gini", M6_Gini_Swap_Premium[forward_years], nsim=1000)

LCExponentialSwapCDF = SwapCDF(forward_years,maturity_length, LCExponentiallambda, "LC", "Exponential", LC_Exponential_Swap_Premium[forward_years], nsim=1000)
RHExponentialSwapCDF = SwapCDF(forward_years,maturity_length, RHExponentiallambda, "RH", "Exponential", RH_Exponential_Swap_Premium[forward_years], nsim=1000)
CBDExponentialSwapCDF = SwapCDF(forward_years,maturity_length, CBDExponentiallambda, "CBD", "Exponential", CBD_Exponential_Swap_Premium[forward_years], nsim=1000)
M6ExponentialSwapCDF = SwapCDF(forward_years,maturity_length, M6Exponentiallambda, "M6", "Exponential", M6_Exponential_Swap_Premium[forward_years], nsim=1000)

LCStDevSwapCDF = SwapCDF(forward_years,maturity_length, LCStDevlambda, "LC", "StDev", LC_Std_Swap_Premium[forward_years], nsim=1000)
RHStDevSwapCDF = SwapCDF(forward_years,maturity_length, RHStDevlambda, "RH", "StDev", RH_Std_Swap_Premium[forward_years], nsim=1000)
CBDStDevSwapCDF = SwapCDF(forward_years,maturity_length, CBDStDevlambda, "CBD", "StDev", CBD_Std_Swap_Premium[forward_years], nsim=1000)
M6StDevSwapCDF = SwapCDF(forward_years,maturity_length, M6StDevlambda, "M6", "StDev", M6_Std_Swap_Premium[forward_years], nsim=1000)

LCVarSwapCDF = SwapCDF(forward_years,maturity_length, LCVarlambda, "LC", "Var", LC_Var_Swap_Premium[forward_years], nsim=1000)
RHVarSwapCDF = SwapCDF(forward_years,maturity_length, RHVarlambda, "RH", "Var", RH_Var_Swap_Premium[forward_years], nsim=1000)
CBDVarSwapCDF = SwapCDF(forward_years,maturity_length, CBDVarlambda, "CBD", "Var", CBD_Var_Swap_Premium[forward_years], nsim=1000)
M6VarSwapCDF = SwapCDF(forward_years,maturity_length, M6Varlambda, "M6", "Var", M6_Var_Swap_Premium[forward_years], nsim=1000)

LCMadSwapCDF = SwapCDF(forward_years,maturity_length, LCMadlambda, "LC", "Mad", LC_Mad_Swap_Premium[forward_years], nsim=1000)
RHMadSwapCDF = SwapCDF(forward_years,maturity_length, RHMadlambda, "RH", "Mad", RH_Mad_Swap_Premium[forward_years], nsim=1000)
CBDMadSwapCDF = SwapCDF(forward_years,maturity_length, CBDMadlambda, "CBD", "Mad", CBD_Mad_Swap_Premium[forward_years], nsim=1000)
M6MadSwapCDF = SwapCDF(forward_years,maturity_length, M6Madlambda, "M6", "Mad", M6_Mad_Swap_Premium[forward_years], nsim=1000)

hist(LCWangSwapCDF)
hist(RHWangSwapCDF)
hist(CBDWangSwapCDF)
hist(M6WangSwapCDF)

alpha = 0.05
LCWangVaR = quantile(LCWangSwapCDF, probs=alpha)
RHWangVaR = quantile(RHWangSwapCDF, probs=alpha)
CBDWangVaR = quantile(CBDWangSwapCDF, probs=alpha)
M6WangVaR = quantile(M6WangSwapCDF, probs=alpha)

LCPropVaR = quantile(LCPropSwapCDF, probs=alpha)
RHPropVaR = quantile(RHPropSwapCDF, probs=alpha)
CBDPropVaR = quantile(CBDPropSwapCDF, probs=alpha)
M6PropVaR = quantile(M6PropSwapCDF, probs=alpha)

LCDualVaR = quantile(LCDualSwapCDF, probs=alpha)
RHDualVaR = quantile(RHDualSwapCDF, probs=alpha)
CBDDualVaR = quantile(CBDDualSwapCDF, probs=alpha)
M6DualVaR = quantile(M6DualSwapCDF, probs=alpha)

LCGiniVaR = quantile(LCGiniSwapCDF, probs=alpha)
RHGiniVaR = quantile(RHGiniSwapCDF, probs=alpha)
CBDGiniVaR = quantile(CBDGiniSwapCDF, probs=alpha)
M6GiniVaR = quantile(M6GiniSwapCDF, probs=alpha)

LCExponentialVaR = quantile(LCExponentialSwapCDF, probs=alpha)
RHExponentialVaR = quantile(RHExponentialSwapCDF, probs=alpha)
CBDExponentialVaR = quantile(CBDExponentialSwapCDF, probs=alpha)
M6ExponentialVaR = quantile(M6ExponentialSwapCDF, probs=alpha)

LCStDevVaR = quantile(LCStDevSwapCDF, probs=alpha)
RHStDevVaR = quantile(RHStDevSwapCDF, probs=alpha)
CBDStDevVaR = quantile(CBDStDevSwapCDF, probs=alpha)
M6StDevVaR = quantile(M6StDevSwapCDF, probs=alpha)

LCVarVaR = quantile(LCVarSwapCDF, probs=alpha)
RHVarVaR = quantile(RHVarSwapCDF, probs=alpha)
CBDVarVaR = quantile(CBDVarSwapCDF, probs=alpha)
M6VarVaR = quantile(M6VarSwapCDF, probs=alpha)

LCMadVaR = quantile(LCMadSwapCDF, probs=alpha)
RHMadVaR = quantile(RHMadSwapCDF, probs=alpha)
CBDMadVaR = quantile(CBDMadSwapCDF, probs=alpha)
M6MadVaR = quantile(M6MadSwapCDF, probs=alpha)

Swap_VaR_table = data.frame(matrix(nrow = 8, ncol = 4, c(LCWangVaR, LCPropVaR, LCDualVaR, LCGiniVaR, LCExponentialVaR, LCStDevVaR, LCVarVaR, LCMadVaR,
                                                            RHWangVaR, RHPropVaR, RHDualVaR, RHGiniVaR, RHExponentialVaR, RHStDevVaR, RHVarVaR, RHMadVaR,
                                                            CBDWangVaR, CBDPropVaR, CBDDualVaR, CBDGiniVaR, CBDExponentialVaR, CBDStDevVaR, CBDVarVaR, CBDMadVaR,
                                                            M6WangVaR, M6PropVaR, M6DualVaR, M6GiniVaR, M6ExponentialVaR, M6StDevVaR, M6VarVaR, M6MadVaR)
))
rownames(Swap_VaR_table) = c("Wang", "Proportional", "Dual", "Gini", "Exponential", "StDev", "Var", "Mad")
colnames(Swap_VaR_table) = c("LC","RH","CBD","M6")
Swap_VaR_table

round(Swap_VaR_table,4)
round(Forward_VaR_table,4)
