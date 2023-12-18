# Reduce chances of scientific notation
options(digits=4, scipen=999)
# Generic libraries used
library(demography)
library(StMoMo)
library(lifecontingencies)
library(stats)
library(cvar)
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
nsimCDF = 3000

LCWangForwardCDF = forwardCDF(forward_years,maturity_length, LCWanglambda, "LC", "Wang", LC_Wang_Forward_Premium[forward_years], nsim=nsimCDF)
RHWangForwardCDF = forwardCDF(forward_years,maturity_length, RHWanglambda, "RH", "Wang", RH_Wang_Forward_Premium[forward_years], nsim=nsimCDF)
CBDWangForwardCDF = forwardCDF(forward_years,maturity_length, CBDWanglambda, "CBD", "Wang", CBD_Wang_Forward_Premium[forward_years], nsim=nsimCDF)
M6WangForwardCDF = forwardCDF(forward_years,maturity_length, M6Wanglambda, "M6", "Wang", M6_Wang_Forward_Premium[forward_years], nsim=nsimCDF)

LCPropForwardCDF = forwardCDF(forward_years,maturity_length, LCProplambda, "LC", "Proportional", LC_Prop_Forward_Premium[forward_years], nsim=nsimCDF)
RHPropForwardCDF = forwardCDF(forward_years,maturity_length, RHProplambda, "RH", "Proportional", RH_Prop_Forward_Premium[forward_years], nsim=nsimCDF)
CBDPropForwardCDF = forwardCDF(forward_years,maturity_length, CBDProplambda, "CBD", "Proportional", CBD_Prop_Forward_Premium[forward_years], nsim=nsimCDF)
M6PropForwardCDF = forwardCDF(forward_years,maturity_length, M6Proplambda, "M6", "Proportional", M6_Prop_Forward_Premium[forward_years], nsim=nsimCDF)

LCDualForwardCDF = forwardCDF(forward_years,maturity_length, LCDuallambda, "LC", "Dual", LC_Dual_Forward_Premium[forward_years], nsim=nsimCDF)
RHDualForwardCDF = forwardCDF(forward_years,maturity_length, RHDuallambda, "RH", "Dual", RH_Dual_Forward_Premium[forward_years], nsim=nsimCDF)
CBDDualForwardCDF = forwardCDF(forward_years,maturity_length, CBDDuallambda, "CBD", "Dual", CBD_Dual_Forward_Premium[forward_years], nsim=nsimCDF)
M6DualForwardCDF = forwardCDF(forward_years,maturity_length, M6Duallambda, "M6", "Dual", M6_Dual_Forward_Premium[forward_years], nsim=nsimCDF)

LCGiniForwardCDF = forwardCDF(forward_years,maturity_length, LCGinilambda, "LC", "Gini", LC_Gini_Forward_Premium[forward_years], nsim=nsimCDF)
RHGiniForwardCDF = forwardCDF(forward_years,maturity_length, RHGinilambda, "RH", "Gini", RH_Gini_Forward_Premium[forward_years], nsim=nsimCDF)
CBDGiniForwardCDF = forwardCDF(forward_years,maturity_length, CBDGinilambda, "CBD", "Gini", CBD_Gini_Forward_Premium[forward_years], nsim=nsimCDF)
M6GiniForwardCDF = forwardCDF(forward_years,maturity_length, M6Ginilambda, "M6", "Gini", M6_Gini_Forward_Premium[forward_years], nsim=nsimCDF)

LCExponentialForwardCDF = forwardCDF(forward_years,maturity_length, LCExponentiallambda, "LC", "Exponential", LC_Exponential_Forward_Premium[forward_years], nsim=nsimCDF)
RHExponentialForwardCDF = forwardCDF(forward_years,maturity_length, RHExponentiallambda, "RH", "Exponential", RH_Exponential_Forward_Premium[forward_years], nsim=nsimCDF)
CBDExponentialForwardCDF = forwardCDF(forward_years,maturity_length, CBDExponentiallambda, "CBD", "Exponential", CBD_Exponential_Forward_Premium[forward_years], nsim=nsimCDF)
M6ExponentialForwardCDF = forwardCDF(forward_years,maturity_length, M6Exponentiallambda, "M6", "Exponential", M6_Exponential_Forward_Premium[forward_years], nsim=nsimCDF)

LCStDevForwardCDF = forwardCDF(forward_years,maturity_length, LCStDevlambda, "LC", "StDev", LC_Std_Forward_Premium[forward_years], nsim=nsimCDF)
RHStDevForwardCDF = forwardCDF(forward_years,maturity_length, RHStDevlambda, "RH", "StDev", RH_Std_Forward_Premium[forward_years], nsim=nsimCDF)
CBDStDevForwardCDF = forwardCDF(forward_years,maturity_length, CBDStDevlambda, "CBD", "StDev", CBD_Std_Forward_Premium[forward_years], nsim=nsimCDF)
M6StDevForwardCDF = forwardCDF(forward_years,maturity_length, M6StDevlambda, "M6", "StDev", M6_Std_Forward_Premium[forward_years], nsim=nsimCDF)

LCVarForwardCDF = forwardCDF(forward_years,maturity_length, LCVarlambda, "LC", "Var", LC_Var_Forward_Premium[forward_years], nsim=nsimCDF)
RHVarForwardCDF = forwardCDF(forward_years,maturity_length, RHVarlambda, "RH", "Var", RH_Var_Forward_Premium[forward_years], nsim=nsimCDF)
CBDVarForwardCDF = forwardCDF(forward_years,maturity_length, CBDVarlambda, "CBD", "Var", CBD_Var_Forward_Premium[forward_years], nsim=nsimCDF)
M6VarForwardCDF = forwardCDF(forward_years,maturity_length, M6Varlambda, "M6", "Var", M6_Var_Forward_Premium[forward_years], nsim=nsimCDF)

LCMadForwardCDF = forwardCDF(forward_years,maturity_length, LCMadlambda, "LC", "Mad", LC_Mad_Forward_Premium[forward_years], nsim=nsimCDF)
RHMadForwardCDF = forwardCDF(forward_years,maturity_length, RHMadlambda, "RH", "Mad", RH_Mad_Forward_Premium[forward_years], nsim=nsimCDF)
CBDMadForwardCDF = forwardCDF(forward_years,maturity_length, CBDMadlambda, "CBD", "Mad", CBD_Mad_Forward_Premium[forward_years], nsim=nsimCDF)
M6MadForwardCDF = forwardCDF(forward_years,maturity_length, M6Madlambda, "M6", "Mad", M6_Mad_Forward_Premium[forward_years], nsim=nsimCDF)


alpha = 0.005
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

############ SWAP CDF ###############
LCWangSwapCDF = SwapCDF(forward_years,maturity_length, LCWanglambda, "LC", "Wang", LC_Wang_Swap_Premium[forward_years], nsim=nsimCDF)
RHWangSwapCDF = SwapCDF(forward_years,maturity_length, RHWanglambda, "RH", "Wang", RH_Wang_Swap_Premium[forward_years], nsim=nsimCDF)
CBDWangSwapCDF = SwapCDF(forward_years,maturity_length, CBDWanglambda, "CBD", "Wang", CBD_Wang_Swap_Premium[forward_years], nsim=nsimCDF)
M6WangSwapCDF = SwapCDF(forward_years,maturity_length, M6Wanglambda, "M6", "Wang", M6_Wang_Swap_Premium[forward_years], nsim=nsimCDF)

LCPropSwapCDF = SwapCDF(forward_years,maturity_length, LCProplambda, "LC", "Proportional", LC_Prop_Swap_Premium[forward_years], nsim=nsimCDF)
RHPropSwapCDF = SwapCDF(forward_years,maturity_length, RHProplambda, "RH", "Proportional", RH_Prop_Swap_Premium[forward_years], nsim=nsimCDF)
CBDPropSwapCDF = SwapCDF(forward_years,maturity_length, CBDProplambda, "CBD", "Proportional", CBD_Prop_Swap_Premium[forward_years], nsim=nsimCDF)
M6PropSwapCDF = SwapCDF(forward_years,maturity_length, M6Proplambda, "M6", "Proportional", M6_Prop_Swap_Premium[forward_years], nsim=nsimCDF)

LCDualSwapCDF = SwapCDF(forward_years,maturity_length, LCDuallambda, "LC", "Dual", LC_Dual_Swap_Premium[forward_years], nsim=nsimCDF)
RHDualSwapCDF = SwapCDF(forward_years,maturity_length, RHDuallambda, "RH", "Dual", RH_Dual_Swap_Premium[forward_years], nsim=nsimCDF)
CBDDualSwapCDF = SwapCDF(forward_years,maturity_length, CBDDuallambda, "CBD", "Dual", CBD_Dual_Swap_Premium[forward_years], nsim=nsimCDF)
M6DualSwapCDF = SwapCDF(forward_years,maturity_length, M6Duallambda, "M6", "Dual", M6_Dual_Swap_Premium[forward_years], nsim=nsimCDF)

LCGiniSwapCDF = SwapCDF(forward_years,maturity_length, LCGinilambda, "LC", "Gini", LC_Gini_Swap_Premium[forward_years], nsim=nsimCDF)
RHGiniSwapCDF = SwapCDF(forward_years,maturity_length, RHGinilambda, "RH", "Gini", RH_Gini_Swap_Premium[forward_years], nsim=nsimCDF)
CBDGiniSwapCDF = SwapCDF(forward_years,maturity_length, CBDGinilambda, "CBD", "Gini", CBD_Gini_Swap_Premium[forward_years], nsim=nsimCDF)
M6GiniSwapCDF = SwapCDF(forward_years,maturity_length, M6Ginilambda, "M6", "Gini", M6_Gini_Swap_Premium[forward_years], nsim=nsimCDF)

LCExponentialSwapCDF = SwapCDF(forward_years,maturity_length, LCExponentiallambda, "LC", "Exponential", LC_Exponential_Swap_Premium[forward_years], nsim=nsimCDF)
RHExponentialSwapCDF = SwapCDF(forward_years,maturity_length, RHExponentiallambda, "RH", "Exponential", RH_Exponential_Swap_Premium[forward_years], nsim=nsimCDF)
CBDExponentialSwapCDF = SwapCDF(forward_years,maturity_length, CBDExponentiallambda, "CBD", "Exponential", CBD_Exponential_Swap_Premium[forward_years], nsim=nsimCDF)
M6ExponentialSwapCDF = SwapCDF(forward_years,maturity_length, M6Exponentiallambda, "M6", "Exponential", M6_Exponential_Swap_Premium[forward_years], nsim=nsimCDF)

LCStDevSwapCDF = SwapCDF(forward_years,maturity_length, LCStDevlambda, "LC", "StDev", LC_Std_Swap_Premium[forward_years], nsim=nsimCDF)
RHStDevSwapCDF = SwapCDF(forward_years,maturity_length, RHStDevlambda, "RH", "StDev", RH_Std_Swap_Premium[forward_years], nsim=nsimCDF)
CBDStDevSwapCDF = SwapCDF(forward_years,maturity_length, CBDStDevlambda, "CBD", "StDev", CBD_Std_Swap_Premium[forward_years], nsim=nsimCDF)
M6StDevSwapCDF = SwapCDF(forward_years,maturity_length, M6StDevlambda, "M6", "StDev", M6_Std_Swap_Premium[forward_years], nsim=nsimCDF)

LCVarSwapCDF = SwapCDF(forward_years,maturity_length, LCVarlambda, "LC", "Var", LC_Var_Swap_Premium[forward_years], nsim=nsimCDF)
RHVarSwapCDF = SwapCDF(forward_years,maturity_length, RHVarlambda, "RH", "Var", RH_Var_Swap_Premium[forward_years], nsim=nsimCDF)
CBDVarSwapCDF = SwapCDF(forward_years,maturity_length, CBDVarlambda, "CBD", "Var", CBD_Var_Swap_Premium[forward_years], nsim=nsimCDF)
M6VarSwapCDF = SwapCDF(forward_years,maturity_length, M6Varlambda, "M6", "Var", M6_Var_Swap_Premium[forward_years], nsim=nsimCDF)

LCMadSwapCDF = SwapCDF(forward_years,maturity_length, LCMadlambda, "LC", "Mad", LC_Mad_Swap_Premium[forward_years], nsim=nsimCDF)
RHMadSwapCDF = SwapCDF(forward_years,maturity_length, RHMadlambda, "RH", "Mad", RH_Mad_Swap_Premium[forward_years], nsim=nsimCDF)
CBDMadSwapCDF = SwapCDF(forward_years,maturity_length, CBDMadlambda, "CBD", "Mad", CBD_Mad_Swap_Premium[forward_years], nsim=nsimCDF)
M6MadSwapCDF = SwapCDF(forward_years,maturity_length, M6Madlambda, "M6", "Mad", M6_Mad_Swap_Premium[forward_years], nsim=nsimCDF)

LCWangSwapVaR = quantile(LCWangSwapCDF, probs=alpha)
RHWangSwapVaR = quantile(RHWangSwapCDF, probs=alpha)
CBDWangSwapVaR = quantile(CBDWangSwapCDF, probs=alpha)
M6WangSwapVaR = quantile(M6WangSwapCDF, probs=alpha)

LCPropSwapVaR = quantile(LCPropSwapCDF, probs=alpha)
RHPropSwapVaR = quantile(RHPropSwapCDF, probs=alpha)
CBDPropSwapVaR = quantile(CBDPropSwapCDF, probs=alpha)
M6PropSwapVaR = quantile(M6PropSwapCDF, probs=alpha)

LCDualSwapVaR = quantile(LCDualSwapCDF, probs=alpha)
RHDualSwapVaR = quantile(RHDualSwapCDF, probs=alpha)
CBDDualSwapVaR = quantile(CBDDualSwapCDF, probs=alpha)
M6DualSwapVaR = quantile(M6DualSwapCDF, probs=alpha)

LCGiniSwapVaR = quantile(LCGiniSwapCDF, probs=alpha)
RHGiniSwapVaR = quantile(RHGiniSwapCDF, probs=alpha)
CBDGiniSwapVaR = quantile(CBDGiniSwapCDF, probs=alpha)
M6GiniSwapVaR = quantile(M6GiniSwapCDF, probs=alpha)

LCExponentialSwapVaR = quantile(LCExponentialSwapCDF, probs=alpha)
RHExponentialSwapVaR = quantile(RHExponentialSwapCDF, probs=alpha)
CBDExponentialSwapVaR = quantile(CBDExponentialSwapCDF, probs=alpha)
M6ExponentialSwapVaR = quantile(M6ExponentialSwapCDF, probs=alpha)

LCStDevSwapVaR = quantile(LCStDevSwapCDF, probs=alpha)
RHStDevSwapVaR = quantile(RHStDevSwapCDF, probs=alpha)
CBDStDevSwapVaR = quantile(CBDStDevSwapCDF, probs=alpha)
M6StDevSwapVaR = quantile(M6StDevSwapCDF, probs=alpha)

LCVarSwapVaR = quantile(LCVarSwapCDF, probs=alpha)
RHVarSwapVaR = quantile(RHVarSwapCDF, probs=alpha)
CBDVarSwapVaR = quantile(CBDVarSwapCDF, probs=alpha)
M6VarSwapVaR = quantile(M6VarSwapCDF, probs=alpha)

LCMadSwapVaR = quantile(LCMadSwapCDF, probs=alpha)
RHMadSwapVaR = quantile(RHMadSwapCDF, probs=alpha)
CBDMadSwapVaR = quantile(CBDMadSwapCDF, probs=alpha)
M6MadSwapVaR = quantile(M6MadSwapCDF, probs=alpha)


################## EXPECTED SHORTFALL ################
LCWangES = mean( sort(LCWangForwardCDF)[1:(nsimCDF*alpha - 1)] )
RHWangES = mean( sort(RHWangForwardCDF)[1:(nsimCDF*alpha - 1)] )
CBDWangES = mean( sort(CBDWangForwardCDF)[1:(nsimCDF*alpha - 1)] )
M6WangES = mean( sort(M6WangForwardCDF)[1:(nsimCDF*alpha - 1)] )

LCPropES = mean( sort(LCPropForwardCDF)[1:(nsimCDF*alpha - 1)] )
RHPropES = mean( sort(RHPropForwardCDF)[1:(nsimCDF*alpha - 1)] )
CBDPropES = mean( sort(CBDPropForwardCDF)[1:(nsimCDF*alpha - 1)] )
M6PropES = mean( sort(M6PropForwardCDF)[1:(nsimCDF*alpha - 1)] )

LCDualES = mean( sort(LCDualForwardCDF)[1:(nsimCDF*alpha - 1)] )
RHDualES = mean( sort(RHDualForwardCDF)[1:(nsimCDF*alpha - 1)] )
CBDDualES = mean( sort(CBDDualForwardCDF)[1:(nsimCDF*alpha - 1)] )
M6DualES = mean( sort(M6DualForwardCDF)[1:(nsimCDF*alpha - 1)] )

LCGiniES = mean( sort(LCGiniForwardCDF)[1:(nsimCDF*alpha - 1)] )
RHGiniES = mean( sort(RHGiniForwardCDF)[1:(nsimCDF*alpha - 1)] )
CBDGiniES = mean( sort(CBDGiniForwardCDF)[1:(nsimCDF*alpha - 1)] )
M6GiniES = mean( sort(M6GiniForwardCDF)[1:(nsimCDF*alpha - 1)] )

LCExponentialES = mean( sort(LCExponentialForwardCDF)[1:(nsimCDF*alpha - 1)] )
RHExponentialES = mean( sort(RHExponentialForwardCDF)[1:(nsimCDF*alpha - 1)] )
CBDExponentialES = mean( sort(CBDExponentialForwardCDF)[1:(nsimCDF*alpha - 1)] )
M6ExponentialES = mean( sort(M6ExponentialForwardCDF)[1:(nsimCDF*alpha - 1)] )

LCStDevES = mean( sort(LCStDevForwardCDF)[1:(nsimCDF*alpha - 1)] )
RHStDevES = mean( sort(RHStDevForwardCDF)[1:(nsimCDF*alpha - 1)] )
CBDStDevES = mean( sort(CBDStDevForwardCDF)[1:(nsimCDF*alpha - 1)] )
M6StDevES = mean( sort(M6StDevForwardCDF)[1:(nsimCDF*alpha - 1)] )

LCVarES = mean( sort(LCVarForwardCDF)[1:(nsimCDF*alpha - 1)] )
RHVarES = mean( sort(RHVarForwardCDF)[1:(nsimCDF*alpha - 1)] )
CBDVarES = mean( sort(CBDVarForwardCDF)[1:(nsimCDF*alpha - 1)] )
M6VarES = mean( sort(M6VarForwardCDF)[1:(nsimCDF*alpha - 1)] )

LCMadES = mean( sort(LCMadForwardCDF)[1:(nsimCDF*alpha - 1)] )
RHMadES = mean( sort(RHMadForwardCDF)[1:(nsimCDF*alpha - 1)] )
CBDMadES = mean( sort(CBDMadForwardCDF)[1:(nsimCDF*alpha - 1)] )
M6MadES = mean( sort(M6MadForwardCDF)[1:(nsimCDF*alpha - 1)] )

LCWangSwapES = mean( sort(LCWangSwapCDF)[1:(nsimCDF*alpha - 1)] )
RHWangSwapES = mean( sort(RHWangSwapCDF)[1:(nsimCDF*alpha - 1)] )
CBDWangSwapES = mean( sort(CBDWangSwapCDF)[1:(nsimCDF*alpha - 1)] )
M6WangSwapES = mean( sort(M6WangSwapCDF)[1:(nsimCDF*alpha - 1)] )

LCPropSwapES = mean( sort(LCPropSwapCDF)[1:(nsimCDF*alpha - 1)] )
RHPropSwapES = mean( sort(RHPropSwapCDF)[1:(nsimCDF*alpha - 1)] )
CBDPropSwapES = mean( sort(CBDPropSwapCDF)[1:(nsimCDF*alpha - 1)] )
M6PropSwapES = mean( sort(M6PropSwapCDF)[1:(nsimCDF*alpha - 1)] )

LCDualSwapES = mean( sort(LCDualSwapCDF)[1:(nsimCDF*alpha - 1)] )
RHDualSwapES = mean( sort(RHDualSwapCDF)[1:(nsimCDF*alpha - 1)] )
CBDDualSwapES = mean( sort(CBDDualSwapCDF)[1:(nsimCDF*alpha - 1)] )
M6DualSwapES = mean( sort(M6DualSwapCDF)[1:(nsimCDF*alpha - 1)] )

LCGiniSwapES = mean( sort(LCGiniSwapCDF)[1:(nsimCDF*alpha - 1)] )
RHGiniSwapES = mean( sort(RHGiniSwapCDF)[1:(nsimCDF*alpha - 1)] )
CBDGiniSwapES = mean( sort(CBDGiniSwapCDF)[1:(nsimCDF*alpha - 1)] )
M6GiniSwapES = mean( sort(M6GiniSwapCDF)[1:(nsimCDF*alpha - 1)] )

LCExponentialSwapES = mean( sort(LCExponentialSwapCDF)[1:(nsimCDF*alpha - 1)] )
RHExponentialSwapES = mean( sort(RHExponentialSwapCDF)[1:(nsimCDF*alpha - 1)] )
CBDExponentialSwapES = mean( sort(CBDExponentialSwapCDF)[1:(nsimCDF*alpha - 1)] )
M6ExponentialSwapES = mean( sort(M6ExponentialSwapCDF)[1:(nsimCDF*alpha - 1)] )

LCStDevSwapES = mean( sort(LCStDevSwapCDF)[1:(nsimCDF*alpha - 1)] )
RHStDevSwapES = mean( sort(RHStDevSwapCDF)[1:(nsimCDF*alpha - 1)] )
CBDStDevSwapES = mean( sort(CBDStDevSwapCDF)[1:(nsimCDF*alpha - 1)] )
M6StDevSwapES = mean( sort(M6StDevSwapCDF)[1:(nsimCDF*alpha - 1)] )

LCVarSwapES = mean( sort(LCVarSwapCDF)[1:(nsimCDF*alpha - 1)] )
RHVarSwapES = mean( sort(RHVarSwapCDF)[1:(nsimCDF*alpha - 1)] )
CBDVarSwapES = mean( sort(CBDVarSwapCDF)[1:(nsimCDF*alpha - 1)] )
M6VarSwapES = mean( sort(M6VarSwapCDF)[1:(nsimCDF*alpha - 1)] )

LCMadSwapES = mean( sort(LCMadSwapCDF)[1:(nsimCDF*alpha - 1)] )
RHMadSwapES = mean( sort(RHMadSwapCDF)[1:(nsimCDF*alpha - 1)] )
CBDMadSwapES = mean( sort(CBDMadSwapCDF)[1:(nsimCDF*alpha - 1)] )
M6MadSwapES = mean( sort(M6MadSwapCDF)[1:(nsimCDF*alpha - 1)] )

LCWangES = mean( sort(LCWangForwardCDF)[1:(nsimCDF*alpha - 1)] )
RHWangES = mean( sort(RHWangForwardCDF)[1:(nsimCDF*alpha - 1)] )
CBDWangES = mean( sort(CBDWangForwardCDF)[1:(nsimCDF*alpha - 1)] )
M6WangES = mean( sort(M6WangForwardCDF)[1:(nsimCDF*alpha - 1)] )

LCPropES = mean( sort(LCPropForwardCDF)[1:(nsimCDF*alpha - 1)] )
RHPropES = mean( sort(RHPropForwardCDF)[1:(nsimCDF*alpha - 1)] )
CBDPropES = mean( sort(CBDPropForwardCDF)[1:(nsimCDF*alpha - 1)] )
M6PropES = mean( sort(M6PropForwardCDF)[1:(nsimCDF*alpha - 1)] )

LCDualES = mean( sort(LCDualForwardCDF)[1:(nsimCDF*alpha - 1)] )
RHDualES = mean( sort(RHDualForwardCDF)[1:(nsimCDF*alpha - 1)] )
CBDDualES = mean( sort(CBDDualForwardCDF)[1:(nsimCDF*alpha - 1)] )
M6DualES = mean( sort(M6DualForwardCDF)[1:(nsimCDF*alpha - 1)] )

LCGiniES = mean( sort(LCGiniForwardCDF)[1:(nsimCDF*alpha - 1)] )
RHGiniES = mean( sort(RHGiniForwardCDF)[1:(nsimCDF*alpha - 1)] )
CBDGiniES = mean( sort(CBDGiniForwardCDF)[1:(nsimCDF*alpha - 1)] )
M6GiniES = mean( sort(M6GiniForwardCDF)[1:(nsimCDF*alpha - 1)] )

LCExponentialES = mean( sort(LCExponentialForwardCDF)[1:(nsimCDF*alpha - 1)] )
RHExponentialES = mean( sort(RHExponentialForwardCDF)[1:(nsimCDF*alpha - 1)] )
CBDExponentialES = mean( sort(CBDExponentialForwardCDF)[1:(nsimCDF*alpha - 1)] )
M6ExponentialES = mean( sort(M6ExponentialForwardCDF)[1:(nsimCDF*alpha - 1)] )

LCStDevES = mean( sort(LCStDevForwardCDF)[1:(nsimCDF*alpha - 1)] )
RHStDevES = mean( sort(RHStDevForwardCDF)[1:(nsimCDF*alpha - 1)] )
CBDStDevES = mean( sort(CBDStDevForwardCDF)[1:(nsimCDF*alpha - 1)] )
M6StDevES = mean( sort(M6StDevForwardCDF)[1:(nsimCDF*alpha - 1)] )

LCVarES = mean( sort(LCVarForwardCDF)[1:(nsimCDF*alpha - 1)] )
RHVarES = mean( sort(RHVarForwardCDF)[1:(nsimCDF*alpha - 1)] )
CBDVarES = mean( sort(CBDVarForwardCDF)[1:(nsimCDF*alpha - 1)] )
M6VarES = mean( sort(M6VarForwardCDF)[1:(nsimCDF*alpha - 1)] )

LCMadES = mean( sort(LCMadForwardCDF)[1:(nsimCDF*alpha - 1)] )
RHMadES = mean( sort(RHMadForwardCDF)[1:(nsimCDF*alpha - 1)] )
CBDMadES = mean( sort(CBDMadForwardCDF)[1:(nsimCDF*alpha - 1)] )
M6MadES = mean( sort(M6MadForwardCDF)[1:(nsimCDF*alpha - 1)] )

LCWangSwapES = mean( sort(LCWangSwapCDF)[1:(nsimCDF*alpha - 1)] )
RHWangSwapES = mean( sort(RHWangSwapCDF)[1:(nsimCDF*alpha - 1)] )
CBDWangSwapES = mean( sort(CBDWangSwapCDF)[1:(nsimCDF*alpha - 1)] )
M6WangSwapES = mean( sort(M6WangSwapCDF)[1:(nsimCDF*alpha - 1)] )

LCPropSwapES = mean( sort(LCPropSwapCDF)[1:(nsimCDF*alpha - 1)] )
RHPropSwapES = mean( sort(RHPropSwapCDF)[1:(nsimCDF*alpha - 1)] )
CBDPropSwapES = mean( sort(CBDPropSwapCDF)[1:(nsimCDF*alpha - 1)] )
M6PropSwapES = mean( sort(M6PropSwapCDF)[1:(nsimCDF*alpha - 1)] )

LCDualSwapES = mean( sort(LCDualSwapCDF)[1:(nsimCDF*alpha - 1)] )
RHDualSwapES = mean( sort(RHDualSwapCDF)[1:(nsimCDF*alpha - 1)] )
CBDDualSwapES = mean( sort(CBDDualSwapCDF)[1:(nsimCDF*alpha - 1)] )
M6DualSwapES = mean( sort(M6DualSwapCDF)[1:(nsimCDF*alpha - 1)] )

LCGiniSwapES = mean( sort(LCGiniSwapCDF)[1:(nsimCDF*alpha - 1)] )
RHGiniSwapES = mean( sort(RHGiniSwapCDF)[1:(nsimCDF*alpha - 1)] )
CBDGiniSwapES = mean( sort(CBDGiniSwapCDF)[1:(nsimCDF*alpha - 1)] )
M6GiniSwapES = mean( sort(M6GiniSwapCDF)[1:(nsimCDF*alpha - 1)] )

LCExponentialSwapES = mean( sort(LCExponentialSwapCDF)[1:(nsimCDF*alpha - 1)] )
RHExponentialSwapES = mean( sort(RHExponentialSwapCDF)[1:(nsimCDF*alpha - 1)] )
CBDExponentialSwapES = mean( sort(CBDExponentialSwapCDF)[1:(nsimCDF*alpha - 1)] )
M6ExponentialSwapES = mean( sort(M6ExponentialSwapCDF)[1:(nsimCDF*alpha - 1)] )

LCStDevSwapES = mean( sort(LCStDevSwapCDF)[1:(nsimCDF*alpha - 1)] )
RHStDevSwapES = mean( sort(RHStDevSwapCDF)[1:(nsimCDF*alpha - 1)] )
CBDStDevSwapES = mean( sort(CBDStDevSwapCDF)[1:(nsimCDF*alpha - 1)] )
M6StDevSwapES = mean( sort(M6StDevSwapCDF)[1:(nsimCDF*alpha - 1)] )

LCVarSwapES = mean( sort(LCVarSwapCDF)[1:(nsimCDF*alpha - 1)] )
RHVarSwapES = mean( sort(RHVarSwapCDF)[1:(nsimCDF*alpha - 1)] )
CBDVarSwapES = mean( sort(CBDVarSwapCDF)[1:(nsimCDF*alpha - 1)] )
M6VarSwapES = mean( sort(M6VarSwapCDF)[1:(nsimCDF*alpha - 1)] )

LCMadSwapES = mean( sort(LCMadSwapCDF)[1:(nsimCDF*alpha - 1)] )
RHMadSwapES = mean( sort(RHMadSwapCDF)[1:(nsimCDF*alpha - 1)] )
CBDMadSwapES = mean( sort(CBDMadSwapCDF)[1:(nsimCDF*alpha - 1)] )
M6MadSwapES = mean( sort(M6MadSwapCDF)[1:(nsimCDF*alpha - 1)] )


Forward_VaR_table = data.frame(matrix(nrow = 8, ncol = 4, c(LCWangVaR, LCPropVaR, LCDualVaR, LCGiniVaR, LCExponentialVaR, LCStDevVaR, LCVarVaR, LCMadVaR,
                                                            RHWangVaR, RHPropVaR, RHDualVaR, RHGiniVaR, RHExponentialVaR, RHStDevVaR, RHVarVaR, RHMadVaR,
                                                            CBDWangVaR, CBDPropVaR, CBDDualVaR, CBDGiniVaR, CBDExponentialVaR, CBDStDevVaR, CBDVarVaR, CBDMadVaR,
                                                            M6WangVaR, M6PropVaR, M6DualVaR, M6GiniVaR, M6ExponentialVaR, M6StDevVaR, M6VarVaR, M6MadVaR)
))
rownames(Forward_VaR_table) = c("Wang", "Proportional", "Dual", "Gini", "Exponential", "StDev", "Var", "Mad")
colnames(Forward_VaR_table) = c("LC","RH","CBD","M6")

Swap_VaR_table = data.frame(matrix(nrow = 8, ncol = 4, c(LCWangSwapVaR, LCPropSwapVaR, LCDualSwapVaR, LCGiniSwapVaR, LCExponentialSwapVaR, LCStDevSwapVaR, LCVarSwapVaR, LCMadSwapVaR,
                                                         RHWangSwapVaR, RHPropSwapVaR, RHDualSwapVaR, RHGiniSwapVaR, RHExponentialSwapVaR, RHStDevSwapVaR, RHVarSwapVaR, RHMadSwapVaR,
                                                         CBDWangSwapVaR, CBDPropSwapVaR, CBDDualSwapVaR, CBDGiniSwapVaR, CBDExponentialSwapVaR, CBDStDevSwapVaR, CBDVarSwapVaR, CBDMadSwapVaR,
                                                         M6WangSwapVaR, M6PropSwapVaR, M6DualSwapVaR, M6GiniSwapVaR, M6ExponentialSwapVaR, M6StDevSwapVaR, M6VarSwapVaR, M6MadSwapVaR)
))
rownames(Swap_VaR_table) = c("Wang", "Proportional", "Dual", "Gini", "Exponential", "StDev", "Var", "Mad")
colnames(Swap_VaR_table) = c("LC","RH","CBD","M6")

Forward_ES_table = data.frame(matrix(nrow = 8, ncol = 4, c(LCWangES, LCPropES, LCDualES, LCGiniES, LCExponentialES, LCStDevES, LCVarES, LCMadES,
                                                            RHWangES, RHPropES, RHDualES, RHGiniES, RHExponentialES, RHStDevES, RHVarES, RHMadES,
                                                            CBDWangES, CBDPropES, CBDDualES, CBDGiniES, CBDExponentialES, CBDStDevES, CBDVarES, CBDMadES,
                                                            M6WangES, M6PropES, M6DualES, M6GiniES, M6ExponentialES, M6StDevES, M6VarES, M6MadES)
))
rownames(Forward_ES_table) = c("Wang", "Proportional", "Dual", "Gini", "Exponential", "StDev", "Var", "Mad")
colnames(Forward_ES_table) = c("LC","RH","CBD","M6")

Swap_ES_table = data.frame(matrix(nrow = 8, ncol = 4, c(LCWangSwapES, LCPropSwapES, LCDualSwapES, LCGiniSwapES, LCExponentialSwapES, LCStDevSwapES, LCVarSwapES, LCMadSwapES,
                                                             RHWangSwapES, RHPropSwapES, RHDualSwapES, RHGiniSwapES, RHExponentialSwapES, RHStDevSwapES, RHVarSwapES, RHMadSwapES,
                                                             CBDWangSwapES, CBDPropSwapES, CBDDualSwapES, CBDGiniSwapES, CBDExponentialSwapES, CBDStDevSwapES, CBDVarSwapES, CBDMadSwapES,
                                                             M6WangSwapES, M6PropSwapES, M6DualSwapES, M6GiniSwapES, M6ExponentialSwapES, M6StDevSwapES, M6VarSwapES, M6MadSwapES)
))
rownames(Swap_ES_table) = c("Wang", "Proportional", "Dual", "Gini", "Exponential", "StDev", "Var", "Mad")
colnames(Swap_ES_table) = c("LC","RH","CBD","M6")


round(Swap_VaR_table,4)
round(Forward_VaR_table,4)
round(Swap_ES_table,4)
round(Forward_ES_table,4)