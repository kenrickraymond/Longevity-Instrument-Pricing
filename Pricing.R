# Reduce chances of scientific notation
options(scipen=999)

library(demography)
library(StMoMo)
library(lifecontingencies)
library(stats)

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
no_years_fitted = length(years.fit)

# Matrix of age weights. See StMoMo vignette page 16 (https://cran.r-project.org/web/packages/StMoMo/vignettes/StMoMoVignette.pdf)
wxt = genWeightMat(ages = ages.fit, years = years.fit)

# InitialiZe mortality models
LC = lc(link = "log")
RH = rh(link = "log") # cohortAgeFun = "NP" sets the coefficient of the cohort term to be a variable not equal to 1
CBD = cbd(link="logit") # Logit link refers to q_{x,t}/{1 - q_{x,t}}
M6 = m6(link="logit")

# Fit Mortality Models
LCfit = fit(LC, data=EWMaleData, ages.fit=ages.fit, years.fit=years.fit)
RHfit = fit(RH, data=EWMaleData, ages.fit=ages.fit, years.fit=years.fit)
CBDfit = fit(CBD, data = EWMaleIniData, ages.fit = ages.fit, years.fit=years.fit, wxt = wxt)
M6fit = fit(M6, data = EWMaleIniData, ages.fit = ages.fit, years.fit=years.fit, wxt = wxt)

### Goodness of Fit
table = data.frame(matrix(nrow = 4, ncol =2, c(AIC(LCfit),
                                               AIC(RHfit),
                                               BIC(LCfit),
                                               BIC(RHfit),
                                               AIC(CBDfit),
                                               AIC(M6fit),
                                               BIC(CBDfit),
                                               BIC(M6fit))
))
colnames(table) = c("AIC","BIC")
rownames(table) = c("LC","RH","CBD","M6")
table

# Plot fitted models
# plot(LCfit)
# plot(RHfit)
# plot(CBDfit)
# plot(M6fit)

# Generate h-year ahead forecasts
years_for = 25
LCfor = forecast(LCfit, h=years_for)
RHfor = forecast(RHfit, h=years_for)
CBDfor = forecast(CBDfit, h=years_for)
M6for = forecast(M6fit, h=years_for)
nsim = 500
################################################################################################################################
#################################################### Market Price of Risk ######################################################
################################################################################################################################
# 2012 Rates for UK Population
payment = 5700
total = 100000

K = length(ages.fit)
k=5 # Reference age, in format of "first_age + k = target_age" 

interest_rate = 0.017
discount_factor = exp(- interest_rate)

source("getLambda.R")
# Initial estimation is required because of root-finding solution
LCWanglambda = getLambda(1, "LC", "Wang")
LCProplambda = getLambda(1.5, "LC", "Proportional")
LCDuallambda = getLambda(1.3, "LC", "Dual")
LCGinilambda = getLambda(0.5, "LC", "Gini")
LCExponentiallambda = getLambda(1, "LC", "Exponential")
# No initial estimation is necessary, closed form expressions for lambda are derived for real world measures
LCStdevlambda = getLambda(0, "LC", "Stdev")
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

# source("SurvivorForward.R")
# LC_Wang_Forward_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPremium(5, years_for, notional_principal=payment, LCWanglambda, "LC", "Wang", nsim=nsim) ) )
# LC_Prop_Forward_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPremium(5, years_for, notional_principal=payment, LCProplambda, "LC", "Proportional", nsim=nsim) ) )
# LC_Std_Forward_Premium = as.numeric(lapply(1:years_for, function(years_for) survivorForwardPremium(5, years_for, notional_principal=payment, LCStdevlambda, "LC", "Stdev", nsim=nsim) ) )
# LC_Var_Forward_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPremium(5, years_for, notional_principal=payment, LCVarlambda, "LC", "Var", nsim=nsim) ) )
# 
# source("SurvivorSwap.R")
# LC_Wang_Swap_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorSwapPremium(5, years_for, notional_principal=payment, LCProplambda, "LC", "Wang", nsim=nsim) ) )
# LC_Prop_Swap_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorSwapPremium(5, years_for, notional_principal=payment, LCProplambda, "LC", "Proportional", nsim=nsim) ) )
# LC_Std_Swap_Premium = as.numeric(lapply(1:years_for, function(years_for) survivorSwapPremium(5, years_for, notional_principal=payment, LCStdevlambda, "LC", "Stdev", nsim=nsim) ) )
# LC_Var_Swap_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorSwapPremium(5, years_for, notional_principal=payment, LCVarlambda, "LC", "Var", nsim=nsim) ) )