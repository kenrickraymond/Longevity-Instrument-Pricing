# Reduce chances of scientific notation
options(scipen=999)

library(demography)
library(StMoMo)
library(lifecontingencies)
source("getLambda.R")
source("Credentials.R")

################################################################################################################################
########################################################## Preparation #########################################################
################################################################################################################################
# The Data
EWMaleData

# Converting central exposure to initial exposure for logit models
EWMaleIniData = central2initial(EWMaleData)

# Restrict the Data
ages.fit = 60:89
years.fit = seq(1961,2001,1)
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
years_for = 15
LCfor = forecast(LCfit, h=years_for)
RHfor = forecast(RHfit, h=years_for)
CBDfor = forecast(CBDfit, h=years_for)
M6for = forecast(M6fit, h=years_for)
nsim = 1000
################################################################################################################################
#################################################### Market Price of Risk ######################################################
################################################################################################################################
payment = 6845
total = 100000

K = length(ages.fit) - 1
k=5 # Reference age, in format of "first_age + k = target_age" 

interest_rate = 0.05
discount_factor = exp(- interest_rate)
discount_factor_simple = 1/(1+interest_rate)

LCWanglambda = getLambda(0.5, 5, "LC", "Wang")
LCProplambda = getLambda(0.5, 5, "LC", "Proportional")
LCStdevlambda = getLambda(0.5, 5, "LC", "Stdev")
LCVarlambda = getLambda(-0.5, 5, "LC", "Var")

RHWanglambda = getLambda(0.5, 5, "RH", "Wang")
RHProplambda = getLambda(0.5, 5, "RH", "Proportional")
RHStdevlambda = getLambda(0.5, 5, "RH", "Stdev")
RHVarlambda = getLambda(-0.5, 5, "RH", "Var")

CBDWanglambda = getLambda(0.5, 5, "CBD", "Wang")
CBDProplambda = getLambda(0.5, 5, "CBD", "Proportional")
CBDStdevlambda = getLambda(0.5, 5, "CBD", "Stdev")
CBDVarlambda = getLambda(-0.5, 5, "CBD", "Var")

M6Wanglambda = getLambda(0.5, 5, "M6", "Wang")
M6Proplambda = getLambda(0.5, 5, "M6", "Proportional")
M6Stdevlambda = getLambda(0.5, 5, "M6", "Stdev")
M6Varlambda = getLambda(-0.5, 5, "M6", "Var")

lambda_table = data.frame(matrix(nrow = 4, ncol = 4, c(LCWanglambda, LCProplambda, LCStdevlambda, LCVarlambda,
                                                       RHWanglambda, RHProplambda, RHStdevlambda, RHVarlambda,
                                                       CBDWanglambda, CBDProplambda, CBDStdevlambda, CBDVarlambda,
                                                       M6Wanglambda, M6Proplambda, M6Stdevlambda, M6Varlambda)
))
rownames(lambda_table) = c("Wang", "Proportional", "StDev", "Var")
colnames(lambda_table) = c("LC","RH","CBD","M6")
lambda_table

source("SurvivorForward.R")
LC_Wang_Forward_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPremium(5, years_for, notional_principal=payment, LCWanglambda, "LC", "Wang", nsim=nsim) ) )
LC_Prop_Forward_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPremium(5, years_for, notional_principal=payment, LCProplambda, "LC", "Proportional", nsim=nsim) ) )
LC_Std_Forward_Premium = as.numeric(lapply(1:years_for, function(years_for) survivorForwardPremium(5, years_for, notional_principal=payment, LCStdevlambda, "LC", "Stdev", nsim=nsim) ) )
LC_Var_Forward_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPremium(5, years_for, notional_principal=payment, LCVarlambda, "LC", "Var", nsim=nsim) ) )

RH_Wang_Forward_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPremium(5, years_for, notional_principal=payment, RHWanglambda, "RH", "Wang", nsim=nsim) ) )
RH_Prop_Forward_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPremium(5, years_for, notional_principal=payment, RHProplambda, "RH", "Proportional", nsim=nsim) ) )
RH_Std_Forward_Premium = as.numeric(lapply(1:years_for, function(years_for) survivorForwardPremium(5, years_for, notional_principal=payment, RHStdevlambda, "RH", "Stdev", nsim=nsim) ) )
RH_Var_Forward_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPremium(5, years_for, notional_principal=payment, RHVarlambda, "RH", "Var", nsim=nsim) ) )

CBD_Wang_Forward_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPremium(5, years_for, notional_principal=payment, CBDWanglambda, "CBD", "Wang", nsim=nsim) ) )
CBD_Prop_Forward_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPremium(5, years_for, notional_principal=payment, CBDProplambda, "CBD", "Proportional", nsim=nsim) ) )
CBD_Std_Forward_Premium = as.numeric(lapply(1:years_for, function(years_for) survivorForwardPremium(5, years_for, notional_principal=payment, CBDStdevlambda, "CBD", "Stdev", nsim=nsim) ) )
CBD_Var_Forward_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPremium(5, years_for, notional_principal=payment, CBDVarlambda, "CBD", "Var", nsim=nsim) ) )

M6_Wang_Forward_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPremium(5, years_for, notional_principal=payment, M6Wanglambda, "M6", "Wang", nsim=nsim) ) )
M6_Prop_Forward_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPremium(5, years_for, notional_principal=payment, M6Proplambda, "M6", "Proportional", nsim=nsim) ) )
M6_Std_Forward_Premium = as.numeric(lapply(1:years_for, function(years_for) survivorForwardPremium(5, years_for, notional_principal=payment, M6Stdevlambda, "M6", "Stdev", nsim=nsim) ) )
M6_Var_Forward_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPremium(5, years_for, notional_principal=payment, M6Varlambda, "M6", "Var", nsim=nsim) ) )

######################################## SWAP ###########################################
source("SurvivorSwap.R")
LC_Wang_Swap_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorSwapPremium(5, years_for, notional_principal=payment, LCWanglambda, "LC", "Wang", nsim=nsim) ) )
LC_Prop_Swap_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorSwapPremium(5, years_for, notional_principal=payment, LCProplambda, "LC", "Proportional", nsim=nsim) ) )
LC_Std_Swap_Premium = as.numeric(lapply(1:years_for, function(years_for) survivorSwapPremium(5, years_for, notional_principal=payment, LCStdevlambda, "LC", "Stdev", nsim=nsim) ) )
LC_Var_Swap_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorSwapPremium(5, years_for, notional_principal=payment, LCVarlambda, "LC", "Var", nsim=nsim) ) )

RH_Wang_Swap_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorSwapPremium(5, years_for, notional_principal=payment, RHWanglambda, "RH", "Wang", nsim=nsim) ) )
RH_Prop_Swap_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorSwapPremium(5, years_for, notional_principal=payment, RHProplambda, "RH", "Proportional", nsim=nsim) ) )
RH_Std_Swap_Premium = as.numeric(lapply(1:years_for, function(years_for) survivorSwapPremium(5, years_for, notional_principal=payment, RHStdevlambda, "RH", "Stdev", nsim=nsim) ) )
RH_Var_Swap_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorSwapPremium(5, years_for, notional_principal=payment, RHVarlambda, "RH", "Var", nsim=nsim) ) )

CBD_Wang_Swap_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorSwapPremium(5, years_for, notional_principal=payment, CBDWanglambda, "CBD", "Wang", nsim=nsim) ) )
CBD_Prop_Swap_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorSwapPremium(5, years_for, notional_principal=payment, CBDProplambda, "CBD", "Proportional", nsim=nsim) ) )
CBD_Std_Swap_Premium = as.numeric(lapply(1:years_for, function(years_for) survivorSwapPremium(5, years_for, notional_principal=payment, CBDStdevlambda, "CBD", "Stdev", nsim=nsim) ) )
CBD_Var_Swap_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorSwapPremium(5, years_for, notional_principal=payment, CBDVarlambda, "CBD", "Var", nsim=nsim) ) )

M6_Wang_Swap_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorSwapPremium(5, years_for, notional_principal=payment, M6Wanglambda, "M6", "Wang", nsim=nsim) ) )
M6_Prop_Swap_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorSwapPremium(5, years_for, notional_principal=payment, M6Proplambda, "M6", "Proportional", nsim=nsim) ) )
M6_Std_Swap_Premium = as.numeric(lapply(1:years_for, function(years_for) survivorSwapPremium(5, years_for, notional_principal=payment, M6Stdevlambda, "M6", "Stdev", nsim=nsim) ) )
M6_Var_Swap_Premium = as.numeric( lapply(1:years_for, function(years_for) survivorSwapPremium(5, years_for, notional_principal=payment, M6Varlambda, "M6", "Var", nsim=nsim) ) )
