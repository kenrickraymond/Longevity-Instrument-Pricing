# Reduce chances of scientific notation
options(scipen=999)

library(demography)
library(StMoMo)
library(lifecontingencies)

source("MortalityFunctions.R")
source("Credentials.R")

################################################################################################################################
########################################################## Preparation #########################################################
################################################################################################################################
# The Data
EWMaleData

# Converting cenrtal exposure to initial exposure for logit models
EWMaleIniData = central2initial(EWMaleData)

# Restrict the Data
ages.fit = 60:89
years.fit = EWMaleIniData$years

# Matrix of age weights. See StMoMo vignette page 16 (https://cran.r-project.org/web/packages/StMoMo/vignettes/StMoMoVignette.pdf)
wxt = genWeightMat(ages = ages.fit, years = EWMaleIniData$years)

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
)
)
colnames(table) = c("AIC","BIC")
rownames(table) = c("LC","RH","CBD","M6")
table

# Plot fitted models
plot(LCfit)
plot(RHfit)
plot(CBDfit)
plot(M6fit)

# Generate h-year ahead forecasts
years_for = 10
LCfor = forecast(LCfit, h=years_for)
RHfor = forecast(RHfit, h=years_for)
CBDfor = forecast(CBDfit, h=years_for)
M6for = forecast(M6fit, h=years_for)

################################################################################################################################
#################################################### Market Price of Risk ######################################################
################################################################################################################################
# Set the parameter values
# Add getLambda()
payment = 6845
total = 100000
notional_principal = 1000
annuitants = 10000

K = length(ages.fit) - 1
k=5 # Reference age, in format of "first_age + k = target_age" 

# Assume lambda is known for now
lambda = 0.3

# Assume a known constant interest rate that is continuously compounded
interest_rate = 0.05
discount_factor = exp(- interest_rate)

LC_Wang_Forward_Price = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPrice(5, years_for, annuitants, notional_principal, lambda, "LC", "Wang") ) )
LC_Prop_Forward_Price = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPrice(5, years_for, annuitants, notional_principal, lambda, "LC", "Proportional") ) )
LC_Std_Forward_Price = as.numeric(lapply(1:years_for, function(years_for) survivorForwardPrice(5, years_for, annuitants, notional_principal, lambda, "LC", "Stdev") ) )
LC_Var_Forward_Price = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPrice(5, years_for, annuitants, notional_principal, lambda, "LC", "Var") ) )


# LC_Wang_Forward_Price
# LC_Prop_Forward_Price
# LC_Std_Forward_Price
# LC_Var_Forward_Price

LC_Wang_Swap_Price = as.numeric( lapply(1:years_for, function(years_for) longevitySwapPrice(5, years_for, annuitants, notional_principal, lambda, "LC", "Wang") ) )
LC_Prop_Swap_Price = round(as.numeric( lapply(1:years_for, function(years_for) longevitySwapPrice(5, years_for, annuitants, notional_principal, lambda, "LC", "Proportional") ) ), digits = 4)
LC_Std_Swap_Price = as.numeric(lapply(1:years_for, function(years_for) longevitySwapPrice(5, years_for, annuitants, notional_principal, lambda, "LC", "Stdev") ) )
LC_Var_Swap_Price = as.numeric( lapply(1:years_for, function(years_for) longevitySwapPrice(5, years_for, annuitants, notional_principal, lambda, "LC", "Var") ) )

# LC_Wang_Swap_Price
# LC_Prop_Swap_Price
# LC_Std_Swap_Price
# LC_Var_Swap_Price

# plot(LC_Wang_Forward_Price, ylim = c(0,20000), lwd=2, type="l", xlim = c(0,years_for), main= "Survival forward prices generated from LC model", ylab="Price", xlab = "Years to Maturity of Survival Swap")
# lines(LC_Prop_Forward_Price, lwd = 2, col="red")
# lines(LC_Std_Forward_Price, lwd = 2, col= "green", lty=2)
# lines(LC_Var_Forward_Price, col="blue", lty=2)
# legend("topleft", legend=c("Wang Principle", "Proportional Hazard Principle","Standard Deviation Principle", "Variance Principle"),
#        col=c("black", "red", "green", "blue"), lty=c(1,1,2,2), cex=0.8)
# 
# plot(LC_Wang_Swap_Price, ylim = c(0,50000), lwd=2, type="l", xlim = c(0,years_for), main= "Longevity Swap prices generated from LC model", ylab="Price", xlab = "Years to Maturity of Longevity sWap")
# lines(LC_Prop_Swap_Price, lwd = 2, col="red")
# lines(LC_Std_Swap_Price, lwd = 2, col= "green", lty=2)
# lines(LC_Var_Swap_Price, col="blue", lty=2)
# legend("topleft", legend=c("Wang Principle", "Proportional Hazard Principle","Standard Deviation Principle", "Variance Principle"),
#        col=c("black", "red", "green", "blue"), lty=c(1,1,2,2), cex=0.8)

################################################################################################################################
#################################################### Model Uncertainty #########################################################
################################################################################################################################
# Time difference of 8.890438 mins for 300 samples
samples = 300

# Note that simulate(modfit, nsim = samples, h = years_for) uses cumsum() hence it does not work for h=1. 
# As a shortcut, append three zero values for the first forecasted year h=1
# survivorForwardPriceUncertainty() returns three lists containing the 0.025, central, and 0.975 prediction intervals for the price of an s-forward
start_time <- Sys.time()
LCWangUncertainty = lapply(2:years_for, function(years_for) survivorForwardPriceUncertainty(samples, k, years_for, annuitants, notional_principal, lambda, "LC", "Wang") )
end_time <- Sys.time()
end_time - start_time
mat = do.call(rbind, LCWangUncertainty)
model_uncertainty_prices_mat = rbind( rep(0,3), mat)

# Note that 200 simulations is insufficient and leads to negative prices.
plot(model_uncertainty_prices_mat[,2], type="l", col="black", ylab="Price", xlab="Years", xlim = c(0,years_for), ylim = c(0, 20000), main="S-forward Model Uncertainty under LC model and Wang Transform")
lines(model_uncertainty_prices_mat[,1], lty=2, col="red")
lines(model_uncertainty_prices_mat[,3], lty=2, col="green")
legend("topleft", legend=c("2.5 quantile", "Central Estimate","97.5 quantile"),
       col=c("black", "red", "green"), lty=c(2,1,2), cex=0.8)

# Equal?
# mod_boot = bootstrap(LCfit, nBoot = samples, type = "semiparametric")
# modsimBoot <- simulate(mod_boot, h = years_for)
# 
# attributes(modsimBoot)
# matrix(modsimBoot$rates, nrow=samples, ncol=years_for)


