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
RH = rh(link = "log", 
        cohortAgeFun = "1") # cohortAgeFun = "NP" sets the coefficient of the cohort term to be a variable not equal to 1
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
## To do, find a way to calibrate lambda
lambda = 0.3

notional_principal = 1000
annuitants = 10000

# Assume a known constant interest rate that is continuously compounded
interest_rate = 0.05
discount_factor = exp(- interest_rate)

LC_Wang_Forward_Price = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPrice(5, years_for, annuitants, notional_principal, lambda, "LC", "Wang") ) )
LC_Prop_Forward_Price = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPrice(5, years_for, annuitants, notional_principal, lambda, "LC", "Proportional") ) )
LC_Std_Forward_Price = as.numeric(lapply(1:years_for, function(years_for) survivorForwardPrice(5, years_for, annuitants, notional_principal, lambda, "LC", "Stdev") ) )
LC_Var_Forward_Price = as.numeric( lapply(1:years_for, function(years_for) survivorForwardPrice(5, years_for, annuitants, notional_principal, lambda, "LC", "Var") ) )

# plot(LC_Wang_risk_prem, ylim = c(0,0.10), lwd=2, type="l", xlim = c(0,years_for), main= "Implied Risk Premium generated from LC model", ylab="Percentage in % Basis", xlab = "Years to Maturity of Longevity Swap")
# lines(LC_Prop_risk_prem, lwd = 2, col="red")
# lines(LC_Std_risk_prem, lwd = 2, col= "green", lty=2)
# lines(LC_Var_risk_prem, col="blue", lty=2)
# legend("topright", legend=c("Wang Principle", "Proportional Hazard Principle","Standard Deviation Principle", "Variance Principle"),
#        col=c("black", "red", "green", "blue"), lty=c(1,1,2,2), cex=0.8)

LC_Wang_Forward_Price
LC_Prop_Forward_Price
LC_Std_Forward_Price
LC_Var_Forward_Price

LC_Wang_Swap_Price = as.numeric( lapply(1:years_for, function(years_for) longevitySwapPrice(5, years_for, annuitants, notional_principal, lambda, "LC", "Wang") ) )
LC_Prop_Swap_Price = round(as.numeric( lapply(1:years_for, function(years_for) longevitySwapPrice(5, years_for, annuitants, notional_principal, lambda, "LC", "Proportional") ) ), digits = 4)
LC_Std_Swap_Price = as.numeric(lapply(1:years_for, function(years_for) longevitySwapPrice(5, years_for, annuitants, notional_principal, lambda, "LC", "Stdev") ) )
LC_Var_Swap_Price = as.numeric( lapply(1:years_for, function(years_for) longevitySwapPrice(5, years_for, annuitants, notional_principal, lambda, "LC", "Var") ) )
# 
LC_Wang_Swap_Price
LC_Prop_Swap_Price
LC_Std_Swap_Price
LC_Var_Swap_Price